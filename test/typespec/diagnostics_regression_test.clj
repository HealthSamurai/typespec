(ns typespec.diagnostics-regression-test
  (:require [clojure.test :refer [deftest is testing]]
            [typespec.parse :as parse]
            [typespec.check :as check]))

(defn result [source & [opts]]
  (check/check-files [(parse/parse-string "src/sample.clj" (str "(ns sample)\n" source))] (or opts {})))
(defn codes [source & [opts]]
  (mapv :code (:diagnostics (result source opts))))

(deftest diagnostic-identity
  (let [files (mapv #(parse/parse-string % "(ns sample)\n(inc \"bad\")\n(inc \"bad\")")
                    ["a/same.clj" "b/same.clj"])
        ds (:diagnostics (check/check-files files {}))]
    (is (= 4 (count ds)))
    (is (every? #(re-matches #"[0-9a-f]{32}" (:id %)) ds))
    (is (= 4 (count (set (map :id ds)))))
    (is (= ds (:diagnostics (check/check-files files {})))))
  (let [base {:code "TSP2345" :file "a.clj" :message "same"
              :pos {:row 1 :col 1 :end-row 1 :end-col 3}}
        id #'check/diag-id]
    (doseq [key [:row :col :end-row :end-col]]
      (is (not= (id base) (id (update-in base [:pos key] inc)))))))

(deftest selective-and-legacy-suppression
  (is (= ["TSP2554"] (codes "#_(t/ignore TSP2345 \"reason\") (do (inc \"bad\") (inc))")))
  (is (= [] (codes "#_(t/ignore \"legacy reason\") (do (inc \"bad\") (inc))")))
  (is (= ["TSP2345"] (codes "#_(t/ignore TSP2345 \"reason\") ; comment\n(inc \"bad\") (inc \"bad\")")))
  (is (= [] (codes "#_(t/ignore TSP2345 \"types\") #_(t/ignore TSP2554 \"arity\") (do (inc \"bad\") (inc))"))))

(deftest invalid-directives-do-not-suppress
  (doseq [args ["" "\"\"" "\"  \"" "TSP2345" "TSP2345 \"\"" "TSP2345 \" \\t \""
                "TSP2345 42" "TSP99999 \"reason\"" "TSP2345 \"reason\" :extra"]]
    (testing args
      (is (= ["TSP830-01" "TSP2345"] (codes (str "#_(t/ignore " args ") (inc \"bad\")"))))))
  (is (= ["TSP2578" "TSP830-01"]
         (codes "#_(t/ignore \"outer\") (do #_(t/ignore TSP2345 \"\") (inc 1))"))))

(deftest unused-after-code-matching
  (is (= ["TSP2578"] (codes "#_(t/ignore TSP2345 \"fixed\") (inc 1)")))
  (is (= ["TSP2578" "TSP2554"] (codes "#_(t/ignore TSP2345 \"wrong code\") (inc)")))
  (is (= ["TSP2578"] (codes "#_(t/ignore TSP2345 \"first\") #_(t/ignore TSP2345 \"duplicate\") (inc \"bad\")")))
  (let [ds (:diagnostics (result "#_(t/ignore TSP2345 \"outer\") (do #_(t/ignore TSP2345 \"inner\") (inc \"bad\"))"))]
    (is (= ["TSP2578"] (mapv :code ds)))
    (is (= 3 (:col (:pos (first ds))))))
  (is (= [] (codes "#_(t/ignore TSP2345 \"reason\") (inc \"bad\")" {:levels {'TSP2345 :off}}))))

(deftest unvisited-and-unattached-directives
  (is (= ["TSP2578"] (codes "(defn f [#_(t/ignore TSP2345 \"unused parameter\") x] x)")))
  (is (= ["TSP830-01"] (codes "#_(t/ignore TSP2345 \"no target\")")))
  (is (= ["TSP830-01"] (codes "(do #_(t/ignore TSP2345 \"no target\"))"))))

(deftest suppression-does-not-change-types
  (let [plain (result "(inc \"bad\")")
        ignored (result "#_(t/ignore TSP2345 \"reason\") (inc \"bad\")")]
    (is (= (:sigs plain) (:sigs ignored)))
    (is (= (map :type (:types plain)) (map :type (:types ignored))))
    (is (= 1 (get-in ignored [:trust :ignore])))))

(deftest exact-range-boundaries
  (let [inside? #'check/inside?
        outer {:row 1 :col 3 :end-row 1 :end-col 10}]
    (is (inside? outer outer))
    (is (not (inside? {:row 1 :col 10 :end-row 1 :end-col 11} outer)))
    (is (not (inside? {:row 1 :col 9 :end-row 1 :end-col 11} outer)))
    (is (not (inside? {} outer)))))

(deftest data-is-not-directive-scope
  (doseq [source ["'[#_(t/ignore TSP2345 \"\") (inc \"bad\")]"
                  "(quote [#_(t/ignore TSP2345 \"reason\") (inc \"bad\")])"
                  "(clojure.core/quote [#_(t/ignore) 1])"
                  "(comment #_(t/ignore) (inc \"bad\"))"
                  "(clojure.core/comment #_(t/ignore) (inc \"bad\"))"
                  "#_[#_(t/ignore) (inc \"bad\")]"
                  "(defmacro m [] #_(t/ignore) '(inc \"bad\"))"]]
    (testing source
      (let [r (result source)]
        (is (empty? (:diagnostics r)))
        (is (zero? (get-in r [:trust :ignore]))))))
  (testing "directives before the boundary form still count"
    (is (= ["TSP2578"] (codes "#_(t/ignore TSP2345 \"reason\") '(inc \"bad\")"))))
  (testing "ordinary collection values remain executable"
    (is (= ["TSP830-01" "TSP2345"] (codes "[#_(t/ignore) (inc \"bad\")]")))))

(deftest incomplete-diagnostic-is-a-known-code
  (is (= ["TSP2578"] (codes "#_(t/ignore TSP210-01 \"known limitation\") (inc 1)"))))
