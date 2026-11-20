(ns typespec.http-replace-regression-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [typespec.check :as c]
            [typespec.types :as t]
            [typespec.externs-regression-test :refer [project]]))

(defn diagnostics [form & [cfg]]
  (let [out (atom nil)]
    (project (str "(ns app (:require [clojure.string :as s]))\n" form)
             "{}" (or cfg {}) #(reset! out (:diagnostics %)))
    @out))

(deftest unknown-input-selects-compatible-replace-overload
  ;; The six source sites produce seven false mismatch diagnostics: MCP's
  ;; string match and string replacement both fail the last callback overload.
  (doseq [form ["(defn f [uri] (s/replace uri #\"(^/|/$)\" \"\"))"
                "(defn f [path] (s/replace path #\"^/\" \"\"))"
                "(defn f [vm] (s/replace (:ns vm) \".\" \"_\"))"
                "(defn f [json] (s/replace json #\"\\\"(.*)\\\" :\" \"<b>$1</b>:\"))"
                "(defn f [yaml] (s/replace yaml #\"(?m)^(.*):\" \"<b>$1</b>:\"))"
                "(defn f [svg cls] (s/replace svg #\"^\\<svg\" (str \"<svg class=\" (pr-str cls))))"]]
    (let [ds (diagnostics form)]
      (is (not-any? #(= "TSP2345" (:code %)) ds) (pr-str ds))
      (is (some #(= "TSP210-01" (:code %)) ds) (pr-str ds)))))

(deftest invalid-known-pairs-remain-errors
  (doseq [form ["(defn f [x] (s/replace x #\"a\" 42))"
                "(defn f [x] (s/replace x \"a\" \\b))"
                "(defn f [x] (s/replace x \\a \"b\"))"
                "(defn f [x] (s/replace x nil \"b\"))"
                "(s/replace 42 #\"a\" \"b\")"]]
    (is (some #(= "TSP2345" (:code %)) (diagnostics form)) form)))

(deftest unknown-match-or-replacement-never-silently-pass
  (doseq [form ["(defn f [m] (s/replace \"abc\" m \"x\"))"
                "(defn f [r] (s/replace \"abc\" #\"a\" r))"
                "(defn f [x m r] (s/replace x m r))"]]
    (is (some #(= "TSP210-01" (:code %)) (diagnostics form)) form)))

(deftest runtime-probes-with-known-inputs-not-upstream-namespaces
  (is (= "a/b" (str/replace "/a/b/" #"(^/|/$)" "")))
  (is (= "a" (str/replace "/a" #"^/" "")))
  (is (= "foo_bar" (str/replace "foo.bar" "." "_")))
  (is (= "<b>a</b>:" (str/replace "\"a\" :" #"\"(.*)\" :" "<b>$1</b>:")))
  (is (= "<b>a</b>: b" (str/replace "a: b" #"(?m)^(.*):" "<b>$1</b>:")))
  (is (= "<svg class=\"x\">" (str/replace "<svg>" #"^\<svg" (str "<svg class=" (pr-str "x")))))
  (is (thrown? Exception (str/replace "a" #"a" 42)))
  ;; Clojure 1.12.4 calls .toString on the first argument, so the Namespace
  ;; found in var metadata is safe at runtime too. The checker conservatively
  ;; keeps its declared CharSequence boundary and unknown warning.
  (is (= "clojure_core" (str/replace (the-ns 'clojure.core) "." "_"))))

(deftest incomplete-candidates-ignore-order-and-rejected-return-types
  (let [node {:op :sym :val 'x :pos {:row 1 :col 1}}
        ctx {:env {} :file {:file "overloads.clj"} :opts {}}
        string (t/prim :string)
        number (t/prim :number)
        good {:params [string string] :ret string}
        other {:params [number string] :ret number}
        rejected {:params [string number] :ret number}]
    (doseq [[ars expected] [[[good rejected] string]
                            [[rejected good] string]
                            [[good other rejected] t/unknown]
                            [[rejected other good] t/unknown]]]
      (binding [c/*diags* (atom [])]
        (with-redefs [c/infer (fn [_ nd] (if (= 'x (:val nd)) t/unknown string))]
          (is (= expected (#'c/call-fn ctx node 'f {:t :fn :arities ars}
                           [node (assoc node :val 'known)]))))
        (is (= ["TSP210-01"] (mapv :code @c/*diags*)))))))
