;; Runs the checker on the demo projects and compares the output with the expected files.
;; Update the expected files with: clojure -M:test --update
(ns typespec.demo-test
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [typespec.main :as main]))

(def cases
  [{:root "demo"          :expected "demo/expected.txt"}
   {:root "test/readme"   :expected "test/readme/expected.txt"}
   {:root "test/examples" :expected "test/examples/expected.txt"}
   {:root "test/features" :expected "test/features/expected.txt" :marked "test/features/src/f/core.clj"}
   {:root "demo"          :expected "demo/expected-strict-nil.txt" :opts {:strict-nil true}}])

(defn marker-problems
  "In a marked file, every line with an ERR or WARN comment must have a diagnostic, and no other line may."
  [{:keys [root marked]}]
  (let [res (main/run-check root nil {})
        got (set (map #(:row (:pos %)) (:diagnostics res)))
        want (set (keep-indexed (fn [i l] (when (re-find #";.*\b(ERR|WARN)\b" l) (inc i)))
                                (str/split-lines (slurp marked))))]
    (concat (for [l (sort (remove got want))] (str "line " l ": expected a diagnostic, got none"))
            (for [l (sort (remove want got))] (str "line " l ": unexpected diagnostic")))))

(defn actual [{:keys [root opts]}]
  (let [res (main/run-check root nil (or opts {}))]
    (str (str/join "\n" (map main/format-diag (:diagnostics res))) "\n")))

(defn -main [& args]
  (let [update? (some #{"--update"} args)
        failed (atom 0)]
    (doseq [c cases
            :let [out (actual c)
                  f (io/file (:expected c))]]
      (cond
        update? (do (spit f out) (println "updated" (:expected c)))
        (not (.exists f)) (do (swap! failed inc) (println "MISSING" (:expected c)))
        (and (:marked c) (seq (marker-problems c)))
        (do (swap! failed inc)
            (println "FAILED " (:marked c))
            (run! #(println "  " %) (marker-problems c)))
        (= out (slurp f)) (println "ok     " (:expected c) (str "(" (count (str/split-lines out)) " diagnostics)"))
        :else (let [exp (set (str/split-lines (slurp f))) act (set (str/split-lines out))]
                (swap! failed inc)
                (println "FAILED " (:expected c))
                (doseq [l (sort (remove act exp))] (println "  - missing:   " l))
                (doseq [l (sort (remove exp act))] (println "  + unexpected:" l)))))
    (when-not update?
      (let [res (main/run-check "demo" nil {})
            strict (main/run-check "demo" nil {:strict true})
            json (json/read-str (json/write-str {:diagnostics (mapv main/diag->json (:diagnostics res))}) :key-fn keyword)
            checks
            [["sig: from s/fdef" (some #{"total          [:clinic/invoice :-> number?]   ; from s/fdef"} (main/sig-lines res 'clinic.billing))]
             ["sig: from :pre and the body" (some #{"with-tax       [:clinic/invoice number? :-> number?]   ; from the body"} (main/sig-lines res 'clinic.billing))]
             ["sig: from hints" (some #{"slug               [String long :-> String]   ; from hints"} (main/sig-lines res 'clinic.export))]
             ["type-at: before narrowing" (= "(s/nilable :clinic/patient)" (main/type-at res "src/clinic/patients.clj" 57 9))]
             ["type-at: after narrowing" (= ":clinic/patient" (main/type-at res "src/clinic/patients.clj" 59 17))]
             ["json: a safe fix for the reflective call"
              (= [{:title "Add hint ^java.io.File" :risk "safe"
                   :edits [{:range {:start [39 12] :end [39 12]} :text "^java.io.File "}]}]
                 (:fixes (first (filter #(= "TSP7023" (:code %)) (:diagnostics json)))))]
             ["trust counters" (= {:cast 1 :any 0 :ignore 2} (:trust res))]
             ["strict: public functions without a signature"
              (= 11 (count (filter #(= "TSP7010" (:code %)) (:diagnostics strict))))]]]
        (doseq [[label ok?] checks]
          (if ok? (println "ok     " label) (do (swap! failed inc) (println "FAILED " label))))))
    (shutdown-agents)
    (System/exit (if (pos? @failed) 1 0))))
