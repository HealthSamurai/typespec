(require '[typespec.main :as checker])

(let [[root layout] *command-line-args*]
  (assert (= "true" (System/getProperty "vendor-demo.check-only")))
  (assert (nil? (find-ns 'vendor-demo.core)))
  (let [good (checker/run-check root ["src"] {})
        bad (checker/run-check root ["negative"] {})
        errors (filter #(= :error (:severity %)) (:diagnostics bad))]
    (assert (empty? (:diagnostics good)) (pr-str (:diagnostics good)))
    (assert (= 1 (count (:diagnostics bad))) (pr-str (:diagnostics bad)))
    (assert (= ["TSP2345"] (mapv :code errors)) (pr-str errors))
    (assert (= "42 is not assignable to string?" (:message (first errors)))
            (pr-str errors))
    (assert (nil? (find-ns 'vendor-demo.core)) "Checker loaded the library")
    (println layout "positive: 0 diagnostics; negative:" (count errors) "error(s)")
    (doseq [d errors] (println (checker/format-diag d)))
    (println layout "library namespace never evaluated (tripwire armed).")))
(shutdown-agents)
