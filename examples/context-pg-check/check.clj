;; Static only: never require pg, system or JDBC libraries as Clojure namespaces.
(require '[fixture :as fixture]
         '[typespec.main :as main])
(assert (= 25 (.feature (Runtime/version))) "This harness requires Java 25; set JAVA_CMD or use run.sh")
(let [{:keys [summary diagnostics]} (fixture/run-check (or (first *command-line-args*) "baseline"))]
  (prn summary)
  (doseq [d diagnostics] (println (main/format-diag d)))
  (shutdown-agents)
  (System/exit (if (some #(= :error (:severity %)) diagnostics) 1 0)))
