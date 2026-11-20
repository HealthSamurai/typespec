;; Run with the pinned classpath from this directory. The source is checked as text.
(require '[typespec.check :as check]
         '[typespec.main :as main]
         '[typespec.parse :as parse])

(let [file (parse/parse-string
            "gcs-probe.clj"
            "(ns probe (:import com.google.cloud.storage.StorageOptions))
(.setCredentials (StorageOptions/newBuilder) creds)
(-> (StorageOptions/newBuilder) (.setCredentials creds) .build)
(-> (StorageOptions/newBuilder) (.setCredentials creds) .missingMethod)")
      result (check/check-files [file] {})
      errors (filter #(= :error (:severity %)) (:diagnostics result))]
  (assert (= "com.google.cloud.storage.StorageOptions$Builder"
             (main/type-at result "gcs-probe.clj" 2 1)))
  (assert (= "com.google.cloud.storage.StorageOptions"
             (main/type-at result "gcs-probe.clj" 3 1)))
  (assert (= [["TSP2339" 4]] (mapv (juxt :code (comp :row :pos)) errors))
          (pr-str (:diagnostics result)))
  (assert (empty? (filter find-ns '[fhir.registry fhir.registry.gcs gcs])))
  (println "PASS: pinned GCS builder, result type and missing-method control"))
(shutdown-agents)
