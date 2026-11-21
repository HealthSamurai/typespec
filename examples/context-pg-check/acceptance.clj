;; Regression acceptance, not the diagnostic CLI: expected errors mean success.
(require '[fixture :as fixture]
         '[clojure.edn :as edn]
         '[typespec.main :as main])

(defn expect! [label expected actual]
  (when-not (= expected actual)
    (throw (ex-info (str "Acceptance mismatch: " label)
                    {:expected expected :actual actual}))))

(defn location [d]
  [(:severity d) (:code d) (:file d) (get-in d [:pos :row]) (get-in d [:pos :col])])

(try
  (when (< (.feature (Runtime/version)) 25)
    (throw (ex-info "Acceptance requires Java >=25" {:java (System/getProperty "java.version")})))
  (expect! "dependency pin" "aa8ea0eb565e7f70f80f3248e6a83198628e1047"
           (get-in (edn/read-string (slurp "deps.edn")) [:deps 'context/pg :git/sha]))
  (expect! "pg absent before checking" nil (find-ns 'pg))
  (let [{:keys [summary diagnostics]} (fixture/run-check "contracts")
        locations (mapv location diagnostics)
        expected (mapv (fn [[severity code row col]]
                         [severity code "upstream/src/pg.clj" row col])
                       [[:warning "TSP210-01" 30 44]
                        [:warning "TSP210-01" 49 3]
                        [:warning "TSP210-01" 53 50]
                        [:error "TSP2322" 57 15]
                        [:warning "TSP210-01" 57 44]
                        [:warning "TSP210-01" 101 48]
                        [:warning "TSP210-01" 115 45]
                        [:warning "TSP210-01" 117 45]
                        [:error "TSP2345" 206 41]
                        [:warning "TSP7023" 308 35]])]
    (prn summary)
    (doseq [d diagnostics] (println (main/format-diag d)))
    (expect! "upstream pin" "aa8ea0eb565e7f70f80f3248e6a83198628e1047" (:pin summary))
    (expect! "complete unchanged source SHA-256"
             "dca4826d414a66c22dc10ebb2a01e1d8dccaf5cf64cdfe9b8bb952987d023db5"
             (:source-sha256 summary))
    (expect! "whole-file coverage"
             {:source-lines 451 :parsed-top-level-forms 54 :namespace 'pg :files 1
              :last-form-position {:row 437 :col 1 :end-row 451 :end-col 28}}
             (select-keys summary [:source-lines :parsed-top-level-forms :namespace :files :last-form-position]))
    (expect! "diagnostic counts" {:error 2 :warning 8} (:counts summary))
    ;; Frequencies ignore reporting order but reject duplicate or missing diagnostics.
    (expect! "exact diagnostic codes, files and positions" (frequencies expected) (frequencies locations))
    (expect! "pg not loaded" false (:pg-loaded? summary))
    (expect! "trust counters" {:cast 0 :any 0 :ignore 0} (:trust summary))
    (println "PASS: pinned whole-file pg acceptance (2 expected errors, 8 warnings).")
    (let [tree (fixture/run-tree-check)
          coverage (:summary tree)]
      (prn coverage)
      (expect! "production source coverage" {:files 9 :forms 351} (select-keys coverage [:files :forms]))
      (expect! "all production namespaces remain unloaded" [] (:upstream-loaded coverage))
      (expect! "whole-tree trust counters" {:cast 0 :any 0 :ignore 0} (:trust coverage))
      (expect! "whole-tree diagnostic counts"
               {[:error "TSP2322"] 1 [:error "TSP2345"] 1 [:error "TSP2531"] 1
                [:warning "TSP210-01"] 76 [:warning "TSP7020"] 10
                [:warning "TSP7023"] 1 [:warning "TSP7031"] 3}
               (:codes coverage))
      (expect! "whole-tree error locations"
               (frequencies (conj (vec (filter #(= :error (first %)) expected))
                                  [:error "TSP2531" "upstream/src/pg/docker.clj" 72 55]))
               (frequencies (map location (filter #(= :error (:severity %)) (:diagnostics tree)))))
      (expect! "checked conditional step in mk-table-constraint" true
               (contains? (set (map location (:diagnostics tree)))
                          [:warning "TSP210-01" "upstream/src/dsql/pg.clj" 1196 77]))
      (println "PASS: all 9 pinned pg production sources (351 forms, 3 expected errors, 90 warnings).")))
  (shutdown-agents)
  (System/exit 0)
  (catch Throwable e
    (binding [*out* *err*]
      (println "FAIL:" (.getMessage e))
      (when (ex-data e) (prn (ex-data e))))
    (shutdown-agents)
    (System/exit 1)))
