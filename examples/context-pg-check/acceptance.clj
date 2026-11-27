;; Regression acceptance, not the diagnostic CLI: expected errors mean success.
(require '[fixture :as fixture]
         '[clojure.edn :as edn]
         '[clojure.string :as str]
         '[typespec.main :as main]
         '[typespec.parse :as parse])

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
      (expect! "create-array passes a String array to JDBC"
               "[Ljava.lang.String;"
               (main/type-at tree "upstream/src/pg.clj" 137 67))
      (expect! "whole-tree trust counters" {:cast 0 :any 0 :ignore 0} (:trust coverage))
      (expect! "whole-tree diagnostic counts"
               {[:error "TSP2322"] 1 [:error "TSP2345"] 1 [:error "TSP2531"] 3 [:error "TSP330-02"] 1
                [:warning "TSP210-01"] 72 [:warning "TSP7020"] 9
                [:warning "TSP7023"] 1 [:warning "TSP7031"] 3}
               (:codes coverage))
      (expect! "whole-tree error locations"
               (frequencies (conj (vec (filter #(= :error (first %)) expected))
                                  [:error "TSP2531" "upstream/src/pg/docker.clj" 72 55]
                                  [:error "TSP2531" "upstream/src/pg/repo.clj" 100 47]
                                  [:error "TSP2531" "upstream/src/pg/repo.clj" 220 23]
                                  [:error "TSP330-02" "upstream/src/dsql/pg.clj" 116 8]))
               (frequencies (map location (filter #(= :error (:severity %)) (:diagnostics tree)))))
      (expect! "checked conditional step in mk-table-constraint" true
               (contains? (set (map location (:diagnostics tree)))
                          [:warning "TSP210-01" "upstream/src/dsql/pg.clj" 1196 77]))
      (expect! "the ObjectMapper Var hint resolves the Java receiver" false
               (contains? (set (map location (:diagnostics tree)))
                          [:warning "TSP7020" "upstream/src/dsql/pg.clj" 417 3]))
      (expect! "remove nil? proves the starts-with? receiver inside letfn" false
               (contains? (set (map location (:diagnostics tree)))
                          [:warning "TSP210-01" "upstream/src/pg.clj" 132 45]))
      (expect! "reduce-kv diagnoses unknown inputs inside the dsql reducers"
               [[1172 40] [1172 73] [1176 44] [1187 32] [1187 48]]
               (->> (:diagnostics tree)
                    (keep (fn [{:keys [file code severity pos]}]
                            (when (and (= "upstream/src/dsql/pg.clj" file)
                                       (= "TSP210-01" code) (= :warning severity)
                                       (<= 1166 (:row pos) 1189))
                              [(:row pos) (:col pos)])))
                    vec))
      (expect! "partition-all keeps the SQL groups nested and bounded"
               (repeat 3 (str "(t/or (s/coll-of (s/coll-of t/unknown :kind seq? :min-count 1 :max-count 2) "
                              ":kind seq? :min-count 1) (s/coll-of t/never :kind seq? :max-count 0))"))
               (mapv (fn [[row col]] (main/type-at tree "upstream/src/dsql/pg.clj" row col))
                     [[1093 12] [1125 12] [1153 10]]))
      (expect! "the SQL else branches narrow pair to exactly two elements"
               (repeat 3 "(s/coll-of t/unknown :kind seq? :min-count 2 :max-count 2)")
               (mapv (fn [[row col]] (main/type-at tree "upstream/src/dsql/pg.clj" row col))
                     [[1102 52] [1134 52] [1162 50]]))
      (expect! "concat preserves the minimum number of fragments in join-vec and SQL handlers"
               (mapv #(str "(s/coll-of t/unknown :kind seq? :min-count " % ")") [1 2 1 2])
               (mapv (fn [[row col]] (main/type-at tree "upstream/src/dsql/pg.clj" row col))
                     [[116 43] [1091 5] [1121 5] [1151 4]]))
      (expect! "into keeps the separator counted in join-vec"
               "(s/coll-of t/unknown :kind vector? :min-count 1)"
               (main/type-at tree "upstream/src/dsql/pg.clj" 116 32))
      (expect! "conj counts the closing fragment in pg/coalesce"
               "(s/coll-of t/unknown :kind vector? :min-count 1)"
               (main/type-at tree "upstream/src/dsql/pg.clj" 1038 7))
      (println "PASS: all 9 pinned pg production sources (351 forms, 6 expected errors, 85 warnings).")
      (let [edit (fn [path source]
                   (if (= path "pg/docker.clj")
                     (str/replace-first source "[socket (java.net.ServerSocket. 0)]"
                                        "[socket (java.net.ServerSocket. 0) probe-resource (Object.)]")
                     source))
            mutated (fixture/run-tree-check edit)
            original (set (:diagnostics tree))]
        (expect! "implicit close rejects an Object bound in free-port"
                 [[:error "TSP2339" "upstream/src/pg/docker.clj" 8 64]]
                 (mapv location (remove original (:diagnostics mutated))))
        (println "WITH-OPEN PASS: all 9 pg sources checked with the resource mutation."))
      (let [path "probe/migration_files.clj"
            fragments "probe/sql_fragment_vector.clj"
            coalesce "probe/coalesce_vector.clj"
            join "probe/sql_fragment_pop.clj"
            tail "probe/column_tail.clj"
            pair "probe/sql_pair_length.clj"
            columns "probe/select_columns.clj"
            jdbc-array "probe/jdbc_array_read.clj"
            checked (fixture/run-tree-check nil (mapv parse/parse-file [path fragments coalesce join tail pair columns jdbc-array]))
            diagnostics (:diagnostics checked)]
        (expect! "filterv keeps the migration count upper bound from Spec"
                 [] (filterv #(= path (:file %)) diagnostics))
        (expect! "into preserves the SQL fragment count inferred from concat"
                 [] (filterv #(= fragments (:file %)) diagnostics))
        (expect! "vec and conj track SQL fragment counts after drop-last"
                 [] (filterv #(= coalesce (:file %)) diagnostics))
        (expect! "pop keeps the SQL fragment bounds after removing the separator"
                 [] (filterv #(= join (:file %)) diagnostics))
        (expect! "peek uses the final position of the SQL fragment vector"
                 [] (filterv #(= tail (:file %)) diagnostics))
        (expect! "an ordered count guard proves the second SQL fragment is present"
                 [] (filterv #(= pair (:file %)) diagnostics))
        (expect! "select-keys projects the columns from the Spec tuple"
                 [] (filterv #(= columns (:file %)) diagnostics))
        (expect! "aget keeps the nullable String component and rejects a numeric return contract"
                 [[12 :error "TSP2322"]]
                 (mapv (juxt (comp :row :pos) :severity :code) (filter #(= jdbc-array (:file %)) diagnostics)))
        (println "FILTERV PASS: filtering migrations preserves the upper length bound from Spec.")
        (println "INTO PASS: copying SQL fragments into a vector preserves the bounds from Spec.")
        (println "VEC PASS: converting trimmed SQL fragments preserves their length bounds.")
        (println "CONJ PASS: appending the closing fragment restores the original length bounds.")
        (println "POP PASS: removing the final separator keeps the remaining SQL fragment bounds.")
        (println "PEEK PASS: reading the final fragment preserves its positional type.")
        (println "COUNT PASS: comparing the pair length proves the second fragment is present.")
        (println "SELECT KEYS PASS: the Spec column tuple determines the projected row.")
        (println "AGET PASS: reading a JDBC String array preserves its component type."))))
  (shutdown-agents)
  (System/exit 0)
  (catch Throwable e
    (binding [*out* *err*]
      (println "FAIL:" (.getMessage e))
      (when (ex-data e) (prn (ex-data e))))
    (shutdown-agents)
    (System/exit 1)))
