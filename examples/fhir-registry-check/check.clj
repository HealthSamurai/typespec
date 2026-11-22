;; Read upstream source as text only. Never require/eval an upstream namespace.
(require '[clojure.java.io :as io]
         '[clojure.edn :as edn]
         '[clojure.string :as str]
         '[typespec.parse :as parse]
         '[typespec.check :as check]
         '[typespec.main :as main]
         '[typespec.externs :as externs])
(import '[java.security MessageDigest] '[java.util HexFormat])
(assert (>= (.feature (Runtime/version)) 25) "Use JDK >=25 (Java catalogue baseline)")

(defn- check-registry-probes! [files opts]
  (let [version-path "probe/version_keys.clj"
        lookup-path "probe/latest_version.clj"
        resource-path "probe/json_resource_type.clj"
        write-path "probe/json_resource_write.clj"
        prerelease-path "probe/prerelease_parts.clj"
        paths #{version-path lookup-path resource-path write-path prerelease-path
                "probe/package_names.clj" "probe/latest_nonempty.clj"
                "probe/nonempty_packages.clj" "probe/parallel_packages.clj"}
        checked (check/check-files (into files (map parse/parse-file) paths) (assoc opts :strict-nil true))
        diagnostics (sort-by :file (filter #(paths (:file %)) (:diagnostics checked)))]
    (assert (= [[resource-path 11 :error "TSP2322"]
                [write-path 12 :error "TSP2322"]
                [lookup-path 16 :error "TSP2322"]
                [prerelease-path 10 :error "TSP2322"]
                [version-path 13 :error "TSP2322"]]
               (mapv (juxt :file (comp :row :pos) :severity :code) diagnostics))
            (str "Expected only the incorrect numeric return contracts to fail: " (pr-str diagnostics)))))

(let [mode (or (first *command-line-args*) "baseline")
      _ (assert (#{"baseline" "contracts"} mode) "Use baseline or contracts")
      output (if (= mode "contracts") "current-contracts" "current")
      pin "ea79b6a18d9ae759593c3824f502bc1200209e22"
      manifest (into (sorted-map) (edn/read-string (slurp "manifest.edn")))
      checkout (io/file "../../.hyper/tmp/ceya/upstream")
      root (io/file checkout "src")
      actual (set (for [f (file-seq root)
                        :when (and (.isFile f) (re-find #"\.(clj|cljc|cljs)$" (.getName f)))]
                    (str "src/" (.relativize (.toPath root) (.toPath f)))))
      _ (assert (= actual (set (keys manifest))) "Manifest must cover every upstream production source file")
      reads (mapv (fn [[path expected]]
                    (let [file (io/file root (subs path 4))
                          bytes (java.nio.file.Files/readAllBytes (.toPath file))
                          sha (.formatHex (HexFormat/of) (.digest (MessageDigest/getInstance "SHA-256") bytes))
                          source (String. bytes "UTF-8")]
                      (assert (= sha expected) (str "SHA-256 mismatch: " path))
                      {:file path :sha256 sha :lines (count (str/split-lines source))
                       :parse (try {:ok (parse/parse-string path source)}
                                   (catch Exception e {:error {:class (.getName (class e))
                                                               :message (.getMessage e)}}))})) manifest)
      parsed (into [] (keep #(get-in % [:parse :ok])) reads)
      opts (if (= mode "contracts")
             {:extern-contracts (externs/load-contracts "." {:externs ["externs.edn"]})}
             {})
      result (check/check-files parsed opts)
      ds (:diagnostics result)
      namespaces (keep :ns parsed)
      loaded (vec (filter find-ns namespaces))
      summary {:mode mode :pin pin :clojure (clojure-version) :java (System/getProperty "java.version")
               :scope "All 11 upstream src/**/*.clj; dependency sources not checked"
               :classpath-mode (or (System/getenv "FHIR_CLASSPATH_MODE") "full")
               :source-files (count reads) :parsed-files (count parsed)
               :failed-files (count (filter #(get-in % [:parse :error]) reads))
               :source-lines (reduce + (map :lines reads))
               :upstream-loaded loaded :trust (:trust result)
               :counts (frequencies (map :severity ds))
               :codes (into (sorted-map) (frequencies (map (juxt :severity :code) ds)))
               :files (mapv (fn [r]
                              (let [p (get-in r [:parse :ok])
                                    fds (filter #(= (:file r) (:file %)) ds)
                                    nodes (when p (mapcat #(tree-seq (comp seq :children) :children %) (:forms p)))]
                                (merge (dissoc r :parse)
                                       {:namespace (:ns p) :forms (count (:forms p))
                                        :last-position (:pos (last (:forms p)))
                                        :opaque-nodes (count (filter #(= :opaque (:op %)) nodes))
                                        :opaque-positions (mapv :pos (filter #(= :opaque (:op %)) nodes))
                                        :parse-error (get-in r [:parse :error])
                                        :diagnostics (frequencies (map (juxt :severity :code) fds))}))) reads)}]
  (assert (empty? loaded) "Upstream namespace unexpectedly loaded")
  (assert (= "(s/map-of t/unknown (s/coll-of t/unknown :kind vector? :min-count 1))"
             (main/type-at result "src/fhir/registry/index.clj" 274 22))
          "Expected group-by in reindex-tgz to keep each group's non-empty vector type")
  (when (= mode "contracts")
    (let [semver-file "src/fhir/registry/semver.clj"
          latest-type (main/type-at result semver-file 81 3)
          prerelease-type (main/type-at result semver-file 45 24)]
      (assert (= "(s/nilable string?)" latest-type)
              (str "Expected semver/latest to retain the version type through sort, got " latest-type))
      (assert (= "(s/nilable string?)" prerelease-type)
              (str "Expected compare-pre-release to retain the vector element type, got " prerelease-type))))
  ;; Never overwrite the historical baseline or reviewed expectation.
  (.mkdirs (io/file output))
  (spit (io/file output "summary.edn") (str (pr-str summary) "\n"))
  (spit (io/file output "diagnostics.edn") (str (pr-str (vec ds)) "\n"))
  (assert (= 11 (:source-files summary) (:parsed-files summary)) "All 11 sources must parse")
  (assert (= {:cast 0 :any 0 :ignore 0} (:trust summary)) "Trust escape introduced")
  (check-registry-probes! parsed opts)
  (prn summary)
  (doseq [r reads :when (get-in r [:parse :error])] (prn {:parse-failure (:file r) :error (get-in r [:parse :error])}))
  (doseq [d ds] (println (main/format-diag d)))
  (shutdown-agents)
  (System/exit (if (or (pos? (:failed-files summary)) (some #(= :error (:severity %)) ds)) 1 0)))
