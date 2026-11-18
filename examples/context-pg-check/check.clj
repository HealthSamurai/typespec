;; Static only: never require pg, system or JDBC libraries as Clojure namespaces.
(require '[clojure.java.io :as io]
         '[clojure.edn :as edn]
         '[clojure.string :as str]
         '[typespec.parse :as parse]
         '[typespec.check :as check]
         '[typespec.externs :as externs]
         '[typespec.main :as main])
(import '[java.security MessageDigest] '[java.util HexFormat])
(def pin "aa8ea0eb565e7f70f80f3248e6a83198628e1047")
(def expected-sha "dca4826d414a66c22dc10ebb2a01e1d8dccaf5cf64cdfe9b8bb952987d023db5")
(let [mode (or (first *command-line-args*) "baseline")
      _ (assert (#{"baseline" "contracts"} mode) "Use baseline or contracts")
      _ (assert (= 25 (.feature (Runtime/version))) "This harness requires Java 25; set JAVA_CMD or use run.sh")
      resource (io/resource "pg.clj")
      _ (assert resource "pg.clj not found on pinned dependency classpath")
      source (slurp resource :encoding "UTF-8")
      sha (.formatHex (HexFormat/of) (.digest (MessageDigest/getInstance "SHA-256") (.getBytes source "UTF-8")))
      _ (assert (= expected-sha sha) (str "Unexpected upstream pg.clj hash: " sha))
      parsed (parse/parse-string "upstream/src/pg.clj" source)
      config (if (= mode "contracts") (edn/read-string (slurp "typespec.edn")) {})
      result (check/check-files [parsed] (assoc config :extern-contracts (externs/load-contracts "." config)))
      ds (:diagnostics result)
      summary {:mode mode :java (System/getProperty "java.version")
               :pin pin :source-sha256 sha :source-lines (count (str/split-lines source))
               :namespace (:ns parsed) :files 1 :parsed-top-level-forms (count (:forms parsed))
               :last-form-position (:pos (last (:forms parsed)))
               :pg-loaded? (boolean (find-ns 'pg))
               :counts (frequencies (map :severity ds))
               :codes (into (sorted-map) (frequencies (map (juxt :severity :code) ds)))
               :trust (:trust result)}]
  (prn summary)
  (doseq [d ds] (println (main/format-diag d)))
  (shutdown-agents)
  (System/exit (if (some #(= :error (:severity %)) ds) 1 0)))
