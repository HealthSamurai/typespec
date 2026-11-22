(ns fixture
  "Shared static whole-file fixture. Loading this namespace does not run the check."
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [typespec.parse :as parse]
            [typespec.check :as check]
            [typespec.externs :as externs])
  (:import [java.nio.file Files]
           [java.security MessageDigest]
           [java.util HexFormat]))

(def pin "aa8ea0eb565e7f70f80f3248e6a83198628e1047")
(def expected-sha "dca4826d414a66c22dc10ebb2a01e1d8dccaf5cf64cdfe9b8bb952987d023db5")

(defn- sha-256 [bytes]
  (.formatHex (HexFormat/of) (.digest (MessageDigest/getInstance "SHA-256") bytes)))

(defn- pg-resource []
  (let [resource (io/resource "pg.clj")]
    (assert resource "pg.clj not found on pinned dependency classpath")
    resource))

(defn- check-parsed [files mode]
  (let [config (if (= mode "contracts") (edn/read-string (slurp "typespec.edn")) {})]
    (check/check-files files (assoc config :extern-contracts (externs/load-contracts "." config)))))

(defn run-check [mode]
  (assert (#{"baseline" "contracts"} mode) "Use baseline or contracts")
  (let [source (slurp (pg-resource) :encoding "UTF-8")
        sha (sha-256 (.getBytes source "UTF-8"))
        _ (assert (= expected-sha sha) (str "Unexpected upstream pg.clj hash: " sha))
        parsed (parse/parse-string "upstream/src/pg.clj" source)
        result (check-parsed [parsed] mode)
        ds (:diagnostics result)]
    {:diagnostics ds
     :summary {:mode mode :java (System/getProperty "java.version")
               :pin pin :source-sha256 sha :source-lines (count (str/split-lines source))
               :namespace (:ns parsed) :files 1 :parsed-top-level-forms (count (:forms parsed))
               :last-form-position (:pos (last (:forms parsed)))
               :pg-loaded? (boolean (find-ns 'pg))
               :counts (frequencies (map :severity ds))
               :codes (into (sorted-map) (frequencies (map (juxt :severity :code) ds)))
               :trust (:trust result)}}))

(defn run-tree-check [& [edit-source]]
  (let [root (.getParentFile (io/file (.toURI (pg-resource))))
        manifest (into (sorted-map) (edn/read-string (slurp "manifest.edn")))
        paths (into {}
                    (for [f (file-seq root)
                          :when (and (.isFile f) (re-find #"\.(clj|cljc|cljs)$" (.getName f)))]
                      [(str/replace (str (.relativize (.toPath root) (.toPath f))) "\\" "/") f]))
        _ (assert (str/includes? (.getCanonicalPath root) pin) "Not the pinned pg dependency")
        _ (assert (= (set (keys manifest)) (set (keys paths))) "Manifest must cover all pg production sources")
        parsed (mapv (fn [[path expected]]
                       (let [bytes (Files/readAllBytes (.toPath (get paths path)))]
                         (assert (= expected (sha-256 bytes)) (str "SHA-256 mismatch: " path))
                         (let [source (String. bytes "UTF-8")]
                           (parse/parse-string (str "upstream/src/" path)
                                               (if edit-source (edit-source path source) source)))))
                     manifest)
        result (check-parsed parsed "contracts")
        ds (:diagnostics result)]
    {:diagnostics ds
     :types (:types result)
     :summary {:files (count parsed)
               :forms (reduce + (map (comp count :forms) parsed))
               :upstream-loaded (vec (filter find-ns (map :ns parsed)))
               :codes (into (sorted-map) (frequencies (map (juxt :severity :code) ds)))
               :trust (:trust result)}}))
