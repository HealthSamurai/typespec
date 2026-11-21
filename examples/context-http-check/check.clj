;; Read upstream source as text only. Never require/eval an upstream namespace.
(require '[clojure.java.io :as io]
         '[clojure.edn :as edn]
         '[clojure.string :as str]
         '[typespec.parse :as parse]
         '[typespec.check :as check]
         '[typespec.main :as main])
(import '[java.security MessageDigest] '[java.util HexFormat])
(assert (>= (.feature (Runtime/version)) 25) "Use JDK >=25 (Java catalogue baseline)")

(defn- check-probes! [files]
  (let [endpoint-path "probe/endpoint.clj"
        response-path "probe/response.clj"
        paths #{endpoint-path response-path}
        checked (check/check-files (into files (map parse/parse-file) paths) {})
        probes (filter #(paths (:file %)) (:diagnostics checked))]
    (assert (= [[endpoint-path 10 :error "TSP2322"]
                [endpoint-path 11 :error "TSP2322"]
                [endpoint-path 15 :error "TSP2322"]
                [endpoint-path 16 :error "TSP2322"]
                [response-path 10 :error "TSP2345"]]
               (mapv (juxt :file (comp :row :pos) :severity :code) probes))
            (str "HTTP contract probe diagnostics changed: " (pr-str probes)))
    (assert (str/includes? (:message (nth probes 0)) "value of :method: :trace"))
    (assert (str/includes? (:message (nth probes 1)) "value of :middleware:"))
    (assert (= (mapv :message (take 2 probes)) (mapv :message (subvec (vec probes) 2 4))))
    (println "ENDPOINT PASS: direct and macro calls use the original HTTP Spec")
    (println "RESPONSE PASS: a string content-type passed and an integer was rejected after assoc-in")))

(let [pin "e158b6b244c2b8e053de2a097d1071d9419adef9"
      manifest (into (sorted-map) (edn/read-string (slurp "manifest.edn")))
      root (.getParentFile (io/file (.toURI (io/resource "http.clj")) ))
      _ (assert (str/includes? (.getCanonicalPath root) pin) "Not the pinned dependency")
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
      result (check/check-files parsed {})
      ds (:diagnostics result)
      namespaces '[http http.formats http.mcp http.openapi http.routing uui uui.heroicons uui.menu]
      loaded (vec (filter find-ns namespaces))
      summary {:pin pin :clojure (clojure-version) :java (System/getProperty "java.version")
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
  ;; Never overwrite the historical baseline or reviewed expectation.
  (.mkdirs (io/file "current"))
  (spit "current/summary.edn" (str (pr-str summary) "\n"))
  (spit "current/diagnostics.edn" (str (pr-str (vec ds)) "\n"))
  (when (= ["--accept"] (vec *command-line-args*))
    (let [expected (edn/read-string (slurp "expected-current.edn"))
          coverage-keys [:file :sha256 :lines :namespace :forms :last-position
                         :opaque-nodes :opaque-positions :parse-error]
          tuples (fn [diagnostics]
                   (vec (sort-by pr-str
                                  (map (juxt :file :pos :severity :code :message :reasons :id)
                                       diagnostics))))]
      (assert (= 8 (:source-files summary) (:parsed-files summary)) "Expected all 8 files parsed")
      (assert (zero? (:failed-files summary)) "Parse failure")
      (assert (= 1757 (:source-lines summary)) "Source line count changed")
      (assert (= {:cast 0 :any 0 :ignore 0} (:trust summary)) "Trust escape introduced")
      (assert (= (:coverage expected) (mapv #(select-keys % coverage-keys) (:files summary)))
              "Per-file coverage/forms/positions changed")
      (assert (= (:counts expected) (:counts summary)) "Diagnostic severity counts changed")
      (assert (= (:diagnostic-tuples expected) (tuples ds)) "Exact diagnostic tuples changed")
      (assert (= "[(t/or number? java.lang.Character) :-> int?]"
                 (main/type-at result "src/http/formats.clj" 305 67))
              "Expected a checked signature for (comp byte int)")
      (assert (= "java.util.concurrent.Future" (main/type-at result "src/http.clj" 265 5))
              "Expected future in notify-on-request to have its built-in type")
      (assert (= "t/unknown" (main/type-at result "src/http.clj" 267 9))
              "Expected the subscriber call inside future to be analyzed")
      (check-probes! parsed)
      (println "ACCEPTANCE PASS: 8 SHA-256 verified; 8 parsed; 0 failed; 1757 lines; all namespaces unloaded; trust 0/0/0; exact reviewed diagnostics")))
  (prn summary)
  (doseq [r reads :when (get-in r [:parse :error])] (prn {:parse-failure (:file r) :error (get-in r [:parse :error])}))
  (doseq [d ds] (println (main/format-diag d)))
  (shutdown-agents)
  (System/exit (if (or (pos? (:failed-files summary)) (some #(= :error (:severity %)) ds)) 1 0)))
