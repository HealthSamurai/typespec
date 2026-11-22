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
        cache-path "probe/atom-cache.clj"
        breadcrumb-path "probe/breadcrumb.clj"
        parts-path "probe/http_parts.clj"
        prefix-path "probe/route_prefix.clj"
        operation-path "probe/operation_part.clj"
        paths #{endpoint-path response-path}
        checked (check/check-files (into files (map parse/parse-file)
                                        (conj paths cache-path breadcrumb-path parts-path prefix-path operation-path)) {})
        probes (filter #(paths (:file %)) (:diagnostics checked))
        cache-probes (filter #(= cache-path (:file %)) (:diagnostics checked))
        breadcrumb-probes (filter #(= breadcrumb-path (:file %)) (:diagnostics checked))
        parts-probes (filter #(= parts-path (:file %)) (:diagnostics checked))
        prefix-probes (filter #(= prefix-path (:file %)) (:diagnostics checked))
        operation-probes (filter #(= operation-path (:file %)) (:diagnostics checked))]
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
    (assert (= [[10 :error "TSP2345"] [11 :error "TSP2345"] [12 :error "TSP2345"]]
               (mapv (juxt (comp :row :pos) :severity :code) cache-probes))
            (str "HTTP atom cache probe diagnostics changed: " (pr-str cache-probes)))
    (assert (empty? breadcrumb-probes)
            (str "Expected breadcrumb to satisfy its length contract: " (pr-str breadcrumb-probes)))
    (assert (empty? parts-probes)
            (str "Expected filtered HTTP parts to satisfy their length contracts: " (pr-str parts-probes)))
    (assert (empty? prefix-probes)
            (str "Expected the route prefix to satisfy its length contract: " (pr-str prefix-probes)))
    (assert (empty? operation-probes)
            (str "Expected nth to use only the operation positions allowed by Spec: " (pr-str operation-probes)))
    (println "ENDPOINT PASS: direct and macro calls use the original HTTP Spec")
    (println "RESPONSE PASS: a string content-type passed and an integer was rejected after assoc-in")
    (println "ATOM PASS: all three operations reject numeric writes to the original icon cache")
    (println "BREADCRUMB PASS: map-indexed and interpose preserve length information from Spec")
    (println "FILTER PASS: filter and remove preserve the upper bounds for HTTP path and name parts")
    (println "BUTLAST PASS: copying a route prefix preserves its length bounds")
    (println "NTH PASS: finite indices select only the operation parts allowed by Spec")))

(defn- check-cookie-path! [files]
  (let [typed-path "probe/cookie_by_path.clj"
        checked (check/check-files (into files (map parse/parse-file) ["probe/cookie_path.clj" typed-path]) {})
        source "src/http.clj"
        diagnostics (filter #(or (= typed-path (:file %))
                                 (and (= source (:file %)) (= 41 (get-in % [:pos :row]))))
                            (:diagnostics checked))]
    (assert (empty? diagnostics) (pr-str diagnostics))
    (let [inferred (main/type-at checked source 41 3)]
      (assert (= "(s/nilable :probe.cookie/value)" inferred) (str "Unexpected get-cookie result: " inferred)))
    (println "GET-IN PASS: cookie lookups use the Spec map-of and a Spec tuple path")))

(defn- check-yaml-extensions! [files]
  (let [path "probe/yaml-codec.clj"
        declaration (->> (slurp (io/resource "clj_yaml/core.clj"))
                         (parse/parse-string path)
                         :forms
                         (some #(when (= ['defprotocol 'YAMLCodec] (mapv :val (take 2 (:children %)))) %)))
        _ (assert declaration "Expected YAMLCodec in the pinned clj-yaml source")
        contracts (parse/parse-string path
                    "(ns clj-yaml.core (:require [clojure.spec.alpha :as s]))
                     (declare generate-string parse-string)
                     (s/fdef encode :args (s/cat :data any?) :ret any?)
                     (s/fdef decode :args (s/cat :data any? :opts any?) :ret any?)")
        checked (check/check-files (conj files (update contracts :forms conj declaration)) {:track-uses true})
        formats "src/http/formats.clj"
        implementations (for [{:keys [kind file var pos]} (:uses checked)
                              :when (and (= :implementation kind) (= formats file)
                                         (= "clj-yaml.core" (namespace var)))]
                          [var (:row pos)])
        problems (for [{:keys [file pos code severity] :as d} (:diagnostics checked)
                       :when (and (= formats file) (<= 22 (:row pos) 61)
                                  (or (= :error severity) (str/starts-with? code "TSP430-")))]
                   d)]
    (assert (= [['clj-yaml.core/decode 24] ['clj-yaml.core/decode 36] ['clj-yaml.core/decode 40]
                ['clj-yaml.core/encode 51] ['clj-yaml.core/encode 54]
                ['clj-yaml.core/encode 57] ['clj-yaml.core/encode 60]]
               implementations))
    (assert (= "java.time.LocalDate" (main/type-at checked formats 51 12)))
    (assert (empty? problems) (pr-str problems))
    (assert (nil? (find-ns 'clj-yaml.core)) "YAML Codec namespace should remain unloaded")
    (println "YAML PASS: all seven method bodies resolve against the pinned protocol declaration")))

(defn- check-mcp-dispatch! [files]
  (let [checked (check/check-files (conj files (parse/parse-file "probe/mcp.clj")) {})
        errors (filter #(= :error (:severity %)) (:diagnostics checked))]
    (assert (empty? errors) (pr-str errors))
    (assert (= ":probe.call/name" (main/type-at checked "src/http/mcp.clj" 100 61))
            "Expected the tools/call method to see the required name from its Spec branch")
    (println "MCP PASS: tools/call narrows the request from its Spec without a local guard")
    (println "MCP TOOLS PASS: map and sort-by preserve the element type and length bounds from Spec")))

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
      (assert (= "int?" (main/type-at result "src/uui.clj" 120 15))
              "Expected an integer index in the breadcrumb map-indexed callback")
      (assert (= "(s/coll-of t/unknown :kind seq?)" (main/type-at result "src/uui.clj" 126 9))
              "Expected breadcrumb interpose to retain a sequence type")
      (assert (= ["map?" "map?"]
                 (mapv (fn [[row col]] (main/type-at result "src/uui/heroicons.clj" row col))
                       [[18 14] [26 11]]))
              "Expected the cache dereference and swap! to retain the atom's map type")
      (check-probes! parsed)
      (check-cookie-path! parsed)
      (check-yaml-extensions! parsed)
      (check-mcp-dispatch! parsed)
      (println "ACCEPTANCE PASS: 8 SHA-256 verified; 8 parsed; 0 failed; 1757 lines; all namespaces unloaded; trust 0/0/0; exact reviewed diagnostics")))
  (prn summary)
  (doseq [r reads :when (get-in r [:parse :error])] (prn {:parse-failure (:file r) :error (get-in r [:parse :error])}))
  (doseq [d ds] (println (main/format-diag d)))
  (shutdown-agents)
  (System/exit (if (or (pos? (:failed-files summary)) (some #(= :error (:severity %)) ds)) 1 0)))
