(ns typespec.core-types
  "Signatures of Clojure standard library functions, written in the type language.
  The declarations are inert EDN resources in the same format as externs of third-party
  libraries; this namespace only reads them from the classpath. Nothing is evaluated."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [typespec.types :as t]))

(def declared-namespaces
  "Namespaces that have built-in declarations. A fixed list: resources are never discovered."
  '[clojure.core clojure.string clojure.spec.alpha])

(defn resource-name
  "Classpath name of the declarations of a namespace. The typespec/ prefix keeps it out of
  the way of resources of the project being checked."
  [ns-sym]
  (str "typespec/types/" (str/replace (str ns-sym) "." "/") ".edn"))

(defn- fail [resource reason message data]
  (throw (ex-info (str "typespec built-in declarations " resource ": " message)
                  (merge {:resource resource :reason reason} data))))

(defn- only-url
  "The single classpath entry with this name. A second copy would silently replace the
  built-in types and bypass :externs-core, so it is an error, not a shadowing rule."
  [^ClassLoader loader resource]
  (let [urls (vals (into (sorted-map) (map (juxt str identity)) (enumeration-seq (.getResources loader resource))))]
    (case (count urls)
      0 (fail resource :missing "not found on the classpath" {})
      1 (first urls)
      (fail resource :duplicate (str "found " (count urls) " copies on the classpath: " (str/join ", " urls))
            {:urls (mapv str urls)}))))

(defn- read-map [resource ^java.net.URL url]
  (with-open [r (java.io.PushbackReader. (java.io.InputStreamReader. (.openStream url) "UTF-8"))]
    (let [eof (Object.)
          opts {:eof eof :readers {} :default (fn [tag _] (throw (ex-info (str "unsupported EDN tag #" tag) {})))}
          read-form #(try (edn/read opts r)
                          (catch Exception e
                            (fail resource :malformed (str "not readable as EDN: " (ex-message e)) {})))
          data (read-form)]
      (when-not (map? data) (fail resource :malformed "expected one EDN map" {}))
      (when-not (identical? eof (read-form)) (fail resource :malformed "expected exactly one EDN map" {}))
      data)))

(defn load-declarations
  "Reads and validates the declarations of every namespace in `declared-namespaces` from
  `loader`. Returns {:externs {var-symbol form} :table {var-symbol type}}. A missing,
  duplicated or malformed resource, a key outside the namespace of its file or a bad
  signature throws: there is no partial table."
  [^ClassLoader loader]
  (let [ctx (fn [resource k]
              {:aliases {} :imports {}
               :fail (fn [bad] (fail resource :bad-signature (str k " has a bad type: " (pr-str bad)) {:var k :form bad}))})]
    (reduce
     (fn [acc ns-sym]
       (let [resource (resource-name ns-sym)]
         (reduce-kv
          (fn [acc k form]
            (when-not (and (qualified-symbol? k) (= (str ns-sym) (namespace k)))
              (fail resource :bad-key (str "key " (pr-str k) " is not a var of " ns-sym) {:key k}))
            (let [ty (try (t/parse-sig (ctx resource k) form)
                          (catch clojure.lang.ExceptionInfo e (throw e))
                          (catch Exception e
                            (fail resource :bad-signature (str k " has a bad signature: " (ex-message e)) {:var k :form form})))]
              (-> acc (assoc-in [:externs k] form) (assoc-in [:table k] ty))))
          acc (read-map resource (only-url loader resource)))))
     {:externs {} :table {}} declared-namespaces)))

;; Read once, from the loader that loaded this namespace: the declarations ship with the code.
(def ^:private declarations (load-declarations (clojure.lang.RT/baseLoader)))

(def externs
  "Built-in declarations as written: var symbol -> signature form."
  (:externs declarations))

(def table
  "Built-in declarations as types: var symbol -> parsed signature."
  (:table declarations))

(def ^:private metadata-namespaces
  '#{clojure.core clojure.string clojure.set clojure.walk clojure.edn
     clojure.pprint clojure.test clojure.spec.alpha})

(defn- load-stdlib-metadata [loader]
  (let [resource "typespec/stdlib-metadata.edn"
        data (read-map resource (only-url loader resource))
        entries (:namespaces data)]
    (when-not
     (and (= #{:format :provenance :namespaces} (set (keys data)))
          (= 1 (:format data))
          (= '{org.clojure/clojure "1.12.4"
               org.clojure/spec.alpha "0.5.238"
               org.clojure/core.specs.alpha "0.4.74"} (:provenance data))
          (map? entries)
          (= metadata-namespaces (set (keys entries)))
          (every? (fn [[_ vars]]
                    (and (map? vars) (seq vars)
                         (every? (fn [[s flags]]
                                   (and (simple-symbol? s) (map? flags)
                                        (= #{:macro :tagged} (set (keys flags)))
                                        (boolean? (:macro flags))
                                        (boolean? (:tagged flags)))) vars))) entries))
      (fail resource :malformed "expected versioned public-var metadata catalogue" {}))
    entries))

;; Pinned, inert metadata: independent of namespace loading and metadata mutation.
;; Provenance and the isolated regeneration procedure live in the resource.
(def ^:private stdlib-metadata
  (load-stdlib-metadata (clojure.lang.RT/baseLoader)))

(defn stdlib-tagged?
  "Does a supported standard public var have a return type hint in the pinned
  catalogue? Never resolves vars or loads analyzed dependencies. Unknown => false."
  [sym]
  (boolean
   (when (and (qualified-symbol? sym)
              (contains? '#{clojure.core clojure.string clojure.set}
                         (symbol (namespace sym))))
     (get-in stdlib-metadata [(symbol (namespace sym)) (symbol (name sym)) :tagged]))))

(defn stdlib-macro?
  "Checks pinned standard public-var macro metadata, never the running JVM.
  core.async and other non-catalogued dependencies are unknown (false)."
  [sym]
  (boolean
   (when (qualified-symbol? sym)
     (get-in stdlib-metadata [(symbol (namespace sym)) (symbol (name sym)) :macro]))))

(defn stdlib-publics
  "Public names in the pinned standard namespace catalogue, or nil if unsupported.
  No dependency loading or live namespace introspection occurs."
  [ns-sym]
  (when-let [vars (get stdlib-metadata ns-sym)]
    (set (keys vars))))
