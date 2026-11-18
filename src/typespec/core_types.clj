(ns typespec.core-types
  "Signatures of clojure.core and clojure.string functions, written in the type language.
  The declarations are inert EDN resources in the same format as externs of third-party
  libraries; this namespace only reads them from the classpath. Nothing is evaluated."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [typespec.types :as t]))

(def declared-namespaces
  "Namespaces that have built-in declarations. A fixed list: resources are never discovered."
  '[clojure.core clojure.string])

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

(defn stdlib-tagged?
  "Does this var of Clojure itself carry a return type hint? (The compiler uses it to avoid reflection.)"
  [sym]
  (boolean
   (when (and (symbol? sym) (namespace sym) (re-matches #"clojure\.(core|string|set)" (namespace sym)))
     (when-let [v (try (requiring-resolve sym) (catch Throwable _ nil))]
       (or (:tag (meta v)) (some #(:tag (meta %)) (:arglists (meta v))))))))

(defn stdlib-macro? [sym]
  (boolean
   (when (and (symbol? sym) (namespace sym) (re-matches #"clojure\.(core|string|set|test|spec\.alpha|core\.async)" (namespace sym)))
     (when-let [v (try (requiring-resolve sym) (catch Throwable _ nil))]
       (:macro (meta v))))))

(def ^:private publics (atom {}))

(defn stdlib-publics
  "Names of public vars of a namespace that ships with Clojure itself, or nil for any other namespace."
  [ns-sym]
  (when (re-matches #"clojure\.(core|string|set|walk|edn|pprint|spec\.alpha)" (str ns-sym))
    (or (get @publics ns-sym)
        (let [names (try (require ns-sym) (set (keys (ns-publics ns-sym))) (catch Throwable _ nil))]
          (swap! publics assoc ns-sym names)
          names))))
