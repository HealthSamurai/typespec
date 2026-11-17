(ns typespec.core-types
  "Signatures of clojure.core and clojure.string functions, written in the type language.
  The same format is used for externs of third-party libraries."
  (:require [typespec.types :as t]))

(def externs
  '{clojure.core/str        [& any? :-> string?]
    clojure.core/inc        ([int? :-> int?] [number? :-> number?])
    clojure.core/dec        ([int? :-> int?] [number? :-> number?])
    clojure.core/+          ([& int? :-> int?] [& number? :-> number?])
    clojure.core/-          ([int? & int? :-> int?] [number? & number? :-> number?])
    clojure.core/*          ([& int? :-> int?] [& number? :-> number?])
    clojure.core//          [number? & number? :-> number?]
    clojure.core/<          [number? & number? :-> boolean?]
    clojure.core/>          [number? & number? :-> boolean?]
    clojure.core/<=         [number? & number? :-> boolean?]
    clojure.core/>=         [number? & number? :-> boolean?]
    clojure.core/min        ([int? & int? :-> int?] [number? & number? :-> number?])
    clojure.core/max        ([int? & int? :-> int?] [number? & number? :-> number?])
    clojure.core/quot       ([int? int? :-> int?] [number? number? :-> number?])
    clojure.core/rem        ([int? int? :-> int?] [number? number? :-> number?])
    clojure.core/mod        ([int? int? :-> int?] [number? number? :-> number?])
    clojure.core/zero?      [number? :-> boolean?]
    clojure.core/pos?       [number? :-> boolean?]
    clojure.core/neg?       [number? :-> boolean?]
    clojure.core/even?      [number? :-> boolean?]
    clojure.core/odd?       [number? :-> boolean?]
    clojure.core/count      [any? :-> int?]
    clojure.core/subs       ([string? int? :-> string?] [string? int? int? :-> string?])
    clojure.core/name       [(t/or string? keyword? symbol?) :-> string?]
    clojure.core/namespace  [(t/or keyword? symbol?) :-> (s/nilable string?)]
    clojure.core/keyword    ([(t/or string? keyword? symbol?) :-> keyword?] [(s/nilable string?) string? :-> keyword?])
    clojure.core/symbol     ([(t/or string? keyword? symbol?) :-> symbol?] [(s/nilable string?) string? :-> symbol?])
    clojure.core/parse-long [string? :-> (s/nilable int?)]
    clojure.core/parse-double [string? :-> (s/nilable double?)]
    clojure.core/parse-uuid [string? :-> (s/nilable uuid?)]
    clojure.core/random-uuid [:-> uuid?]
    clojure.core/not        [any? :-> boolean?]
    clojure.core/nil?       [any? :-> boolean?]
    clojure.core/some?      [any? :-> boolean?]
    clojure.core/string?    [any? :-> boolean?]
    clojure.core/number?    [any? :-> boolean?]
    clojure.core/int?       [any? :-> boolean?]
    clojure.core/keyword?   [any? :-> boolean?]
    clojure.core/map?       [any? :-> boolean?]
    clojure.core/vector?    [any? :-> boolean?]
    clojure.core/empty?     [any? :-> boolean?]
    clojure.core/contains?  [any? any? :-> boolean?]
    clojure.core/=          [any? & any? :-> boolean?]
    clojure.core/not=       [any? & any? :-> boolean?]
    clojure.core/println    [& any? :-> nil?]
    clojure.core/prn        [& any? :-> nil?]
    clojure.core/pr-str     [& any? :-> string?]
    clojure.core/ex-info    ([string? map? :-> java.lang.Exception] [string? map? any? :-> java.lang.Exception])
    clojure.core/range      ([:-> (s/coll-of int?)] [number? :-> (s/coll-of int?)] [number? number? :-> (s/coll-of int?)])
    clojure.core/keys       [any? :-> (s/nilable (s/coll-of any?))]
    clojure.core/vals       [any? :-> (s/nilable (s/coll-of any?))]
    clojure.string/upper-case  [string? :-> string?]
    clojure.string/lower-case  [string? :-> string?]
    clojure.string/capitalize  [string? :-> string?]
    clojure.string/trim        [string? :-> string?]
    clojure.string/reverse     [string? :-> string?]
    clojure.string/blank?      [(s/nilable string?) :-> boolean?]
    clojure.string/join        ([any? :-> string?] [any? any? :-> string?])
    clojure.string/split       ([string? any? :-> (s/coll-of string? :kind vector?)] [string? any? int? :-> (s/coll-of string? :kind vector?)])
    clojure.string/split-lines [string? :-> (s/coll-of string? :kind vector?)]
    clojure.string/starts-with? [string? string? :-> boolean?]
    clojure.string/ends-with?  [string? string? :-> boolean?]
    clojure.string/includes?   [string? string? :-> boolean?]
    clojure.string/replace     [string? any? any? :-> string?]})

(def table
  (let [ctx {:aliases {} :imports {} :fail (fn [f] (throw (ex-info "bad core type" {:form f})))}]
    (into {} (map (fn [[k v]] [k (t/parse-sig ctx v)])) externs)))

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
