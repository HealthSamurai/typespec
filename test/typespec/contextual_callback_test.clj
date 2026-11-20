(ns typespec.contextual-callback-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [typespec.check :as check]
            [typespec.main :as main]
            [typespec.parse :as parse]))

(def ^:private spec-source
  "(ns callbacks (:require [clojure.spec.alpha :as s] [clojure.string :as str]))
(s/def ::on-name (s/fspec :args (s/cat :name string?) :ret string?))
(s/fdef use-name :args (s/cat :f ::on-name) :ret string?)
(defn use-name [f] (f \"Ada\"))\n")

(defn- check-source
  ([source] (check-source source {}))
  ([source opts]
   (check/check-files [(parse/parse-string "callbacks.clj" (str spec-source source))] opts)))

(defn- codes [result] (mapv :code (:diagnostics result)))

(deftest spec-gives-the-inline-callback-a-parameter-type
  (doseq [source ["(use-name (fn [name] (str/upper-case name)))"
                  "(use-name #(str/upper-case %))"
                  "(let [use use-name] (use (clojure.core/fn [name] (str/upper-case name))))"]]
    (let [res (check-source source {:strict true})]
      (is (empty? (codes res)) (pr-str (:diagnostics res)))))
  (let [source "(use-name (fn [name] (str (inc name))))"
        res (check-source source {:strict true})
        col (inc (str/last-index-of source "name"))
        row (inc (count (str/split-lines spec-source)))
        d (first (:diagnostics res))]
    (is (= ["TSP2345"] (codes res)) (pr-str (:diagnostics res)))
    (is (= [row col] ((juxt :row :col) (:pos d))))
    (is (= "string?" (main/type-at res "callbacks.clj" row col)))
    (is (some #(and (= [row col] ((juxt :row :col) (:pos %)))
                    (= {:source :callback :parameter 1 :type {:t :prim :name :string}} (:context %)))
              (:types res))))
  (let [source "(use-name #(str/upper-case %))"
        res (check-source source)
        row (inc (count (str/split-lines spec-source)))]
    (is (= "[string? :-> string?]" (main/type-at res "callbacks.clj" row (inc (str/index-of source "#")))))))

(deftest checks-callback-arity-and-actual-return
  (doseq [[source expected] [["(use-name (fn [a b] (str a b)))" "TSP2554"]
                             ["(use-name #(str %2))" "TSP2554"]
                             ["(use-name #(str \"constant\"))" "TSP2554"]
                             ["(use-name (fn [name] 42))" "TSP2345"]
                             ["(use-name (fn [name] nil))" "TSP2345"]]]
    (let [res (check-source source)]
      (is (= [expected] (codes res)) (pr-str (:diagnostics res)))))
  (let [source "(use-name (fn [name] (.trim ^String name)))"]
    (is (empty? (codes (check-source source))))
    (is (= ["TSP2345"] (codes (check-source source {:strict-nil true})))))
  (is (= ["TSP2345"] (codes (check-source "(use-name (fn [name] (System/getProperty name)))")))))

(deftest explicit-signatures-contextualize-destructuring-and-check-hints
  (let [res (check-source
             "(s/def ::name string?)
              (s/def ::person (s/keys :req-un [::name]))
              (defn use-person {:t/sig '[[::person :-> string?] :-> string?]} [f] (f {:name \"Ada\"}))
              (use-person (fn [{:keys [name]}] (str/upper-case name)))
              (use-person (fn [{:keys [name]}] (str (inc name))))")]
    (is (= ["TSP2345"] (codes res)) (pr-str (:diagnostics res))))
  (let [res (check-source
             "(defn use-number {:t/sig '[[number? :-> string?] :-> string?]} [f] (f 1.5))
              (use-number (fn [^long n] (str n)))")]
    (is (= ["TSP2345"] (codes res)) (pr-str (:diagnostics res)))))

(deftest unresolved-overload-contexts-stay-unresolved
  (let [res (check-source
             "(defn run {:t/sig '(t/fn ([[string? :-> string?] :-> string?]
                                        [[int? :-> int?] :-> int?]))}
                [f] (throw (Exception.)))
              (run (fn [x] x))")]
    (is (= ["TSP210-01"] (codes res)) (pr-str (:diagnostics res)))))

(deftest contextual-bodies-and-directives-are-checked-once
  (let [res (check-source "(use-name (fn [name] (str #_(t/any \"fixture\") name)))")]
    (is (empty? (codes res)) (pr-str (:diagnostics res)))
    (is (= {:any 1 :cast 0 :ignore 0} (:trust res)))))

(deftest overloads-use-ordinary-arguments-before-callbacks
  (let [source "(defn use-kind {:t/sig '(t/fn ([[string? :-> string?] #{:name} :-> string?]
                                              [[int? :-> int?] #{:count} :-> int?]))}
                  [f k] (throw (Exception.)))\n"]
    (is (empty? (codes (check-source (str source
                                          "(use-kind #(str/upper-case %) :name)\n"
                                          "(use-kind #(inc %) :count)")))))
    (is (= ["TSP2345"] (codes (check-source (str source "(use-kind #(str (inc %)) :name)")))))))

(deftest variadic-and-keyword-arguments-have-context
  (let [source "(defn use-rest {:t/sig '[[string? & int? :-> string?] :-> string?]}
                  [f] (f \"Ada\" 1))\n"]
    (doseq [callback ["(fn [name & ns] (if-let [n (first ns)] (str (inc n)) (str/upper-case name)))"
                      "#(if-let [n (first %&)] (str (inc n)) (str/upper-case %))"
                      "(fn ([name] (str/upper-case name))
                           ([name & ns] (if-let [n (first ns)] (str (inc n)) name)))"]]
      (let [res (check-source (str source "(use-rest " callback ")") {:strict-nil true})]
        (is (empty? (codes res)) (pr-str (:diagnostics res)))))
    (is (= ["TSP2345"]
           (codes (check-source (str source "(use-rest (fn ([name] 42) ([name & xs] name)))"))))))
  (is (empty? (codes (check-source "(defn configure [& {:keys [::on-name]}] \"configured\")
                                   (configure ::on-name #(str/upper-case %))")))))

(deftest let-bound-callbacks-capture-their-definition-scope
  (doseq [source ["(let [f (fn [name] (str/upper-case name)) g f] (use-name g))"
                  "(let [title \"old\" f (fn [name] (str (str/upper-case title) name)) title 42] (use-name f))"
                  "(let [f (fn [name] (str/upper-case name))] (f \"Ada\"))"
                  "(use-name (fn self [name] (if (str/blank? name) (self \"Ada\") name)))"]]
    (let [res (check-source source)]
      (is (empty? (codes res)) (pr-str (:diagnostics res)))))
  (doseq [source ["(let [f (fn [name] (str (inc name)))] (use-name f))"
                  "(let [f (fn [x] (inc \"bad\"))] 0)"
                  "(use-name (fn self [name] (self 42)))"]]
    (let [res (check-source source)]
      (is (= ["TSP2345"] (codes res)) (pr-str (:diagnostics res)))))
  (is (= ["TSP210-01"]
         (codes (check-source "(defn- left [x] (right x)) (defn- right [x] (left x))
                              (defn use-cycle {:t/sig '[:-> string?]} [] (left 1))")))))

(deftest collection-callback-selects-the-runtime-arity
  (is (empty? (codes (check-source "(mapv (fn ([x] (inc x)) ([x y] \"other\")) [1 2])"))))
  (is (= ["TSP2554"] (codes (check-source "(mapv (fn [a b] (str a b)) [1 2])"))))
  (is (= ["TSP2345"] (codes (check-source "(mapv #(+ %1 %2) [1 2] [\"a\" \"b\"])"))))
  (is (empty? (codes (check-source "(mapv #(+ %1 %2) [1 2] [3 4])")))))

(deftest reduce-checks-cardinality-and-the-accumulator
  (doseq [source ["(defn f {:t/sig '[:-> int?]} [] (reduce (fn [a b] \"unused\") 0 []))"
                  "(defn f {:t/sig '[:-> int?]} [] (reduce (fn [a b] \"unused\") [1]))"
                  "(defn f {:t/sig '[:-> string?]} [] (reduce (fn [] \"empty\") []))"
                  "(defn f {:t/sig '[:-> string?]} [] (reduce (fn [a b] \"changed\") 0 [1 2]))"
                  "(defn f {:t/sig '[:-> (s/coll-of int? :kind vector?)]} []
                     (reduce (fn [acc x] (conj acc x)) [] [1 2]))"
                  "(defn f {:t/sig '[(s/coll-of int?) :-> (t/or string? int?)]} [xs]
                     (reduce (fn ([] \"empty\") ([a b] (+ a b))) xs))"]]
    (let [res (check-source source)]
      (is (empty? (codes res)) (pr-str {:source source :diagnostics (:diagnostics res)}))))
  (is (= ["TSP2554"] (codes (check-source "(reduce (fn [a b] 42) [])"))))
  (is (= ["TSP2345"] (codes (check-source "(reduce (fn [a b] (if (string? a) (inc a) \"changed\")) 0 [1 2])"))))
  (let [res (check-source "(reduce (fn [a b] (str #_(t/any \"fixture\") a)) 0 [1 2])")]
    (is (empty? (codes res)) (pr-str (:diagnostics res)))
    (is (= 1 (get-in res [:trust :any]))))
  (let [res (check-source "(defn- f [rf] (reduce rf 0 [1 2]))" {:strict true})]
    (is (= ["TSP210-01"] (codes res)) (pr-str (:diagnostics res)))
    (is (= #{:accumulator} (get-in res [:diagnostics 0 :reasons])))))
