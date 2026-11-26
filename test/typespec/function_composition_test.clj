(ns typespec.function-composition-test
  (:require [clojure.test :refer [deftest is]]
            [typespec.check :as check]
            [typespec.main :as main]
            [typespec.parse :as parse]))

(defn- checked [source]
  (check/check-files [(parse/parse-string "composition.clj"
                                         (str "(ns sample (:require [clojure.string :as str] [clojure.spec.alpha :as s]))\n" source))]
                     {:strict-nil true}))

(defn- codes [result] (mapv :code (:diagnostics result)))

(deftest composition-transfers-inputs-and-returns
  (let [result (checked "(comp inc count)
                         ((comp inc count) \"abc\")
                         ((comp subs str/trim) \" abc \" 1)
                         (mapv inc (map (comp byte int) \"abc\"))
                         (let [next-size (comp #(inc %) count)] (next-size \"a\"))
                         ((comp) 42)
                         (let [same (comp str/trim)] (same \" a \"))")]
    ;; The rightmost function takes every supplied argument.
    (is (= ["TSP2554"] (codes result)) (pr-str (:diagnostics result)))
    (is (= "[any? :-> int?]" (main/type-at result "composition.clj" 2 1)))
    (is (= "int?" (main/type-at result "composition.clj" 3 26))))
  (let [result (checked "(defn parse-and-slice {:t/sig '[string? int? :-> string?]}
                           [s n] (subs s n))
                         ((comp str/trim parse-and-slice) \" test \" 1)
                         (let [f (comp inc count)] (mapv f [\"a\" \"bc\"]))")]
    (is (empty? (codes result)) (pr-str (:diagnostics result)))))

(deftest composition-diagnoses-links-and-keeps-overload-results
  (let [result (checked "(comp inc str)
                         (comp subs str)
                         (comp 42 count)
                         (defn either {:t/sig '[int? :-> (t/or string? int?)]} [x] (if (= x 0) \"bad\" x))
                         (comp inc either)")]
    (is (= ["TSP2345" "TSP2554" "TSP2345" "TSP2345"] (codes result)) (pr-str (:diagnostics result)))
    (is (= [2 3 4 6] (mapv #(get-in % [:pos :row]) (:diagnostics result))))
    (is (= 7 (get-in result [:diagnostics 0 :pos :col]))))
  (let [result (checked "(defn select-ret {:t/sig '(t/fn ([int? :-> string?] [number? :-> int?]))}
                           [x] (throw (Exception.)))
                         (defn numbers {:t/sig '[number? :-> number?]} [x] x)
                         (let [f (comp select-ret numbers)] (str/trim (f 1.5)))")]
    (is (= ["TSP2345"] (codes result)) (pr-str (:diagnostics result)))))

(deftest composition-keeps-only-map-shapes-that-reach-the-result
  (let [api (parse/parse-string
             "records.clj"
             "(ns records (:require [clojure.spec.alpha :as s]))
(s/def ::name string?)
(s/def ::person (s/keys :req-un [::name]))
(s/def ::seen boolean?)
(s/def ::marker (s/keys :req-un [::seen]))
(s/fdef person :args (s/cat) :ret ::person)
(defn person [] {:name \"Ada\"})
(s/fdef touch :args (s/cat :m map?) :ret map?)
(defn touch [m] (assoc m :seen true))
(s/fdef marker :args (s/cat :m map?) :ret ::marker)
(defn marker [_] {:seen true})")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [records :as r] [clojure.string :as str]))
(str/upper-case (:name ((comp r/touch r/person))))
(inc (:name ((comp r/touch r/person))))
(:seen ((comp r/marker r/touch) {:name \"Ada\"}))
(:name ((comp r/marker r/touch) {:name \"Ada\"}))")
        r (check/check-files [api client] {})]
    (is (= [[3 "TSP2345"] [5 "TSP2339"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ":records/seen" (main/type-at r "client.clj" 4 1)))))

(deftest composition-projects-known-keys-and-tuple-positions
  (let [r (checked "(s/def ::name string?)
(s/def ::age int?)
(s/fdef person :args (s/cat) :ret (s/keys :req-un [::name] :opt-un [::age]))
(defn person [] {:name \"Ada\"})
(s/fdef pair :args (s/cat) :ret (s/tuple string? int?))
(defn pair [] [\"Ada\" 42])
((comp str/upper-case :name person))
((comp inc :name person))
((comp inc :nickname person))
((comp :age person))
((comp inc clojure.core/second pair))
((comp inc first pair))
(comp first number?)
(let [first (fn [x] 42)] ((comp inc first pair)))")]
    (is (= [[9 "TSP2345"] [10 "TSP2339"] [13 "TSP2345"] [14 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= "(s/nilable ::age)" (main/type-at r "composition.clj" 11 1)))))

(deftest composition-instantiates-intermediate-generic-contracts
  (let [api (parse/parse-string
             "generic.clj"
             "(ns generic)
(defn wrap {:t/sig '(t/all [a] [a :-> (s/tuple a)])} [x] [x])
(defn bounded {:t/sig '(t/all [(a :< number?)] [a :-> a])} [x] x)
(defn with-prefix {:t/sig '(t/all [a] [string? a :-> (s/tuple a)])} [_ x] [x])")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [generic :as g] [clojure.string :as str]))
(defn value [] 42)
(mapv (comp g/wrap inc) [1 2])
((comp first (partial g/with-prefix \"prefix\") value))
(str/upper-case ((comp first g/wrap value)))
(comp inc g/bounded str)
(comp g/with-prefix value)")
        r (check/check-files [api client] {})]
    (is (= [[5 "TSP2345"] [6 "TSP2345"] [7 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["(s/coll-of (s/tuple int?) :kind vector? :min-count 2 :max-count 2)"
            "int?"]
           (mapv #(main/type-at r "client.clj" % 1) [3 4])))))

(deftest composition-uses-the-values-and-misses-of-a-literal-set
  (let [r (checked "(s/fdef format-json :args (s/cat) :ret #{:json})
(defn format-json [] :json)
(s/fdef format-xml :args (s/cat) :ret #{:xml})
(defn format-xml [] :xml)
((comp name #{:json :charset} format-json))
((comp #{:json :charset} format-xml))
((comp name #{:json :charset} format-xml))
((comp #{false} (fn [] false)))
((comp #{} format-json))")]
    (is (= [[8 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["string?" "nil?" "false" "nil?"]
           (mapv #(main/type-at r "composition.clj" % 1) [6 7 9 10])))))

(deftest composition-specializes-a-rightmost-position-at-direct-and-callback-calls
  (let [api (parse/parse-string
             "formatter.clj"
             "(ns formatter)
(defn category [] (comp #{:json :charset} first))
(defn next-first [] (comp inc first))")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [formatter :as f]))
(group-by (f/category) [[:json \"a\"] [:xml \"b\"] [:charset \"c\"]])
(let [step (f/next-first)] (step [42 \"a\"]))
(let [step (f/next-first)] (step [\"abc\" 42]))
((f/next-first) 42)
((f/next-first))
(mapv (f/next-first) [[1] [2]])
((comp inc (comp second)) [\"a\" 1])
((partial (f/next-first) [42]))
(partial (f/next-first) [\"abc\"])
(let [first (fn [] 1)] ((comp inc first)))
((f/category) [:json \"a\"])
((f/category) [:xml \"b\"])
((f/category) [])")
        r (check/check-files [api client] {})
        ds (:diagnostics r)]
    (is (= [[4 "TSP2345"] [5 "TSP2345"] [6 "TSP2554"] [10 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) ds))
        (pr-str ds))
    (is (re-find #"^\(s/map-of \(s/nilable keyword\?\)" (main/type-at r "client.clj" 2 1)))
    (is (= ["int?"
            "(s/coll-of int? :kind vector? :min-count 2 :max-count 2)"
            "int?" "int?" "int?" ":json" "nil?" "nil?"]
           (mapv #(main/type-at r "client.clj" % 1) [3 7 8 9 11 12 13 14])))))

(deftest partial-checks-prefix-and-preserves-arities
  (let [result (checked "(partial subs \"abc\")
                         ((partial subs \"abc\") 1)
                         ((clojure.core/partial subs \"abc\") 1 2)
                         ((partial subs \"abc\" 1))
                         (let [f (partial + 1 2)] (f 3 4))
                         (mapv (partial subs \"abc\") [0 1])
                         (let [f (partial subs)] (f \"abc\" 1))")]
    (is (empty? (codes result)) (pr-str (:diagnostics result)))
    (is (= "([int? :-> string?] [int? int? :-> string?])"
           (main/type-at result "composition.clj" 2 1))))
  (let [result (checked "(partial)
                         (partial subs 42)
                         (partial subs \"abc\" 0 1 2)
                         ((partial subs \"abc\") 0 1 2)
                         ((partial subs \"abc\") \"x\")")]
    (is (= ["TSP2554" "TSP2345" "TSP2554" "TSP2554" "TSP2345"] (codes result))
        (pr-str (:diagnostics result))))
  (let [result (checked "(defn consume {:t/sig '[[string? :-> string?] string? :-> string?]}
                            [f x] (f x))
                          (let [f (partial consume #(str/upper-case %))] (f \"name\"))")]
    (is (empty? (codes result)) (pr-str (:diagnostics result)))))

(deftest partial-preserves-the-shape-of-a-captured-map-across-namespaces
  (let [api (parse/parse-string
             "records.clj"
             "(ns records (:require [clojure.spec.alpha :as s]))
(s/fdef touch :args (s/cat :m map?) :ret map?)
(defn touch [m] (assoc m :seen true))")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [records :as r] [clojure.spec.alpha :as s]))
(s/fdef original :args (s/cat) :ret string?)
(defn original [] (subs (:name ((partial r/touch {:name \"Ada\"}))) 1))
(let [f (partial r/touch {:name \"Ada\"})] (:seen (f)))
(inc (:name ((partial r/touch {:name \"Ada\"}))))")
        r (check/check-files [api client] {:strict true})]
    (is (= [["client.clj" 5 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= "boolean?" (main/type-at r "client.clj" 4 1)))))

(deftest partial-retains-generic-constraints-until-the-call
  (let [api (parse/parse-string
             "select.clj"
             "(ns selection)
(defn choose {:t/sig '(t/all [a] [string? int? a :-> a])}
  [text offset x] (subs text offset) x)
(defn either {:t/sig '(t/all [a] [a a :-> a])} [x _] x)")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [selection :as s]))
(let [f (partial (partial s/choose \"title\") 1)] (mapv f [1 2]))
(let [f (partial s/either :first)] (f \"second\"))
((partial s/choose 42 1) \"text\")
((partial s/choose \"title\") \"wrong\" 1)
((partial s/choose \"title\" 1))
(partial s/choose \"title\" 1 2 3)")
        r (check/check-files [api client] {})
        ds (:diagnostics r)]
    (is (= [[4 "TSP2345"] [5 "TSP2345"] [6 "TSP2554"] [7 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) ds))
        (pr-str ds))
    (is (= ["(s/coll-of int? :kind vector? :min-count 2 :max-count 2)"
            "#{\"second\" :first}"]
           (mapv #(main/type-at r "client.clj" % 1) [2 3])))))

(deftest partial-checks-captured-generic-callbacks-once
  (let [r (checked "(defn run-with {:t/sig '(t/all [a b] [a (t/fn [a :-> b]) int? :-> b])}
  [x f _] (f x))
(let [f (partial run-with #_(t/cast string?) \"title\" #(subs % 1))]
  [(f 1) (f 2)])
(partial run-with \"title\" #(inc %))")]
    (is (= [[6 "TSP2345"]] (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= 1 (get-in r [:trust :cast])))))

(deftest opaque-or-shadowed-functions-stay-conservative
  (let [result (checked "(defn no-proof [f x] (comp inc f) (partial subs x))
                         (let [comp (fn [x] x) partial (fn [x] x)]
                           (comp 1) (partial 2))")]
    (is (= ["TSP210-01" "TSP210-01"] (codes result)) (pr-str (:diagnostics result))))
  (let [result (checked "(defn comp [x] x)
                         (comp 42)
                         (inc ((clojure.core/comp inc count) \"x\"))")]
    (is (empty? (codes result)) (pr-str (:diagnostics result)))))

(deftest partial-respects-a-fixed-arity-that-shadows-the-variadic-minimum
  (let [r (checked "(let [f (fn ([^String x] :fixed) ([^Long x & more] :rest))
      p (partial f 42)]
  (p :more))
(let [f (fn ([^String x] :fixed) ([^Long x & more] :rest))
      p (partial f 42)]
  (p))")]
    (is (= [[7 "TSP2554" "p expects 1+ argument(s), got 0"]]
           (mapv (juxt (comp :row :pos) :code :message) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ":rest" (main/type-at r "composition.clj" 4 3))))
  (let [f (fn ([^String x] (.length x)) ([^Long x & _] x))
        p (partial f 42)]
    (is (= 42 (p :more)))
    (is (thrown? ClassCastException (p)))))
