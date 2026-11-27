(ns typespec.apply-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [typespec.check :as check]
            [typespec.main :as main]
            [typespec.parse :as parse]))

(defn- checked
  ([source] (checked source {}))
  ([source opts]
   (check/check-files [(parse/parse-string "spread.clj" (str "(ns spread (:require [clojure.spec.alpha :as s]))\n" source))]
                      opts)))

(defn- codes [r] (mapv :code (:diagnostics r)))
(defn- type-at [r row] (main/type-at r "spread.clj" row 1))

(deftest applies-known-positions-including-prefixes-and-empty-tails
  (let [r (checked "(apply subs [\"abc\" 1])
(let [pair [1 2]] (clojure.core/apply subs \"abc\" pair))
(apply + [])
(apply + nil)
(apply + '())
(apply + ())
(apply subs \"abc\" 1 nil)
(let [[_ & tail] [:skip \"abc\" 1]] (apply subs tail))
(apply (fn [x] (inc x)) [1])
(let [apply (fn [_ _] :local)] (apply subs []))")]
    (is (empty? (codes r)) (pr-str (:diagnostics r)))
    (is (= ["string?" "string?" "int?" "int?" "int?" "int?" "string?" "string?" "int?" ":local"]
           (mapv #(type-at r %) (range 2 12))))))

(deftest spreads-persistent-maps-as-typed-entries
  (let [api (parse/parse-string "entries.clj" "(ns entries (:require [clojure.spec.alpha :as s]))
(s/fdef describe :args (s/cat :a clojure.lang.IMapEntry :b clojure.lang.IMapEntry) :ret string?)
(defn describe [a b] (str (key a) (key b)))")
        client (parse/parse-string "client.clj" "(ns client (:require [entries :as api]))
(apply api/describe {:a 1 :b 2})
(apply api/describe {:a 1})
(apply + {})
(apply (fn [entry] (inc (val entry))) {:a 42})
(apply (fn [entry] (inc (val entry))) {:a \"wrong\"})")
        r (check/check-files [api client] {})]
    (is (= [[3 "TSP2554"] [6 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["string?" "int?" "int?"]
           (mapv #(main/type-at r "client.clj" % 1) [2 4 5]))))
  (is (= 43 (apply (fn [entry] (inc (val entry))) {:a 42})))
  (is (thrown? clojure.lang.ArityException (apply (fn [_ _] :ok) {:a 1}))))

(deftest rejects-final-java-spread-types-that-cannot-be-sequenced
  (let [r (checked "(apply str (StringBuilder. \"ok\"))
(apply + java.time.Instant/EPOCH)
(apply + (java.time.LocalDate/of 2025 1 1))
(apply + (int-array [1 2]))")]
    (is (= [[3 "TSP2345"] [4 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r))))
    (is (= ["string?" "int?"] (mapv #(type-at r %) [2 5]))))
  (is (= "ok" (apply str (StringBuilder. "ok"))))
  (is (thrown? IllegalArgumentException (apply + java.time.Instant/EPOCH))))

(deftest applies-keywords-with-an-optional-default
  (let [r (checked "(apply :key [{:key 7}])
(apply :key {:key nil} [42])
(apply :key [{:other 7} :default])
(apply :key [42 :default])
(apply :key [])
(apply :key [nil :default :extra])
(inc (apply :key [{:key \"wrong\"}]))")]
    (is (= ["7" "nil?" ":default" ":default"] (mapv #(type-at r %) [2 3 4 5])))
    (is (= [[6 "TSP2554"] [7 "TSP2554"] [8 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (= :default (apply :key [42 :default])))
  (is (nil? (apply :key [{:key nil} 42])))
  (is (thrown? clojure.lang.ArityException (apply :key []))))

(deftest applies-quoted-symbols-as-map-keys
  (let [r (checked "(apply 'field [{'field 7}])
(apply 'field [{'field nil} :default])
(apply 'field [{} :default])
(apply 'field [nil :default])
(apply 'field [])
(inc (apply 'field [{'field \"wrong\"}]))")]
    (is (= ["7" "nil?" ":default" ":default"] (mapv #(type-at r %) [2 3 4 5])))
    (is (= [[6 "TSP2554"] [7 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (= 7 (apply 'field [{'field 7}])))
  (is (thrown? clojure.lang.ArityException (apply 'field []))))

(deftest applies-maps-as-lookups-and-preserves-spec-values
  (let [api (parse/parse-string "config.clj" "(ns config (:require [clojure.spec.alpha :as s]))
(s/def ::title string?)
(s/fdef read :args (s/cat) :ret (s/keys :req-un [::title]))
(defn read [] {:title \"text\"})")
        client (parse/parse-string "client.clj" "(ns client (:require [config :as api] [clojure.string :as str]))
(str/upper-case (apply (api/read) [:title]))
(apply {:title \"text\"} [:title])
(apply {:title nil} [:title 42])
(apply {:title \"text\"} [:miss 42])
(inc (apply (api/read) [:title]))
(apply {:title \"text\"} [])")
        r (check/check-files [api client] {})]
    (is (= [[6 "TSP2345"] [7 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r))))
    (is (= ["\"text\"" "nil?" "42"] (mapv #(main/type-at r "client.clj" % 1) [3 4 5]))))
  (is (nil? (apply {:title nil} [:title 42])))
  (is (= 42 (apply {:title "text"} [:miss 42]))))

(deftest applies-sets-as-one-argument-lookups
  (let [r (checked "(apply #{false nil :present} [false])
(apply #{false nil :present} [nil])
(apply #{:present} [:missing])
(apply #{:present} [:present])
(s/fdef known-set :args (s/cat) :ret (s/coll-of string? :kind set?))
(defn known-set [] #{\"word\"})
(apply (known-set) [\"word\"])
(apply (known-set) [:other])
(inc (apply #{\"wrong\"} [\"wrong\"]))
(apply #{:present} [:present :default])")]
    (is (= ["false" "nil?" "nil?" ":present" "(s/nilable string?)" "nil?"]
           (mapv #(type-at r %) [2 3 4 5 8 9])))
    (is (= [[10 "TSP2345"] [11 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (false? (apply #{false nil} [false])))
  (is (thrown? clojure.lang.ArityException (apply #{:present} [:present :default]))))

(deftest applies-vectors-with-checked-positions-and-jvm-index-coercion
  (let [r (checked "(apply [\"text\" 2] [0])
(apply [\"text\" 2] [1N])
(apply [\"text\" 2] [4294967296])
(apply [\"text\" 2] [0.5])
(apply [\"text\" 2] [2])
(apply [\"text\" 2] [-1])
(apply [\"text\" 2] [0 :extra])
(apply [] [0])")]
    (is (= ["\"text\"" "2" "\"text\""] (mapv #(type-at r %) [2 3 4])))
    (is (= [[5 "TSP2345"] [6 "TSP330-02"] [7 "TSP330-02"] [8 "TSP2554"] [9 "TSP330-02"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r))))
    (is (= 20 (get-in r [:diagnostics 1 :pos :col]))))
  (is (= "text" (apply ["text"] [4294967296])))
  (is (thrown? IndexOutOfBoundsException (apply ["text"] [1])))
  (is (thrown? IllegalArgumentException (apply ["text"] [0.5]))))

(deftest cons-produces-a-typed-prefix-for-apply
  (let [r (checked "(apply subs (cons \"text\" [1]))
(apply subs (cons \"text\" (int-array [1])))
(first (cons \"text\" nil))
(second (cons \"text\" [2]))
(apply subs (cons \"text\" [\"bad\"]))
(apply subs (cons \"text\" nil))
(cons :key false)
(cons :key)")]
    (is (= ["string?" "string?" "\"text\"" "2"] (mapv #(type-at r %) [2 3 4 5])))
    (is (= [[6 "TSP2345"] [7 "TSP2554"] [8 "TSP2345"] [9 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (= "ext" (apply subs (cons "text" [1]))))
  (is (thrown? ClassCastException (apply subs (cons "text" ["bad"])))))

(deftest list-star-keeps-prefixes-and-spreads-the-final-argument
  (let [r (checked "(apply subs (list* \"text\" [1 3]))
(apply subs (list* \"text\" 1 [3]))
(apply (fn [a b c d e] (str a b c d e)) (list* :a :b :c :d [:e]))
(list* [])
(first (list* [:key]))
(apply subs (list* \"text\" [\"bad\"]))
(list* :key false)
(list*)")]
    (is (= ["string?" "string?" "string?" "nil?" ":key"] (mapv #(type-at r %) [2 3 4 5 6])))
    (is (= [[7 "TSP2345"] [8 "TSP2345"] [9 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (= "ex" (apply subs (list* "text" 1 [3]))))
  (is (nil? (list* [])))
  (is (thrown? IllegalArgumentException (list* :key false))))

(deftest lazy-seq-preserves-the-positions-its-body-will-produce
  (let [r (checked "(apply subs (lazy-seq [\"text\" 1]))
(apply + (lazy-seq nil))
(first (lazy-seq [\"word\" 2]))
(let [_ (lazy-seq (throw (Exception.)))] :ok)
(apply subs (lazy-seq [\"text\" \"wrong\"]))
(apply subs (lazy-seq [\"text\"]))
(lazy-seq 42)
(lazy-seq java.time.Instant/EPOCH)")]
    (is (= ["string?" "int?" "\"word\"" ":ok"] (mapv #(type-at r %) [2 3 4 5])))
    (is (= [[6 "TSP2345"] [7 "TSP2554"] [8 "TSP2345"] [9 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (= "ext" (apply subs (lazy-seq ["text" 1]))))
  (is (thrown? ClassCastException (apply subs (lazy-seq ["text" "wrong"])))))

(deftest reports-arity-and-element-diagnostics-at-the-source
  (let [forms ["(apply subs [\"abc\"])"
               "(apply subs [\"abc\" \"wrong\"])"
               "(let [tail [\"wrong\"]] (apply subs \"abc\" tail))"
               "(apply subs \"abc\" \"wrong\" [])"
               "(apply inc 42)"
               "(apply 42 [1])"
               "(apply inc)"]
        r (checked (str/join "\n" forms))
        ds (:diagnostics r)]
    (is (= ["TSP2554" "TSP2345" "TSP2345" "TSP2345" "TSP2345" "TSP2345" "TSP2554"] (codes r))
        (pr-str ds))
    (is (= (vec (range 2 9)) (mapv (comp :row :pos) ds)))
    (is (= (inc (str/index-of (nth forms 1) "\"wrong\"")) (get-in ds [1 :pos :col])))
    (is (= (inc (str/last-index-of (nth forms 2) "tail")) (get-in ds [2 :pos :col])))
    (is (= (inc (str/index-of (nth forms 3) "\"wrong\"")) (get-in ds [3 :pos :col])))))

(deftest reuses-generic-variadic-and-map-return-contracts
  (let [r (checked "(defn retain {:t/sig '(t/all [a] [a :-> a])} [x] x)
(apply retain [\"text\"])
(defn render {:t/sig '[string? & int? :-> string?]} [prefix & suffix] prefix)
(apply render \"label\" [1 2])
(apply render \"label\" [1 \"wrong\"])
(defn with-version [m] (assoc m :version 1))
(:id (apply with-version [{:id 42}]))")]
    (is (= ["TSP2345"] (codes r)) (pr-str (:diagnostics r)))
    (is (= ["\"text\"" "string?" "42"] (mapv #(type-at r %) [3 5 8])))))

(deftest spreads-a-tuple-declared-in-spec
  (let [r (checked "(s/fdef slice :args (s/cat :args (s/tuple string? int?)) :ret string?)
(defn slice [args] (apply subs args))
(slice [\"abc\" 1])")]
    (is (empty? (codes r)) (pr-str (:diagnostics r)))
    (is (= "string?" (main/type-at r "spread.clj" 3 20)))))

(deftest checks-every-known-tail-branch
  (let [r (checked "(defn render {:t/sig '(t/fn ([int? :-> int?] [int? int? :-> string?]))}
  ([x] x) ([x y] (str x y)))
(defn choice [flag] (apply render (if flag [1] [1 2])))
(defn invalid [flag] (apply inc (if flag [1] [])))")]
    (is (= ["TSP2554"] (codes r)) (pr-str (:diagnostics r)))
    (is (= "(t/or int? string?)" (main/type-at r "spread.clj" 4 21)))))

(deftest applies-finite-length-intervals-from-spec
  (let [api (parse/parse-string
             "render.clj"
             "(ns render (:require [clojure.spec.alpha :as s]))
(s/fdef render :args (s/alt :one (s/cat :x int?) :two (s/cat :x int? :y int?)) :ret string?)
(defn render ([x] (str x)) ([x y] (str x \":\" y)))")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [render :as r] [clojure.spec.alpha :as s]))
(s/def ::tail (s/coll-of int? :kind vector? :min-count 1 :max-count 2))
(s/fdef good :args (s/cat :xs ::tail) :ret string?)
(defn good [xs] (apply r/render xs))
(s/fdef bad :args (s/cat :xs ::tail) :ret any?)
(defn bad [xs] (apply inc xs))")
        r (check/check-files [api client] {:strict true})
        ds (:diagnostics r)]
    (is (= [["client.clj" 6 "TSP2554" "apply inc expects 1 argument(s), got 2"]]
           (mapv (juxt :file (comp :row :pos) :code :message) ds))
        (pr-str ds))
    (is (= "string?" (main/type-at r "client.clj" 4 17)))))

(deftest spreads-counted-string-literals
  (let [r (checked "(apply (fn [x y] (str x y)) \"ab\")
(apply (fn [x y] (str x y)) \"😀\")
(apply + \"ab\")
(apply + \"\")")]
    (is (= [[4 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["string?" "string?" "int?"] (mapv #(type-at r %) [2 3 5]))))
  (is (= "😀" (apply (fn [x y] (str x y)) "😀")))
  (is (thrown? ClassCastException (apply + "ab"))))

(deftest spreads-jvm-arrays-using-component-type-and-length
  (let [r (checked "(apply (fn [x y] (+ x y)) (int-array [1 2]))
(apply subs (int-array [1 2]))
(apply inc (int-array 0))
(s/fdef nullable :args (s/cat :x string?) :ret string?)
(defn nullable [x] x)
(apply nullable (make-array String 1))")]
    (is (= [[3 "TSP2345"] [4 "TSP2554"] [7 "TSP2531"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= "int?" (type-at r 2))))
  (is (= 3 (apply + (int-array [1 2]))))
  (is (thrown? ClassCastException (apply subs (int-array [1 2]))))
  (is (thrown? clojure.lang.ArityException (apply inc (int-array 0)))))

(deftest uses-array-return-hints-from-another-namespace
  (let [api (parse/parse-string "arrays.clj"
                               "(ns arrays)
(defn ^ints values [] (int-array [1 2]))
(defn ^booleans flags [] (boolean-array [true]))")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [arrays :as a] [clojure.spec.alpha :as s]))
(s/fdef total :args (s/cat) :ret int?)
(defn total [] (apply + (a/values)))
(defn bad [] (apply + (a/flags)))")
        r (check/check-files [api client] {})]
    (is (= "int?" (main/type-at r "client.clj" 3 16)))
    (is (= [[4 "TSP2345"]] (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))))

(deftest callback-inference-keeps-spread-positions-correlated
  (let [api (parse/parse-string
             "choices.clj"
             "(ns choices (:require [clojure.spec.alpha :as s]))
(s/fdef pairs :args (s/cat :flag boolean?) :ret (s/or :text (s/tuple string? string?) :number (s/tuple int? int?)))
(defn pairs [flag] (if flag [\"x\" \"y\"] [1 2]))")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [choices :as c] [clojure.spec.alpha :as s]))
(s/fdef inline :args (s/cat :flag boolean?) :ret (s/or :text string? :number int?))
(defn inline [flag]
  (apply (fn [x y] (if (string? x) (subs y 0) (inc y))) (c/pairs flag)))
(s/fdef local :args (s/cat :flag boolean?) :ret (s/or :text string? :number int?))
(defn local [flag]
  (let [render (fn [x y] (if (string? x) (subs y 0) (inc y)))]
    (apply render (c/pairs flag))))
(defn invalid [flag]
  (apply (fn [x y] (if (string? x) (subs y 0) (inc y))) (if flag [\"x\" 1] [1 2])))")
        r (check/check-files [api client] {})
        ds (:diagnostics r)]
    (is (= [["client.clj" 10 "TSP2345"]] (mapv (juxt :file (comp :row :pos) :code) ds))
        (pr-str ds))
    (is (= ["(t/or string? int?)" "(t/or string? int?)"]
           (mapv #(apply main/type-at r "client.clj" %) [[4 3] [8 5]])))))

(deftest spread-callback-trust-counts-directives-across-branches
  (let [r (checked "(defn render [flag]
  (apply (fn [x]
           (str #_(t/any \"shared\") x)
           (if (string? x) x #_(t/cast int?) x))
         (if flag [\"x\"] [1])))")]
    (is (empty? (codes r)) (pr-str (:diagnostics r)))
    (is (= {:any 1 :cast 1 :ignore 0} (:trust r)))))

(deftest reports-unknown-tail-shapes-and-supports-targeted-ignore
  (let [r (checked "(s/fdef forward :args (s/cat :xs (s/coll-of int? :kind vector?)) :ret any?)
(defn forward [xs] (apply inc xs))
(defn ignored [xs] #_(t/ignore TSP410-01 \"Caller validates length\") (apply inc xs))
(defn unchecked-variadic [x & more] x)
(defn wrapper [xs] (apply unchecked-variadic xs))")]
    (is (= ["TSP410-01"] (codes r)) (pr-str (:diagnostics r)))
    (is (= [:warning #{:apply-tail}] ((juxt :severity :reasons) (first (:diagnostics r)))))
    (is (= 1 (get-in r [:trust :ignore]))))
  (let [r (checked "(defn forward [xs] (apply inc xs))" {:strict true})]
    (is (= :error (:severity (first (filter #(= "TSP410-01" (:code %)) (:diagnostics r))))))))

(deftest checks-seqable-tails-with-a-single-variadic-contract
  (let [r (checked "(s/fdef join :args (s/cat :items (partial instance? clojure.lang.ISeq)) :ret string?)
(defn join [items] (apply str items))
(s/fdef label :args (s/cat :label string? :items (s/* int?)) :ret string?)
(defn label [label & items] label)
(s/fdef good :args (s/cat :items (s/coll-of int? :kind vector?)) :ret string?)
(defn good [items] (apply label \"good\" items))
(s/fdef bad :args (s/cat :items (s/coll-of keyword? :kind vector?)) :ret string?)
(defn bad [items] (apply label \"bad\" items))
(s/fdef missing-prefix :args (s/cat :items (s/coll-of string? :kind vector?)) :ret any?)
(defn missing-prefix [items] (apply label items))
(s/fdef choice :args (s/cat :flag boolean? :items (s/coll-of #{1} :kind vector?)) :ret string?)
(defn ones {:t/sig '[& #{1} :-> string?]} [& items] \"ok\")
(defn choice [flag items] (apply ones (if flag [1] items)))")]
    (is (= [[9 "TSP2345"] [11 "TSP410-01"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= "string?" (main/type-at r "spread.clj" 3 20)))))

(deftest homogeneous-spread-uses-minimum-length-and-overloads
  (let [api (parse/parse-string
             "total.clj"
             "(ns total (:require [clojure.spec.alpha :as s]))
(s/fdef required :args (s/cat :x int? :more (s/+ int?)) :ret int?)
(defn required [x & more] (reduce + x more))")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [total :as total] [clojure.spec.alpha :as s]))
(s/def ::numbers (s/coll-of int? :kind vector? :min-count 2))
(s/def ::strings (s/coll-of string? :kind vector? :min-count 2))
(s/fdef sum :args (s/cat :xs ::numbers) :ret int?)
(defn sum [xs] (apply total/required xs))
(s/fdef bad :args (s/cat :xs ::strings) :ret any?)
(defn bad [xs] (apply total/required xs))
(s/fdef add :args (s/cat :xs (s/coll-of int? :kind vector?)) :ret int?)
(defn add [xs] (apply + xs))
(s/fdef maybe-short :args (s/cat :xs (s/coll-of int? :kind vector? :min-count 1)) :ret any?)
(defn maybe-short [xs] (apply total/required xs))")
        r (check/check-files [api client] {})
        ds (:diagnostics r)]
    (is (= [["client.clj" 7 "TSP2345"] ["client.clj" 11 "TSP410-01"]]
           (mapv (juxt :file (comp :row :pos) :code) ds))
        (pr-str ds))
    (is (= ["int?" "int?"] (mapv #(main/type-at r "client.clj" % 16) [5 9])))))

(deftest homogeneous-spread-includes-the-empty-call-and-fixed-overloads
  (let [r (checked "(defn combine {:t/sig '(t/fn ([:-> string?] [int? :-> keyword?] [int? int? & int? :-> int?]))}
  ([] \"empty\") ([x] :one) ([x y & tail] (+ x y)))
(s/fdef forward :args (s/cat :xs (s/coll-of int? :kind vector?)) :ret any?)
(defn forward [xs] (apply combine xs))")]
    (is (empty? (codes r)) (pr-str (:diagnostics r)))
    (is (= "(t/or string? keyword? int?)" (main/type-at r "spread.clj" 5 20)))))

(deftest spread-arguments-are-inferred-once
  (let [r (checked "(apply str [#_(t/any \"fixture\") 42])")]
    (is (empty? (codes r)) (pr-str (:diagnostics r)))
    (is (= 1 (get-in r [:trust :any])))))
