(ns typespec.collection-length-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is]]
            [typespec.check :as check]
            [typespec.length :as length]
            [typespec.main :as main]
            [typespec.parse :as parse]
            [typespec.types :as t]))

(defn- checked
  ([source] (checked source {:strict-nil true}))
  ([source opts]
   (check/check-files
    [(parse/parse-string "length.clj"
                         (str "(ns example (:require [clojure.spec.alpha :as s] [clojure.string :as str]))\n" source))]
    opts)))

(defn- codes [r] (mapv :code (:diagnostics r)))
(defn- at [r row] (main/type-at r "length.clj" row 1))

(deftest repeat-preserves-elements-and-uses-the-jvm-long-count
  (let [r (checked "(first (repeat \"word\"))
(repeat 1.9 \"word\")
(repeat 0.5 \"word\")
(first (repeat -1 :unused))
(inc (first (repeat 1 \"wrong\")))
(repeat :bad :value)
(repeat 9223372036854775808N :value)
(repeat)")]
    (is (= ["string?" "(s/coll-of string? :kind seq? :min-count 1 :max-count 1)"
            "(s/coll-of t/never :kind seq? :max-count 0)" "nil?"]
           (mapv #(at r %) (range 2 6))))
    (is (= [[6 "TSP2345"] [7 "TSP2345"] [8 "TSP2345"] [9 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (= [:value] (vec (repeat 1.9 :value))))
  (is (empty? (repeat 0.5 :value)))
  (is (thrown? IllegalArgumentException (repeat 9223372036854775808N :value))))

(deftest repeatedly-checks-the-zero-arity-callback-when-it-can-run
  (let [r (checked "(first (repeatedly (fn [] \"word\")))
(repeatedly 1.9 (fn [] \"word\"))
(first (repeatedly 0 42))
(first (repeatedly 0.5 (fn [] \"word\")))
(repeatedly 2 inc)
(repeatedly 2 42)
(repeatedly :bad (fn [] :value))
(repeatedly)
(repeatedly 1 :key)")]
    (is (= ["string?" "(s/coll-of string? :kind seq? :min-count 2 :max-count 2)" "nil?" "string?"]
           (mapv #(at r %) (range 2 6))))
    (is (= [[6 "TSP2554"] [7 "TSP2345"] [8 "TSP2345"] [9 "TSP2554"] [10 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (= [:value :value] (vec (repeatedly 1.9 (constantly :value)))))
  (is (empty? (repeatedly 0 42)))
  (is (thrown? clojure.lang.ArityException (first (repeatedly 2 inc)))))

(deftest iterate-checks-the-callback-for-repeated-application
  (let [r (checked "(first (iterate inc 0))
(first (iterate str/upper-case \"word\"))
(iterate (fn [x] (if (string? x) x (str x))) 0)
(iterate inc \"wrong\")
(iterate (fn [] 1) 0)
(iterate 42 :value)
(iterate inc)
(iterate parse-long \"1\")")]
    (is (= ["int?" "string?" "(s/coll-of (t/or int? string?) :kind seq? :min-count 1)"]
           (mapv #(at r %) (range 2 5))))
    (is (= [[5 "TSP2345"] [6 "TSP2554"] [7 "TSP2345"] [8 "TSP2554"] [9 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (= [0 "0" "0"] (vec (take 3 (iterate #(if (string? %) % (str %)) 0)))))
  (is (thrown? ClassCastException (second (iterate inc "wrong")))))

(deftest reductions-checks-the-selected-reducer-arities-and-early-stops
  (let [r (checked "(reductions + 0 [1 2])
(reductions + [])
(first (reductions inc [1]))
(first (reductions 42 (reduced \"done\") :unused))
(reductions (fn [_ x] (reduced x)) 0 [1 2 3])
(reductions inc [])
(reductions inc 0 [1])
(reductions 42 0 [1])
(reductions + 0 42)
(reductions +)")]
    (is (= ["(s/coll-of int? :kind seq? :min-count 3 :max-count 3)"
            "(s/coll-of int? :kind seq? :min-count 1 :max-count 1)"
            "int?" "string?" "(s/coll-of int? :kind seq? :min-count 1 :max-count 2)"]
           (mapv #(at r %) (range 2 7))))
    (is (= [[7 "TSP2554"] [8 "TSP2554"] [9 "TSP2345"] [10 "TSP2345"] [11 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (= [0 1 3] (vec (reductions + 0 [1 2]))))
  (is (= ["done"] (vec (reductions 42 (reduced "done") :unused))))
  (is (= [0 1] (vec (reductions (fn [_ x] (reduced x)) 0 [1 2 3]))))
  (is (thrown? clojure.lang.ArityException (first (reductions inc [])))))

(deftest bounded-count-uses-counted-collections-and-clips-other-sources
  (let [r (checked "(bounded-count :unused [:a :b])
(bounded-count 0 {:a 1})
(bounded-count :unused nil)
(bounded-count 1 \"ab\")
(bounded-count 1.5 (int-array [1 2 3]))
(bounded-count 0 (list 1 2))
(nth [:only] (bounded-count 0 [:a]))
(bounded-count :bad \"ab\")
(bounded-count 0 true)
(bounded-count 1)")]
    (is (= ["2" "1" "0" "1" "2" "2"] (mapv #(at r %) (range 2 8))))
    (is (= [[8 "TSP330-02"] [9 "TSP2345"] [10 "TSP2345"] [11 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (= 2 (bounded-count :unused [:a :b])))
  (is (= 2 (bounded-count 1.5 (int-array [1 2 3]))))
  (is (thrown? ClassCastException (bounded-count :bad "ab"))))

(deftest not-empty-preserves-the-original-collection-and-refines-the-input
  (let [r (checked "(not-empty [])
(first (not-empty [\"word\" 2]))
(not-empty \"text\")
(not-empty (int-array 0))
(s/fdef safe :args (s/cat :xs (s/coll-of int? :kind vector?)) :ret (s/nilable int?))
(defn safe [xs] (when (not-empty xs) (first xs)))
(inc (first (not-empty [\"wrong\"])))
(not-empty true)
(not-empty)")]
    (is (= ["nil?" "\"word\"" "\"text\"" "nil?"]
           (mapv #(at r %) (range 2 6))))
    (is (= [[8 "TSP2345"] [9 "TSP2345"] [10 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (let [values (int-array [1])]
    (is (identical? values (not-empty values))))
  (is (nil? (not-empty [])))
  (is (thrown? IllegalArgumentException (not-empty true))))

(deftest frequencies-types-known-counts-and-generic-map-keys
  (let [r (checked "(get (frequencies [:a :a :b]) :a)
(get (frequencies (list \"a\" \"a\")) \"a\")
(get (frequencies [:a :a :b]) :missing :none)
(get (frequencies nil) :missing 0)
(s/fdef tally :args (s/cat :xs (s/coll-of string?)) :ret (s/map-of string? int?))
(defn tally [xs] (frequencies xs))
(subs (get (frequencies [:a]) :a) 0)
(frequencies 42)
(frequencies [:a] :extra)")]
    (is (= ["2" "2" ":none" "0"] (mapv #(at r %) (range 2 6))))
    (is (= [[8 "TSP2345"] [9 "TSP2345"] [10 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (= {:a 2 :b 1} (frequencies [:a :a :b])))
  (is (thrown? ClassCastException (subs (:a (frequencies [:a])) 0))))

(deftest zipmap-pairs-positions-and-uses-the-last-value-for-duplicates
  (let [api (parse/parse-string "values.clj" "(ns values (:require [clojure.spec.alpha :as s]))
(s/fdef read-values :args (s/cat) :ret (s/tuple string? int?))
(defn read-values [] [\"word\" 2])")
        client (parse/parse-string "client.clj" "(ns client (:require [values :as api] [clojure.string :as str]))
(str/upper-case (:word (zipmap [:word :n] (api/read-values))))
(inc (:n (zipmap [:word :n] (api/read-values))))
(inc (:word (zipmap [:word :n] (api/read-values))))
(:word (zipmap [:word :word] [\"first\" 2]))
(:missing (zipmap [:word :missing] [\"first\"]) :fallback)
(:missing (zipmap nil [1]) :fallback)
(zipmap :bad [1])
(zipmap [:key])")
        r (check/check-files [api client] {})]
    (is (= [[4 "TSP2345"] [8 "TSP2345"] [9 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r))))
    (is (= ["2" ":fallback" ":fallback"] (mapv #(main/type-at r "client.clj" % 1) [5 6 7]))))
  (is (= {:word 2} (zipmap [:word :word] ["first" 2])))
  (is (thrown? IllegalArgumentException (zipmap :bad [1]))))

(deftest random-sample-preserves-bounds-for-deterministic-probabilities
  (let [r (checked "(second (random-sample 1 [0 \"word\"]))
(first (random-sample 0 [0 \"word\"]))
(first (random-sample :unused []))
(random-sample 0.5 [1 2])
(into [] (random-sample -1) [1 2])
(into [] (random-sample 2) [1 2])
(inc (first (random-sample 1 [\"word\"])))
(random-sample :bad [1])
(into [] (random-sample :bad) [1])
(random-sample 1 42)
(random-sample)")]
    (is (= ["\"word\"" "nil?" "nil?" "(s/coll-of int? :kind seq? :max-count 2)"
            "(s/coll-of t/never :kind vector?)" "(s/coll-of int? :kind vector? :max-count 2)"]
           (mapv #(at r %) (range 2 8))))
    (is (= [[8 "TSP2345"] [9 "TSP2345"] [10 "TSP2345"] [11 "TSP2345"] [12 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (= [1 2] (vec (random-sample 2 [1 2]))))
  (is (empty? (into [] (random-sample -1) [1 2])))
  (is (thrown? ClassCastException (first (random-sample :bad [1])))))

(deftest distinct-predicate-checks-arity-and-known-repeats
  (let [r (checked "(distinct? nil)
(distinct? :x :y \"word\")
(distinct? :x :y :x)
(distinct? 1 1.0)
(let [x (fn [y] (distinct? :x y :x))] (x 1))
(if (distinct? :x :x) (inc \"unreachable\") :ok)
(if (distinct? :x :y) (inc \"bad\") :ok)
(distinct?)")]
    (is (= ["true" "true" "false" "true" "false" ":ok"] (mapv #(at r %) (range 2 8))))
    (is (= [[8 "TSP2345"] [9 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (distinct? 1 1.0))
  (is (not (distinct? :x :y :x))))

(deftest flatten-infers-leaves-and-length-from-known-sequential-values
  (let [r (checked "(flatten [1 [2 (list 3)] []])
(flatten nil)
(flatten \"abc\")
(flatten {:a [1]})
(flatten #{1})
(flatten [nil \"abc\" {:a 1} #{2}])
(mapv inc (flatten [1 [2]]))
(mapv inc (flatten [1 [\"wrong\"]]))
(flatten)")]
    (is (= [[9 "TSP2345"] [10 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["(s/coll-of int? :kind seq? :min-count 3 :max-count 3)"
            "(s/coll-of t/never :kind seq? :max-count 0)"
            "(s/coll-of t/never :kind seq? :max-count 0)"
            "(s/coll-of t/never :kind seq? :max-count 0)"
            "(s/coll-of t/never :kind seq? :max-count 0)"]
           (mapv #(at r %) (range 2 7))))
    (is (re-find #"string\?.*:min-count 4 :max-count 4" (at r 7))))
  (is (= [nil "abc" {:a 1} #{2}] (vec (flatten [nil "abc" {:a 1} #{2}]))))
  (is (empty? (flatten "abc")))
  (is (thrown? ClassCastException (mapv inc (flatten [1 ["wrong"]])))))

(deftest flatten-uses-vector-specs-from-another-namespace
  (let [api (parse/parse-string
             "rows.clj"
             "(ns rows (:require [clojure.spec.alpha :as s]))
(s/def ::row (s/coll-of int? :kind vector? :min-count 2 :max-count 3))
(s/fdef read-rows :args (s/cat) :ret (s/coll-of ::row :kind vector? :min-count 2 :max-count 2))
(defn read-rows [] [[1 2] [3 4]])")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [rows :as r] [clojure.spec.alpha :as s]))
(s/fdef leaves :args (s/cat) :ret (s/coll-of int? :kind seq? :min-count 4 :max-count 6))
(defn leaves [] (flatten (r/read-rows)))
(s/fdef wrong :args (s/cat) :ret (s/coll-of string? :kind seq?))
(defn wrong [] (flatten (r/read-rows)))")
        r (check/check-files [api client] {:strict true})]
    (is (= [["client.clj" 5 "TSP2322"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= "(s/coll-of int? :kind seq? :min-count 4 :max-count 6)"
           (main/type-at r "client.clj" 3 17)))))

(def ^:private vector-guard-api
  (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::nums (s/coll-of int? :kind vector? :max-count 5))
(s/def ::nullable (s/coll-of (s/nilable int?) :kind vector? :max-count 5))
(s/def ::long (s/coll-of int? :kind vector? :min-count 3 :max-count 5))
(s/def ::long-nullable (s/coll-of (s/nilable int?) :kind vector? :min-count 3 :max-count 5))
(s/def ::short (s/coll-of int? :kind vector? :max-count 2))
(s/def ::short-nullable (s/coll-of (s/nilable int?) :kind vector? :max-count 2))
(s/fdef long-enough :args (s/cat :v ::long) :ret int?)
(defn long-enough [v] (count v))
(s/fdef nullable-long-enough :args (s/cat :v ::long-nullable) :ret int?)
(defn nullable-long-enough [v] (count v))
(s/fdef short-enough :args (s/cat :v ::short) :ret int?)
(defn short-enough [v] (count v))
(s/fdef nullable-short-enough :args (s/cat :v ::short-nullable) :ret int?)
(defn nullable-short-enough [v] (count v))
(s/def ::seqs (s/nilable (s/coll-of int? :kind seq? :max-count 5)))
(s/fdef enough-two :args (s/cat :v (s/coll-of int? :kind vector? :min-count 2 :max-count 5)) :ret int?)
(defn enough-two [v] (count v))
(s/fdef short-seq :args (s/cat :v (s/coll-of int? :kind seq? :max-count 1)) :ret int?)
(defn short-seq [v] (count v))
(s/fdef integer :args (s/cat :n int?) :ret int?)
(defn integer [n] n)"))

(defn- vector-guard-diagnostics [source]
  (let [client (parse/parse-string "client.clj"
                                 (str "(ns client (:require [clojure.spec.alpha :as s] [api :as api]))\n" source))]
    (:diagnostics (check/check-files [vector-guard-api client] {}))))

(deftest get-in-guards-refine-vector-length-across-namespaces
  (let [issues (vector-guard-diagnostics
                 "(s/fdef present :args (s/cat :v ::api/nums) :ret int?)
(defn present [v]
  (if (some? (get-in v [2])) (+ (api/long-enough v) (api/integer (get-in v [2]))) 0))
(s/fdef absent :args (s/cat :v ::api/nums) :ret int?)
(defn absent [v] (if (nil? (get-in v [2])) (api/short-enough v) 0))
(s/fdef nullable-value :args (s/cat :v ::api/nullable) :ret int?)
(defn nullable-value [v] (if (some? (get-in v [2])) (api/nullable-long-enough v) 0))
(s/fdef unsafe-short :args (s/cat :v ::api/nullable) :ret int?)
(defn unsafe-short [v] (if (nil? (get-in v [2])) (api/nullable-short-enough v) 0))")]
    (is (= [["client.clj" 10 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest get-guards-refine-vector-positions-across-namespaces
  (let [issues (vector-guard-diagnostics
                 "(s/fdef from-get :args (s/cat :v ::api/nums :i #{2}) :ret int?)
(defn from-get [v i]
  (if (some? (get v i nil)) (+ (api/long-enough v) (api/integer (get v 2))) 0))
(s/fdef tuple-lookup :args (s/cat :v (s/tuple (s/nilable int?))) :ret int?)
(defn tuple-lookup [v]
  (if (some? (get v 4294967296)) (api/integer (nth v 0)) 0))
(s/fdef incorrect-default :args (s/cat :v ::api/nums) :ret int?)
(defn incorrect-default [v]
  (if (some? (get v 2 0)) (api/integer (get v 2)) 0))
(s/fdef map-keys :args (s/cat :m (s/map-of int? int?)) :ret int?)
(defn map-keys [m]
  (if (some? (get m 4294967296)) (api/integer (get m 0)) 0))")]
    (is (= [["client.clj" 10 "TSP2531"] ["client.clj" 13 "TSP2531"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest contains-guards-refine-vector-length-across-namespaces
  (let [issues (vector-guard-diagnostics
                 "(s/fdef present :args (s/cat :v ::api/nullable) :ret int?)
(defn present [v]
  (if (contains? v 2) (api/nullable-long-enough v) (api/nullable-short-enough v)))
(s/fdef singleton :args (s/cat :v ::api/nums :i #{2}) :ret int?)
(defn singleton [v i] (if (contains? v i) (api/integer (get v 2)) 0))
(s/fdef wrapped :args (s/cat :v (s/nilable (s/tuple int?))) :ret int?)
(defn wrapped [v] (if (contains? v 4294967296N) (api/integer (get v 0)) 0))
(s/fdef nil-element :args (s/cat :v ::api/nullable) :ret int?)
(defn nil-element [v] (if (contains? v 2) (api/integer (get v 2)) 0))
(s/fdef shadowed :args (s/cat :v ::api/nums) :ret int?)
(defn shadowed [v]
  (let [contains? (fn [v i] true)] (if (contains? v 2) (api/integer (get v 2)) 0)))")]
    (is (= [["client.clj" 10 "TSP2531"] ["client.clj" 13 "TSP2531"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest nth-rejects-inputs-without-positional-access
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/fdef id :args (s/cat :row (s/tuple string? int?)) :ret int?)
(defn id [row] (nth row 1))
(s/fdef bad :args (s/cat :row int?) :ret any?)
(defn bad [row] (nth row 0 :missing))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(a/id [\"valid\" 1])
(a/id [\"invalid\" \"1\"])
(nth {:a 1} 0 nil)
(nth #{1} 0 nil)
(nth 42 -1 :fallback)
(nth 42 -1)
(nth \"abc\" 0)
(nth nil 0 :missing)
(nth (first {:a 1}) 1)
(nth (int-array [1]) 0)")
        result (check/check-files [client api] {})]
    (is (= [["api.clj" 5 "TSP2345"] ["client.clj" 3 "TSP2345"] ["client.clj" 4 "TSP2345"]
            ["client.clj" 5 "TSP2345"] ["client.clj" 7 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))
    (is (= ":fallback" (main/type-at result "client.clj" 6 1))))
  (is (= :fallback (nth 42 -1 :fallback)))
  (is (thrown? UnsupportedOperationException (nth 42 -1)))
  (is (thrown? UnsupportedOperationException (nth {:a 1} 0 nil))))

(deftest nth-guards-with-nil-default-refine-vectors-across-namespaces
  (let [issues (vector-guard-diagnostics
                 "(s/fdef from-nth :args (s/cat :v ::api/nums :i #{2}) :ret int?)
(defn from-nth [v i] (if (some? (nth v i nil)) (api/long-enough v) 0))
(s/fdef tuple-lookup :args (s/cat :v (s/tuple (s/nilable int?))) :ret int?)
(defn tuple-lookup [v] (if (some? (nth v 0 nil)) (api/integer (nth v 0)) 0))
(s/fdef fractional :args (s/cat :v ::api/nums) :ret int?)
(defn fractional [v] (if (some? (nth v 2.9 nil)) (api/long-enough v) 0))
(s/fdef incorrect-default :args (s/cat :v ::api/nums) :ret int?)
(defn incorrect-default [v]
  (if (some? (nth v 2 0)) (api/integer (get v 2)) 0))
(defn overflow [v] (nth v 4294967296 nil))")]
    (is (= [["client.clj" 10 "TSP2531"] ["client.clj" 11 "TSP330-02"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest sequence-positions-share-length-facts-across-namespaces
  (let [issues (vector-guard-diagnostics
                 "(s/fdef vec-second :args (s/cat :v ::api/nums) :ret int?)
(defn vec-second [v]
  (if (some? (second v)) (+ (api/enough-two v) (api/integer (first v))) 0))
(s/fdef seq-second :args (s/cat :v ::api/seqs) :ret int?)
(defn seq-second [v] (if (some? (second v)) (api/integer (first v)) 0))
(s/fdef seq-absent :args (s/cat :v ::api/seqs) :ret int?)
(defn seq-absent [v] (if (and (some? v) (nil? (second v))) (api/short-seq v) 0))
(s/fdef nil-element :args (s/cat :v ::api/nullable) :ret int?)
(defn nil-element [v] (if (some? (second v)) (api/integer (first v)) 0))
(s/fdef alias :args (s/cat :v ::api/nullable) :ret int?)
(defn alias [v] (let [n (second v)] (if (some? n) (api/integer n) 0)))
(s/fdef destructured :args (s/cat :v ::api/nums) :ret int?)
(defn destructured [v]
  (let [[a b] v] (if (some? b) (+ (api/enough-two v) (api/integer a)) 0)))
(s/fdef destructured-alias :args (s/cat :v ::api/nullable) :ret int?)
(defn destructured-alias [[a b :as all]]
  (if (some? b) (+ (count all) (api/integer b)) 0))
(s/fdef destructured-bad :args (s/cat :v ::api/nullable) :ret int?)
(defn destructured-bad [v] (let [[a b] v] (if (some? b) (api/integer a) 0)))")]
    (is (= [["client.clj" 10 "TSP2531"] ["client.clj" 20 "TSP2531"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest range-checks-start-stop-and-step
  (let [r (checked "(range)
(range 4.5)
(range 1 4.5)
(range 0.5 4)
(range 4 0 -2)
(range 1 4.5 2)
(clojure.core/range 0 2 0.5)
(range 4 9 0)
(range 0 2 \"bad\")
(range 0 2 1 :extra)")
        ds (:diagnostics r)]
    (is (= [[10 12 "TSP2345"] [11 1 "TSP2554"]]
           (mapv (juxt (comp :row :pos) (comp :col :pos) :code) ds))
        (pr-str ds))
    (is (= ["(s/coll-of int?)"
            "(s/coll-of int?)"
            "(s/coll-of int?)"
            "(s/coll-of number?)"
            "(s/coll-of int?)"
            "(s/coll-of int?)"
            "(s/coll-of number?)"
            "(s/coll-of int?)"]
           (mapv #(at r %) (range 2 10))))))

(deftest reduce-uses-proven-collection-lengths
  (let [r (checked "(map (fn [group] (reduce (fn [a b] (+ a b)) group))
       (vals (group-by odd? [1 2 3])))
(s/fdef single :args (s/cat :xs (s/coll-of int? :min-count 1 :max-count 1)) :ret int?)
(defn single [xs] (reduce (fn [a b c] :unused) xs))
(s/fdef at-least-two :args (s/cat :xs (s/coll-of int? :min-count 2)) :ret string?)
(defn at-least-two [xs] (reduce (fn [a b] \"always\") xs))
(s/fdef empty-with-init :args (s/cat :xs (s/coll-of int? :max-count 0)) :ret int?)
(defn empty-with-init [xs] (reduce (fn [a] :unused) 42 xs))
(reduce (fn [a b c] :unused) '(1))
(s/fdef maybe-empty :args (s/cat :xs (s/coll-of int?)) :ret any?)
(defn maybe-empty [xs] (reduce (fn [a b] (+ a b)) xs))
(reduce (fn [a b] 0) [])
(s/fdef invalid-step :args (s/cat :xs (s/coll-of int? :min-count 2)) :ret any?)
(defn invalid-step [xs] (reduce (fn [a] a) xs))
(s/fdef maybe-nil :args (s/cat :xs (s/nilable (s/coll-of int? :min-count 1))) :ret any?)
(defn maybe-nil [xs] (reduce (fn [a b] 0) xs))")
        ds (:diagnostics r)]
    (is (= [[12 "TSP2554"] [13 "TSP2554"] [15 "TSP2554"] [17 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) ds))
        (pr-str ds))
    (is (= ["0" "0" "2" "0"]
           (mapv #(second (re-find #"is called with (\d+) argument" (:message %))) ds)))))

(deftest reduce-imports-nonempty-specs-across-namespaces
  (let [api (parse/parse-string
             "groups.clj"
             "(ns sample.groups (:require [clojure.spec.alpha :as s]))
(s/fdef nonempty :args (s/cat) :ret (s/coll-of int? :min-count 1))
(defn nonempty [] [1 2])
(s/fdef maybe-empty :args (s/cat) :ret (s/coll-of int?))
(defn maybe-empty [] [])")
        client (parse/parse-string
                "client.clj"
                "(ns sample.client (:require [sample.groups :as g]))
(reduce (fn [a b] (+ a b)) (g/nonempty))
(reduce (fn [a b] (+ a b)) (g/maybe-empty))")
        ds (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 3 "TSP2554"]]
           (mapv (juxt :file (comp :row :pos) :code) ds))
        (pr-str ds))))

(deftest partition-all-preserves-nested-types-and-bounds
  (let [r (checked "(s/def ::chunk (s/coll-of int? :kind seq? :min-count 1 :max-count 2))
(s/fdef chunks :args (s/cat :xs (s/coll-of int?)) :ret (s/coll-of ::chunk :kind seq?))
(defn chunks [xs] (partition-all 2 xs))
(first (first (partition-all 2 [1])))
(nth (first (partition-all 2 [1])) 1)
(partition-all 3 2 [1 2 3 4 5])
(partition-all 2 nil)
(partition-all 0 [1])
(partition-all 2 1 [(inc \"bad\")])
(partition-all 2 \"bad\" [])
(partition-all 2 3 42)
(partition-all)")]
    (is (= [[6 "TSP330-02"] [10 "TSP2345"] [11 "TSP2345"] [12 "TSP2345"] [13 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["int?"
            "(s/coll-of (s/coll-of int? :kind seq? :min-count 1 :max-count 3) :kind seq? :min-count 3 :max-count 3)"
            "(s/coll-of t/never :kind seq? :max-count 0)"
            "(s/coll-of (s/coll-of t/never :kind seq? :max-count 0) :kind seq? :min-count 1)"]
           (mapv #(at r %) [5 7 8 9])))))

(deftest partition-all-transducers-bound-the-number-and-size-of-groups
  (let [r (checked "(into [] (partition-all 5) [10 20])
(into [] (partition-all 0) [])
(into [] (comp (take 2) (partition-all 4) (interpose :unused)) [10 20 30])
(s/def ::group (s/coll-of int? :kind vector? :min-count 1))
(s/fdef unsplit :args (s/cat :xs (s/coll-of int?)) :ret (s/coll-of ::group :kind vector? :max-count 1))
(defn unsplit [xs] (into [] (partition-all 0) xs))
(s/def ::short (s/coll-of int? :kind vector? :min-count 1 :max-count 2))
(s/fdef groups :args (s/cat :capacity int?) :ret (s/coll-of ::short :kind vector? :max-count 2))
(defn groups [capacity] (into [] (partition-all capacity) [10 20]))")]
    (is (empty? (:diagnostics r)) (pr-str (:diagnostics r)))
    (is (= ["(s/coll-of (s/coll-of int? :kind vector? :min-count 1 :max-count 2) :kind vector? :max-count 1)"
            "(s/coll-of t/never :kind vector?)"
            "(s/coll-of (s/coll-of int? :kind vector? :min-count 1 :max-count 2) :kind vector? :max-count 1)"]
           (mapv #(at r %) [2 3 4])))))

(deftest partition-all-transducers-use-counts-from-another-namespace
  (let [api (parse/parse-string
             "items.clj"
             "(ns items (:require [clojure.spec.alpha :as s]))
(s/fdef items :args (s/cat) :ret (s/coll-of int? :kind vector? :min-count 3 :max-count 3))
(defn items [] [10 20 30])")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [items :as i] [clojure.spec.alpha :as s]))
(s/fdef heads :args (s/cat) :ret (s/coll-of int? :kind vector? :max-count 2))
(defn heads [] (into [] (comp (partition-all 2) (map first)) (i/items)))
(s/def ::group (s/coll-of int? :kind vector? :min-count 1 :max-count 3))
(s/fdef unsplit :args (s/cat) :ret (s/coll-of ::group :kind vector? :max-count 1))
(defn unsplit [] (into [] (comp (partition-all 0) (interpose :unused)) (i/items)))
(nth (into [] (partition-all 2) (i/items)) 2)
(nth (into [] (partition-all 0) (i/items)) 1)
(into [] (comp (take 1) (partition-all 3) (map #(nth % 1))) (i/items))")
        ds (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 7 "TSP330-02"] ["client.clj" 8 "TSP330-02"] ["client.clj" 9 "TSP330-02"]]
           (mapv (juxt :file (comp :row :pos) :code) ds))
        (pr-str ds))))

(deftest partition-by-checks-the-key-and-preserves-sequence-bounds
  (let [r (checked "(partition-by odd? [1 3 2])
(first (first (partition-by identity [42])))
(partition-by nil nil)
(partition-by (fn [a b] a) [])
(partition-by (fn [x] (subs x 0)) [1])
(partition-by identity 42)
(partition-by)
(partition-by identity [1] :extra)
(partition-by {1 nil 2 false} [1 1 2])")
        ds (:diagnostics r)]
    (is (= [[6 "TSP2345"] [7 "TSP2345"] [8 "TSP2554"] [9 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) ds))
        (pr-str ds))
    (is (= ["(s/coll-of (s/coll-of int? :kind seq? :min-count 1 :max-count 3) :kind seq? :min-count 1 :max-count 3)"
            "int?"
            "(s/coll-of t/never :kind seq? :max-count 0)"
            "(s/coll-of t/never :kind seq? :max-count 0)"
            "(s/coll-of (s/coll-of int? :kind seq? :min-count 1 :max-count 3) :kind seq? :min-count 1 :max-count 3)"]
           (mapv #(at r %) [2 3 4 5 10])))))

(deftest partition-by-preserves-spec-bounds-from-another-namespace
  (let [api (parse/parse-string
             "items.clj"
             "(ns items (:require [clojure.spec.alpha :as s]))
(s/fdef items :args (s/cat) :ret (s/coll-of string? :kind vector? :min-count 1 :max-count 3))
(defn items [] [\"ant\" \"arc\" \"bee\"])
(s/fdef bucket :args (s/cat :item string?) :ret string?)
(defn bucket [item] (subs item 0 1))")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [items :as i] [clojure.spec.alpha :as s]))
(s/def ::group (s/coll-of string? :kind seq? :min-count 1 :max-count 3))
(s/fdef groups :args (s/cat) :ret (s/coll-of ::group :kind seq? :min-count 1 :max-count 3))
(defn groups [] (partition-by i/bucket (i/items)))
(s/fdef heads :args (s/cat) :ret (s/coll-of string? :kind vector? :min-count 1 :max-count 3))
(defn heads [] (mapv first (partition-by i/bucket (i/items))))
(doall (partition-by i/bucket [42]))
(doall (partition-by (fn [a b] a) (i/items)))")
        ds (:diagnostics (check/check-files [api client] {:strict-nil true}))]
    (is (= [["client.clj" 7 "TSP2345"] ["client.clj" 8 "TSP2554"]]
           (mapv (juxt :file (comp :row :pos) :code) ds))
        (pr-str ds))))

(deftest positional-callbacks-use-tuple-positions-and-collection-lengths
  (let [r (checked "(mapv first [[:a 1] [:b 2]])
(mapv clojure.core/second [[:a \"ant\"] [:b \"bee\"]])
(let [first (fn [_] true)] (mapv first [[1]]))
(into [] (comp (partition-all 2) (map second)) [1 2 3])
(into [] (comp (partition-all 2) (map first)) [1 2 3])")]
    (is (empty? (codes r)) (pr-str (:diagnostics r)))
    (is (= ["(s/coll-of keyword? :kind vector? :min-count 2 :max-count 2)"
            "(s/coll-of string? :kind vector? :min-count 2 :max-count 2)"
            "(s/coll-of boolean? :kind vector? :min-count 1 :max-count 1)"
            "(s/coll-of (s/nilable int?) :kind vector? :max-count 2)"
            "(s/coll-of int? :kind vector? :max-count 2)"]
           (mapv #(at r %) (range 2 7))))))

(deftest positional-callbacks-use-specs-from-another-namespace
  (let [api (parse/parse-string
             "data.clj"
             "(ns data (:require [clojure.spec.alpha :as s]))
(s/fdef numbers :args (s/cat) :ret (s/coll-of int? :kind vector?))
(defn numbers [] [1 2 3])
(s/fdef pairs :args (s/cat) :ret (s/coll-of (s/tuple keyword? string?) :kind vector?))
(defn pairs [] [[:a \"ant\"] [:b \"bee\"]])")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [data :as d] [clojure.spec.alpha :as s] [clojure.core :as c]))
(s/fdef heads :args (s/cat) :ret (s/coll-of int? :kind vector?))
(defn heads [] (into [] (comp (partition-all 2) (map first)) (d/numbers)))
(s/fdef labels :args (s/cat) :ret (s/coll-of string? :kind vector?))
(defn labels [] (mapv c/second (d/pairs)))
(into [] (comp (partition-all 2) (map second) (map inc)) (d/numbers))
(into [] (comp (map first) (map inc)) (d/pairs))")
        ds (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 6 "TSP2531"] ["client.clj" 7 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) ds))
        (pr-str ds))))

(deftest interpose-keeps-separators-elements-and-lengths
  (let [r (checked "(interpose 42 [\"a\" \"b\"])
(interpose 42 [\"a\"])
(interpose nil [1 2])
(interpose :sep [])
(interpose :sep nil)
(interpose :sep 42)
(interpose)
(interpose :sep [] :extra)
(interpose :sep [(inc \"bad\")])
(interpose :sep)
(s/fdef labels :args (s/cat :labels (s/coll-of string?)) :ret (s/coll-of string? :kind seq?))
(defn labels [labels] (->> labels (map-indexed (fn [i label] (str i label))) (interpose \" / \")))
(s/fdef bad-labels :args (s/cat :labels (s/coll-of string?)) :ret (s/coll-of string? :kind seq?))
(defn bad-labels [labels] (->> labels (map-indexed (fn [i label] (str i label))) (interpose 42)))")]
    (is (= [[7 "TSP2345"] [8 "TSP2554"] [9 "TSP2554"] [10 "TSP2345"] [15 "TSP2322"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["(s/coll-of (t/or string? int?) :kind seq? :min-count 3 :max-count 3)"
            "(s/coll-of string? :kind seq? :min-count 1 :max-count 1)"
            "(s/coll-of (s/nilable int?) :kind seq? :min-count 3 :max-count 3)"
            "(s/coll-of t/never :kind seq? :max-count 0)"
            "(s/coll-of t/never :kind seq? :max-count 0)"
            "t/unknown"]
           (mapv #(at r %) [2 3 4 5 6 11])))))

(deftest concat-combines-element-types-and-length-bounds
  (let [r (checked "(concat)
(concat nil [] [\"sql\"] [1 2])
(clojure.core/concat \"id\" \"\")
(concat (throw (Exception.)) [])
(concat nil 42 [1] :separator)
(s/fdef optional :args (s/cat :xs (s/nilable (s/coll-of int? :kind vector? :min-count 2 :max-count 3))))
(defn optional [xs] (concat xs [\"x\"]))
(s/fdef unbounded :args (s/cat :xs (s/coll-of string? :kind vector? :min-count 1)))
(defn unbounded [xs] (concat xs [\"x\"]))
(s/def ::fragments (s/coll-of string? :kind vector? :min-count 1 :max-count 2))
(s/def ::sql (s/coll-of string? :kind seq? :min-count 3 :max-count 4))
(s/fdef fragments :args (s/cat :acc ::fragments) :ret ::sql)
(defn fragments [acc] (concat acc [\"name\" \",\"]))
(s/fdef bad-content :args (s/cat :acc ::fragments) :ret ::sql)
(defn bad-content [acc] (concat acc [\"name\" 42]))
(s/fdef bad-length :args (s/cat :acc ::fragments) :ret ::sql)
(defn bad-length [acc] (concat acc [\"name\"]))")]
    (is (= [[6 "TSP2345"] [6 "TSP2345"] [16 "TSP2322"] [18 "TSP330-01"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["(s/coll-of t/never :kind seq? :max-count 0)"
            "(s/coll-of (t/or string? int?) :kind seq? :min-count 3 :max-count 3)"
            "(s/coll-of java.lang.Character :kind seq? :min-count 2 :max-count 2)"
            "t/never"]
           (mapv #(at r %) [2 3 4 5])))
    (is (= ["(s/coll-of (t/or int? string?) :kind seq? :min-count 1 :max-count 4)"
            "(s/coll-of string? :kind seq? :min-count 2)"]
           (mapv (fn [[row col]] (main/type-at r "length.clj" row col)) [[8 21] [10 22]])))))

(deftest group-by-preserves-nonempty-values-and-optional-keys
  (let [r (checked "(s/def ::name string?)
(s/def ::package (s/keys :req-un [::name]))
(s/def ::packages (s/coll-of ::package :kind vector?))
(s/def ::group (s/coll-of ::package :kind vector? :min-count 1))
(s/def ::index (s/map-of string? ::group))
(s/fdef by-name :args (s/cat :packages ::packages) :ret ::index)
(defn by-name [packages] (group-by :name packages))
(s/fdef checked-first :args (s/cat :packages ::packages) :ret (s/nilable ::package))
(defn checked-first [packages]
  (when-let [group (get-in (by-name packages) [\"example\"])]
    (first group)))
(s/fdef unchecked-first :args (s/cat :packages ::packages) :ret ::package)
(defn unchecked-first [packages] (first (get-in (group-by :name packages) [\"example\"])))
(get (group-by (fn [_] :found) nil) :found :missing)
(get (group-by (fn [_] :found) []) :found :missing)")]
    (is (= [[14 "TSP2322"]] (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= "::package" (main/type-at r "length.clj" 12 5)))
    (is (= [":missing" ":missing"] (mapv #(at r %) [15 16])))))

(deftest sort-by-preserves-element-types-and-lengths
  (let [r (checked "(sort-by identity > [3 1 2])
(sort-by identity [])
(sort-by identity nil)
(sort-by identity \"ba\")
(sort-by identity 42)
(sort-by identity)
(sort-by identity > [1] :extra)
(sort-by identity (throw (Exception.)) [1])
(sort-by #(inc %) [\"bad\"])
(s/def ::name string?)
(s/def ::tool (s/keys :req-un [::name]))
(s/def ::tools (s/coll-of ::tool :kind vector? :min-count 1 :max-count 3))
(s/def ::ordered (s/coll-of ::tool :kind seq? :min-count 1 :max-count 3))
(s/fdef ordered :args (s/cat :tools ::tools) :ret ::ordered)
(defn ordered [tools] (sort-by :name tools))
(s/fdef first-tool :args (s/cat :tools ::tools) :ret ::tool)
(defn first-tool [tools] (first (sort-by :name tools)))
(s/fdef maybe-first :args (s/cat :tools (s/nilable ::tools)) :ret ::tool)
(defn maybe-first [tools] (first (sort-by :name tools)))")]
    (is (= [[6 "TSP2345"] [7 "TSP2554"] [8 "TSP2554"] [10 "TSP2345"] [20 "TSP2322"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["(s/coll-of int? :kind seq? :min-count 3 :max-count 3)"
            "(s/coll-of t/never :kind seq? :max-count 0)"
            "(s/coll-of t/never :kind seq? :max-count 0)"
            "(s/coll-of java.lang.Character :kind seq? :min-count 2 :max-count 2)"
            "t/never"]
           (mapv #(at r %) [2 3 4 5 9])))))

(deftest sort-reads-the-collection-in-both-arities
  (let [r (checked "(sort [3 1 2])
(sort > [3 1])
(sort (java.util.Comparator/reverseOrder) [\"b\" \"a\"])
(sort nil)
(sort > 42)
(sort)
(sort > [1] :extra)
(sort (throw (Exception.)) [1])
(s/def ::versions (s/coll-of string? :kind vector? :min-count 1 :max-count 3))
(s/def ::ordered (s/coll-of string? :kind seq? :min-count 1 :max-count 3))
(s/fdef ordered :args (s/cat :versions ::versions) :ret ::ordered)
(defn ordered [versions] (sort compare versions))")]
    (is (= [[6 "TSP2345"] [7 "TSP2554"] [8 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["(s/coll-of int? :kind seq? :min-count 3 :max-count 3)"
            "(s/coll-of int? :kind seq? :min-count 2 :max-count 2)"
            "(s/coll-of string? :kind seq? :min-count 2 :max-count 2)"
            "(s/coll-of t/never :kind seq? :max-count 0)"
            "t/never"]
           (mapv #(at r %) [2 3 4 5 9])))))

(deftest reverse-keeps-length-and-reverses-known-positions
  (let [r (checked "(first (reverse [1 \"tail\"]))
(nth (reverse [1 \"tail\"]) 1)
(nth (reverse [1 \"tail\"]) 2 :missing)
(reverse nil)
(reverse [])
(reverse \"ab\")
(reverse 42)
(reverse)
(reverse [1] [])
(reverse (throw (Exception.)))
(s/def ::versions (s/coll-of string? :kind vector? :min-count 1 :max-count 3))
(s/def ::output (s/coll-of string? :kind seq? :min-count 1 :max-count 3))
(s/fdef newest-first :args (s/cat :versions ::versions) :ret ::output)
(defn newest-first [versions] (reverse (sort compare versions)))")]
    (is (= [[8 "TSP2345"] [9 "TSP2554"] [10 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["\"tail\"" "1" ":missing"
            "(s/coll-of t/never :kind seq? :max-count 0)"
            "(s/coll-of t/never :kind seq? :max-count 0)"
            "(s/coll-of java.lang.Character :kind seq? :min-count 2 :max-count 2)"
            "t/never"]
           (mapv #(at r %) [2 3 4 5 6 7 11])))))

(deftest drop-last-trims-length-and-preserves-prefix-positions
  (let [r (checked "(drop-last [1 2 \"tail\"])
(first (drop-last 2 [\"head\" 1 2]))
(nth (drop-last [1 \"head\" :tail]) 1)
(drop-last 3 [\"short\"])
(drop-last 0 [1 2])
(drop-last -1 [1 2])
(drop-last nil)
(drop-last 42)
(drop-last \"bad\" [1])
(drop-last)
(drop-last 2 [] :extra)
(s/def ::input (s/coll-of string? :kind vector? :min-count 2 :max-count 4))
(s/def ::output (s/coll-of string? :kind seq? :min-count 1 :max-count 3))
(s/fdef trim-comma :args (s/cat :fragments ::input) :ret ::output)
(defn trim-comma [fragments] (->> fragments (drop-last)))
(s/fdef trim-n :args (s/cat :n int? :fragments ::input))
(defn trim-n [n fragments] (drop-last n fragments))")]
    (is (= [[9 "TSP2345"] [10 "TSP2345"] [11 "TSP2554"] [12 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["(s/coll-of int? :kind seq? :min-count 2 :max-count 2)"
            "\"head\"" "\"head\""
            "(s/coll-of t/never :kind seq? :max-count 0)"
            "(s/coll-of int? :kind seq? :min-count 2 :max-count 2)"
            "(s/coll-of int? :kind seq? :min-count 2 :max-count 2)"
            "(s/coll-of t/never :kind seq? :max-count 0)"]
           (mapv #(at r %) (range 2 9))))
    (is (= "(s/coll-of string? :kind seq? :max-count 4)"
           (main/type-at r "length.clj" 18 28)))))

(deftest mapv-keeps-the-shortest-input-length
  (let [r (checked "(mapv inc [1 2])
(mapv + [1 2 3] [4 5])
(mapv (fn [a b c d] (str a b c d)) [1 2] [3] [4 5] [6 7])
(mapv inc nil)
(mapv vector [] [\"unused\"])
(mapv identity (throw (Exception.)))
(mapv identity 42)
(mapv identity)
(mapv inc [\"bad\"])
(s/def ::name string?)
(s/def ::package (s/keys :req-un [::name]))
(s/def ::packages (s/coll-of ::package :kind vector? :min-count 1 :max-count 3))
(s/def ::names (s/coll-of string? :kind vector? :min-count 1 :max-count 3))
(s/fdef names :args (s/cat :packages ::packages) :ret ::names)
(defn names [packages] (mapv :name packages))
(s/fdef first-name :args (s/cat :packages ::packages) :ret string?)
(defn first-name [packages] (first (mapv :name packages)))
(s/fdef cap :args (s/cat :xs (s/coll-of int?)))
(defn cap [xs] (mapv + xs [1 2]))")]
    (is (= [[8 "TSP2345"] [9 "TSP2554"] [10 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["(s/coll-of int? :kind vector? :min-count 2 :max-count 2)"
            "(s/coll-of int? :kind vector? :min-count 2 :max-count 2)"
            "(s/coll-of string? :kind vector? :min-count 1 :max-count 1)"
            "(s/coll-of t/never :kind vector? :max-count 0)"
            "(s/coll-of t/never :kind vector? :max-count 0)"
            "t/never"]
           (mapv #(at r %) (range 2 8))))
    (is (= "(s/coll-of int? :kind vector? :max-count 2)"
           (main/type-at r "length.clj" 20 16)))))

(deftest map-keeps-length-without-changing-the-transducer-form
  (let [r (checked "(map inc [1 2])
(map str [1 2 3] [4])
(map str nil)
(map str)
(map)
(map identity 42)
(s/fdef limited :args (s/cat :xs (s/coll-of int?)))
(defn limited [xs] (map + xs [1 2]))")]
    (is (= [[6 "TSP2554"] [7 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["(s/coll-of int? :kind seq? :min-count 2 :max-count 2)"
            "(s/coll-of string? :kind seq? :min-count 1 :max-count 1)"
            "(s/coll-of t/never :kind seq? :max-count 0)"
            "t/unknown"]
           (mapv #(at r %) (range 2 6))))
    (is (= "(s/coll-of int? :kind seq? :max-count 2)"
           (main/type-at r "length.clj" 9 20)))))

(deftest filterv-keeps-an-upper-bound-and-allows-empty-results
  (let [r (checked "(filterv some? [nil 2 3])
(filterv string? [1 2])
(filterv some? nil)
(filterv some? 42)
(filterv some?)
(s/def ::file string?)
(s/def ::migration (s/keys :req-un [::file]))
(s/def ::migrations (s/coll-of ::migration :kind vector? :min-count 1 :max-count 3))
(s/def ::selected (s/coll-of ::migration :kind vector? :max-count 3))
(s/fdef select-file :args (s/cat :file string? :migrations ::migrations) :ret ::selected)
(defn select-file [file migrations] (filterv #(= file (:file %)) migrations))
(s/fdef unchecked-first :args (s/cat :file string? :migrations ::migrations) :ret ::migration)
(defn unchecked-first [file migrations] (first (filterv #(= file (:file %)) migrations)))")]
    (is (= [[5 "TSP2345"] [6 "TSP2554"] [14 "TSP2322"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["(s/coll-of int? :kind vector? :max-count 3)"
            "(s/coll-of t/never :kind vector? :max-count 0)"
            "(s/coll-of t/never :kind vector? :max-count 0)"]
           (mapv #(at r %) [2 3 4])))))

(deftest filter-and-remove-keep-the-upper-bound-and-narrow-elements
  (let [r (checked "(filter some? [nil 2 3])
(remove nil? [nil 2 3])
(filter string? [1 2])
(remove nil? [nil nil])
(filter some?)
(remove nil?)
(filter some? 42)
(remove)")]
    (is (= [[8 "TSP2345"] [9 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["(s/coll-of int? :kind seq? :max-count 3)"
            "(s/coll-of int? :kind seq? :max-count 3)"
            "(s/coll-of t/never :kind seq? :max-count 0)"
            "(s/coll-of t/never :kind seq? :max-count 0)"
            "t/unknown" "t/unknown"]
           (mapv #(at r %) (range 2 8))))))

(deftest into-vectors-adds-length-bounds
  (let [r (checked "(into [] [1 2])
(into [1] [\"one\"])
(into [] \"ab\")
(first (into [] nil))
(into [1] (throw (Exception.)))
(into #{} [1 1])
(s/def ::parts (s/coll-of string? :kind vector? :min-count 1 :max-count 2))
(s/def ::tail (s/coll-of string? :kind seq? :min-count 2 :max-count 3))
(s/def ::result (s/coll-of string? :kind vector? :min-count 3 :max-count 5))
(s/fdef append-parts :args (s/cat :parts ::parts :tail ::tail) :ret ::result)
(defn append-parts [parts tail] (into parts tail))
(s/fdef append-fragments :args (s/cat :parts ::parts) :ret ::result)
(defn append-fragments [parts] (into [] (concat parts [\"name\" \",\"])))")]
    (is (empty? (:diagnostics r)) (pr-str (:diagnostics r)))
    (is (= ["(s/coll-of int? :kind vector? :min-count 2 :max-count 2)"
            "(s/coll-of (t/or int? string?) :kind vector? :min-count 2 :max-count 2)"
            "(s/coll-of java.lang.Character :kind vector? :min-count 2 :max-count 2)"
            "nil?" "t/never" "(s/coll-of int? :kind set?)"]
           (mapv #(at r %) (range 2 8))))))

(deftest into-transducer-results-retain-collection-bounds
  (let [r (checked "(into [0] (take 1) [10 20])
(into #{0} (map (fn [_] 0)) [10 20])
(into nil (map inc) [10 20])
(into nil (take 0) [10 20])
(into [0] cat [[1 2]])
(first (into [0] (filter (fn [_] false)) [10 20]))
(nth (into [] (take 1) [10 20]) 1)
(nth (into [0] (interpose 0) [10 20]) 4)
(s/def ::input (s/coll-of int? :kind seq? :min-count 1 :max-count 2))
(s/def ::result (s/coll-of int? :kind seq? :min-count 1 :max-count 3))
(s/fdef prepend :args (s/cat :items ::input) :ret ::result)
(defn prepend [items] (into items (take 1) [10 20]))
(into [0] identity [10 20])")
        ds (:diagnostics r)]
    (is (= [[8 "TSP330-02"] [9 "TSP330-02"] [14 "TSP210-01"]]
           (mapv (juxt (comp :row :pos) :code) ds))
        (pr-str ds))
    (is (= ["(s/coll-of int? :kind vector? :min-count 1 :max-count 2)"
            "(s/coll-of int? :kind set? :min-count 1 :max-count 3)"
            "(s/nilable (s/coll-of int? :kind seq? :min-count 1 :max-count 2))"
            "nil?"
            "(s/coll-of int? :kind vector? :min-count 1)"
            "0"]
           (mapv #(at r %) (range 2 8))))))

(deftest into-transducer-bounds-use-specs-from-another-namespace
  (let [api (parse/parse-string
             "growth.clj"
             "(ns growth (:require [clojure.spec.alpha :as s]))
(s/fdef pair :args (s/cat) :ret (s/coll-of int? :kind vector? :min-count 2 :max-count 2))
(defn pair [] [10 20])")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [growth :as g] [clojure.spec.alpha :as s]))
(s/fdef prefix :args (s/cat) :ret (s/coll-of int? :kind vector? :min-count 1 :max-count 2))
(defn prefix [] (into [0] (take 1) (g/pair)))
(s/fdef items :args (s/cat) :ret (s/coll-of int? :kind set? :min-count 1 :max-count 3))
(defn items [] (into #{0} (map inc) (g/pair)))
(nth (into [] (take 1) (g/pair)) 1)
(nth (into [0] (interpose 0) (g/pair)) 4)")
        ds (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 6 "TSP330-02"] ["client.clj" 7 "TSP330-02"]]
           (mapv (juxt :file (comp :row :pos) :code) ds))
        (pr-str ds))))

(deftest vec-keeps-bounds-and-known-positions
  (let [r (checked "(vec nil)
(vec #{1 2})
(vec \"ab\")
(get (vec (reverse [1 \"tail\"])) 0)
(get (vec (drop-last [1 nil :tail])) 1 :missing)
(vec (throw (Exception.)))
(vec 42)
(vec)
(vec [] [])
(s/def ::input (s/coll-of string? :kind vector? :min-count 2 :max-count 4))
(s/def ::result (s/coll-of string? :kind vector? :min-count 1 :max-count 3))
(s/fdef trim-fragments :args (s/cat :parts ::input) :ret ::result)
(defn trim-fragments [parts] (-> parts drop-last vec))")]
    (is (= [[8 "TSP2345"] [9 "TSP2554"] [10 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["(s/coll-of t/never :kind vector? :max-count 0)"
            "(s/coll-of int? :kind vector? :min-count 2 :max-count 2)"
            "(s/coll-of java.lang.Character :kind vector? :min-count 2 :max-count 2)"
            "\"tail\"" "nil?" "t/never"]
           (mapv #(at r %) (range 2 8))))))

(deftest sequence-tails-check-inputs-and-arity
  (let [r (checked "(s/fdef needed :args (s/cat :xs (s/coll-of int? :kind seq? :min-count 1)) :ret int?)
(defn needed [xs] (count xs))
(needed (seq [1]))
(needed (next [1 2]))
(seq nil)
(next [1])
(rest nil)
(next (throw (Exception.)))
(seq 42)
(next :wrong)
(rest false)
(seq)
(clojure.core/rest [] :extra)")]
    (is (= [[10 "TSP2345"] [11 "TSP2345"] [12 "TSP2345"] [13 "TSP2554"] [14 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["nil?" "nil?" "(s/coll-of t/never :kind seq? :max-count 0)" "t/never"]
           (mapv #(at r %) [6 7 8 9])))))

(deftest take-preserves-the-selected-prefix-and-its-bounds
  (let [r (checked "(first (take 1 [\"word\" 2]))
(first (take 0 42))
(first (take 0.5 [\"one\" :two]))
(take 2 [1 2 3])
(inc (first (take 1 [\"word\" 2])))
(take \"invalid\" [1])
(take 1 42)
(take)")]
    (is (= [[6 "TSP2345"] [7 "TSP2345"] [8 "TSP2345"] [9 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["\"word\"" "nil?" "\"one\"" "(s/coll-of int? :kind seq? :min-count 2 :max-count 2)"]
           (mapv #(at r %) (range 2 6))))))

(deftest drop-keeps-the-selected-tail-and-its-bounds
  (let [r (checked "(first (drop 1 [1 \"word\" :tail]))
(first (drop 0.5 [:first :second]))
(first (drop 8 [1]))
(first (drop 2 [:gone 1 2 3]))
(inc (first (drop 1 [2 \"word\"])))
(drop 0 42)
(drop 1 [] :extra)")]
    (is (= [[6 "TSP2345"] [7 "TSP2345"] [8 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["\"word\"" ":second" "nil?" "2"]
           (mapv #(at r %) (range 2 6))))))

(deftest nthrest-preserves-the-source-at-zero-and-known-positions-after-skipping
  (let [r (checked "(nthrest nil 0)
(nthrest 42 -1)
(first (nthrest [1 \"word\"] 0.5))
(first (nthrest [1] 2))
(inc (first (nthrest [1 \"word\"] 1)))
(nthrest 42 1)
(nthrest [] :invalid)
(nthrest [])
(nthrest 42 1.5)")]
    (is (= ["nil?" "42" "\"word\"" "nil?"]
           (mapv #(at r %) (range 2 6))))
    (is (= [[6 "TSP2345"] [7 "TSP2345"] [8 "TSP2345"] [9 "TSP2554"] [10 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (= 42 (nthrest 42 -1)))
  (is (thrown? IllegalArgumentException (nthrest 42 1.5))))

(deftest nthnext-sequences-at-zero-and-returns-nil-when-the-tail-is-empty
  (let [r (checked "(first (nthnext [1 \"word\"] 1))
(nthnext [1] 1)
(nthnext [] 0)
(nthnext nil :unused)
(first (nthnext \"ab\" 0))
(inc (first (nthnext [1 \"word\"] 1)))
(nthnext 42 0)
(nthnext [] :invalid)
(nthnext [])")]
    (is (= ["\"word\"" "nil?" "nil?" "nil?" "java.lang.Character"]
           (mapv #(at r %) (range 2 7))))
    (is (= [[7 "TSP2345"] [8 "TSP2345"] [9 "TSP2345"] [10 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (nil? (nthnext nil :unused)))
  (is (thrown? ClassCastException (nthnext [] :invalid))))

(deftest split-at-keeps-the-positions-in-both-parts
  (let [r (checked "(let [[head tail] (split-at 1 [1 \"word\"])] (str (first head) (str/upper-case (first tail))))
(first (second (split-at 0 [\"whole\"])))
(first (first (split-at 8 [\"whole\"])))
(first (second (split-at 8 [\"whole\"])))
(inc (first (second (split-at 1 [1 \"word\"]))))
(split-at 1 42)
(split-at 1)")]
    (is (= ["string?" "\"whole\"" "\"whole\"" "nil?"]
           (mapv #(at r %) (range 2 6))))
    (is (= [[6 "TSP2345"] [7 "TSP2345"] [8 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (= [[1] ["word"]] (mapv vec (split-at 1 [1 "word"]))))
  (is (thrown? IllegalArgumentException (first (second (split-at 1 42))))))

(deftest split-with-keeps-both-sides-of-the-first-rejected-element
  (let [r (checked "(first (first (split-with number? [1 \"word\" 2])))
(first (second (split-with number? [1 \"word\" 2])))
(second (second (split-with number? [1 \"word\" 2])))
(first (second (split-with :unused [])))
(inc (first (second (split-with number? [1 \"word\" 2]))))
(split-with inc 42)
(split-with number?)")]
    (is (= ["1" "\"word\"" "2" "nil?"]
           (mapv #(at r %) (range 2 6))))
    (is (= [[6 "TSP2345"] [7 "TSP2345"] [8 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (= [[1] ["word" 2]] (mapv vec (split-with number? [1 "word" 2]))))
  (is (thrown? ClassCastException (inc (first (second (split-with number? [1 "word" 2])))))))

(deftest take-nth-keeps-strided-positions-and-counts
  (let [r (checked "(second (take-nth 2 [1 :skip \"word\"]))
(take-nth 2 [1 2 3 4 5])
(first (take-nth 0 [\"same\" 2]))
(first (take-nth :unused []))
(into [] (take-nth -2) [1 2 3 4 5])
(inc (second (take-nth 2 [1 :skip \"word\"])))
(take-nth :invalid [1])
(take-nth 2 42)
(take-nth)
(into [] (take-nth 0) [1])
(into [] (take-nth 0) [])")]
    (is (= ["\"word\"" "(s/coll-of int? :kind seq? :min-count 3 :max-count 3)"
            "string?" "nil?" "(s/coll-of int? :kind vector? :max-count 3)"]
           (mapv #(at r %) (range 2 7))))
    (is (= [[7 "TSP2345"] [8 "TSP2345"] [9 "TSP2345"] [10 "TSP2554"] [11 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (= ["same" "same" "same"] (vec (take 3 (take-nth 0 ["same" 2])))))
  (is (= [1 3 5] (into [] (take-nth -2) [1 2 3 4 5])))
  (is (thrown? ArithmeticException (into [] (take-nth 0) [1]))))

(deftest interleave-keeps-row-order-and-stops-at-the-shortest-source
  (let [r (checked "(second (interleave [1] [\"word\" :unused]))
(nth (interleave [1 3] [\"word\"] [:tag :unused]) 2)
(interleave [1 2] [3] [4 5])
(first (interleave [1] []))
(first (interleave [\"only\"]))
(interleave)
(inc (second (interleave [1] [\"word\"])))
(interleave 42 [1])")]
    (is (= ["\"word\"" ":tag" "(s/coll-of int? :kind seq? :min-count 3 :max-count 3)"
            "nil?" "\"only\"" "(s/coll-of t/never :kind seq? :max-count 0)"]
           (mapv #(at r %) (range 2 8))))
    (is (= [[8 "TSP2345"] [9 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (= [1 "word" :tag] (vec (interleave [1 3] ["word"] [:tag :unused]))))
  (is (thrown? ClassCastException (inc (second (interleave [1] ["word"]))))))

(deftest keep-removes-nil-and-retains-false-and-known-positions
  (let [r (checked "(first (keep identity [nil false \"word\"]))
(second (keep identity [nil false \"word\"]))
(keep (fn [_] \"word\") [1 2])
(first (keep (fn [_] nil) [1 2]))
(first (keep 42 []))
(inc (first (keep identity [nil \"bad\"])))
(keep inc [\"bad\"])
(keep 42 [1])
(keep inc 42)
(keep)")]
    (is (= ["false" "\"word\"" "(s/coll-of string? :kind seq? :min-count 2 :max-count 2)"
            "nil?" "nil?"]
           (mapv #(at r %) (range 2 7))))
    (is (= [[7 "TSP2345"] [8 "TSP2345"] [9 "TSP2345"] [10 "TSP2345"] [11 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (= [false "word"] (vec (keep identity [nil false "word"]))))
  (is (thrown? ClassCastException (first (keep inc ["bad"])))))

(deftest keep-indexed-checks-the-index-and-the-element
  (let [r (checked "(keep-indexed (fn [i x] (+ i x)) [2 3])
(first (keep-indexed (fn [_ _] false) [nil]))
(first (keep-indexed (fn [_ _] :unused) []))
(first (keep-indexed :lookup [\"word\"]))
(first (keep-indexed (fn [_ _] (throw (Exception.))) [1]))
(keep-indexed inc [1])
(keep-indexed (fn [i _] (str/trim i)) [1])
(keep-indexed (fn [_ x] (inc x)) [\"wrong\"])
(keep-indexed identity 42)
(keep-indexed)")]
    (is (= ["(s/coll-of int? :kind seq? :min-count 2 :max-count 2)" "boolean?"
            "nil?" "string?" "nil?"]
           (mapv #(at r %) (range 2 7))))
    (is (= [[7 "TSP2554"] [8 "TSP2345"] [9 "TSP2345"] [10 "TSP2345"] [11 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (= [false "word"] (vec (keep-indexed (fn [i x] (case i 0 nil 1 false x)) [0 1 "word"]))))
  (is (thrown? clojure.lang.ArityException (first (keep-indexed inc [1])))))

(deftest dedupe-removes-only-consecutive-repeats
  (let [r (checked "(second (dedupe [\"a\" \"a\" \"b\" \"a\"]))
(nth (dedupe [\"a\" \"a\" \"b\" \"a\"]) 2)
(first (dedupe [:clojure.core/none :clojure.core/none 1]))
(first (dedupe [:clojure.core/none]))
(second (dedupe [nil nil false false]))
(inc (second (dedupe [\"a\" \"a\" \"b\"])))
(dedupe 42)
(dedupe [] :extra)")]
    (is (= ["\"b\"" "\"a\"" "1" "nil?" "false"]
           (mapv #(at r %) (range 2 7))))
    (is (= [[7 "TSP2345"] [8 "TSP2345"] [9 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (= [1 :clojure.core/none] (vec (dedupe [:clojure.core/none :clojure.core/none 1 :clojure.core/none]))))
  (is (thrown? IllegalArgumentException (first (dedupe 42)))))

(deftest cycle-preserves-elements-and-can-repeat-beyond-the-input-length
  (let [r (checked "(first (cycle [\"word\"]))
(cycle [1 2])
(cycle [])
(first (cycle nil))
(take 8 (cycle (int-array [1])))
(inc (first (cycle [\"bad\"])))
(cycle 42)
(cycle [] :extra)")]
    (is (= ["string?" "(s/coll-of int? :kind seq? :min-count 2)"
            "(s/coll-of t/never :kind seq? :max-count 0)" "nil?"
            "(s/coll-of int? :kind seq? :min-count 1 :max-count 8)"]
           (mapv #(at r %) (range 2 7))))
    (is (= [[7 "TSP2345"] [8 "TSP2345"] [9 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (= [1 2 1 2 1 2] (vec (take 6 (cycle [1 2])))))
  (is (thrown? IllegalArgumentException (cycle 42))))

(deftest take-last-keeps-the-suffix-and-returns-nil-when-empty
  (let [r (checked "(first (take-last 1 [1 \"word\"]))
(first (take-last 0.5 [:first :second]))
(take-last 0 [1])
(take-last 2 nil)
(take-last 2 [1 2 3])
(inc (first (take-last 1 [2 \"word\"])))
(take-last 1)
(take-last true [1])")]
    (is (= [[7 "TSP2345"] [8 "TSP2554"] [9 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["\"word\"" ":second" "nil?" "nil?" "(s/coll-of int? :kind seq? :min-count 2 :max-count 2)"]
           (mapv #(at r %) (range 2 7))))))

(deftest distinct-keeps-the-first-occurrence-and-known-length
  (let [r (checked "(first (distinct [:first :first :second]))
(second (distinct [:first :first :second]))
(distinct [nil nil false nil])
(distinct [])
(distinct 42)
(distinct [] :extra)
(distinct #{1 2})
(distinct {:k 1})")]
    (is (= [[6 "TSP2345"] [7 "TSP2554"] [8 "TSP2345"] [9 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= [":first" ":second" "(s/coll-of (s/nilable boolean?) :kind seq? :min-count 2 :max-count 2)"
            "(s/coll-of t/never :kind seq? :max-count 0)"]
           (mapv #(at r %) (range 2 6))))))

(deftest shuffle-returns-a-vector-with-the-input-cardinality
  (let [r (checked "(shuffle [1 2 3])
(second (shuffle [:only]))
(shuffle [])
(shuffle nil)
(shuffle {:x 1})
(shuffle \"abc\")
(shuffle (int-array [1]))
(shuffle 42)
(shuffle [] :extra)
(mapv inc (shuffle [\"wrong\"]))")]
    (is (= [[5 "TSP2345"] [6 "TSP2345"] [7 "TSP2345"] [8 "TSP2345"] [9 "TSP2345"]
            [10 "TSP2554"] [11 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["(s/coll-of int? :kind vector? :min-count 3 :max-count 3)" "nil?"
            "(s/coll-of t/never :kind vector? :max-count 0)"]
           (mapv #(at r %) (range 2 5))))))

(deftest set-counts-distinct-values-without-retaining-positions
  (let [r (checked "(set [\"a\" \"a\" \"b\"])
(set [nil nil false])
(set #{1 2})
(set nil)
(set 42)
(set)
(mapv inc (set [\"bad\"]))")]
    (is (= [[6 "TSP2345"] [7 "TSP2554"] [8 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["(s/coll-of string? :kind set? :min-count 2 :max-count 2)" "(s/coll-of (s/nilable boolean?) :kind set? :min-count 2 :max-count 2)" "(s/coll-of int? :kind set? :min-count 2 :max-count 2)" "(s/coll-of t/never :kind set? :max-count 0)"]
           (mapv #(at r %) (range 2 6))))))

(deftest ffirst-checks-both-sequence-levels
  (let [r (checked "(ffirst [[\"name\" 42] [:other]])
(ffirst [])
(ffirst [nil])
(ffirst [\"text\"])
(inc (ffirst [[\"bad\"]]))
(ffirst [42])
(ffirst 42)
(ffirst [] :extra)")]
    (is (= [[6 "TSP2345"] [7 "TSP2345"] [8 "TSP2345"] [9 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["\"name\"" "nil?" "nil?" "java.lang.Character"] (mapv #(at r %) (range 2 6))))))

(deftest rand-nth-returns-an-element-and-rejects-known-invalid-input
  (let [r (checked "(rand-nth [\"only\"])
(rand-nth [:a :b])
(rand-nth nil)
(rand-nth \"x\")
(rand-nth [])
(rand-nth #{1})
(rand-nth {:x 1})
(rand-nth \"\")
(rand-nth [] :extra)
(inc (rand-nth [\"bad\"]))")]
    (is (= [[6 "TSP330-02"] [7 "TSP2345"] [8 "TSP2345"] [9 "TSP330-02"] [10 "TSP2554"] [11 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["\"only\"" "#{:a :b}" "nil?" "java.lang.Character"] (mapv #(at r %) (range 2 6))))))

(deftest take-while-keeps-the-proved-prefix
  (let [r (checked "(take-while string? [\"one\" \"two\" 3 \"later\"])
(first (take-while string? [\"one\" 2]))
(first (take-while number? [\"one\" 2]))
(second (take-while (fn [_] true) [1 :two]))
(inc (first (take-while string? [\"bad\"])))
(take-while number? 42)
(take-while)
(take-while (fn [_ _] true) [])")]
    (is (= [[6 "TSP2345"] [7 "TSP2345"] [8 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["(s/coll-of string? :kind seq? :min-count 2 :max-count 2)" "\"one\"" "nil?" ":two"]
           (mapv #(at r %) (range 2 6))))))

(deftest drop-while-preserves-elements-after-the-first-rejection
  (let [r (checked "(first (drop-while string? [\"skip\" 2 \"keep\"]))
(second (drop-while string? [\"skip\" 2 \"keep\"]))
(first (drop-while string? [\"all\"]))
(first (drop-while (fn [_] false) [\"first\"]))
(str/trim (first (drop-while string? [\"skip\" 2 \"keep\"])))
(drop-while number? 42)
(drop-while string? [] :extra)
(drop-while (fn [_ _] true) [])")]
    (is (= [[6 "TSP2345"] [7 "TSP2345"] [8 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["2" "\"keep\"" "nil?" "\"first\""] (mapv #(at r %) (range 2 6))))))

(deftest literal-strings-preserve-bounds-under-sequence-operations
  (let [r (checked "(first \"ab\")
(first \"\")
(second \"😀\")
(second \"x\")
(seq \"\")
(next \"x\")
(rest \"ab\")
(rest \"\")
(int (first (seq \"ab\")))
(int (second \"x\"))
(int (first (next \"x\")))")]
    (is (= [[11 "TSP2531"] [12 "TSP2531"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["java.lang.Character" "nil?" "java.lang.Character" "nil?" "nil?" "nil?"
            "(s/coll-of java.lang.Character :kind seq? :min-count 1 :max-count 1)"
            "(s/coll-of t/never :kind seq? :max-count 0)" "int?"]
           (mapv #(at r %) (range 2 11))))))

(deftest array-sequences-preserve-length-and-component-across-namespaces
  (let [api (parse/parse-string
             "arrays.clj"
             "(ns arrays)
(defn numbers [] (int-array [1 2]))
(defn words [] (into-array String [\"text\"]))")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [arrays :as a]))
(inc (first (a/numbers)))
(mapv inc (seq (a/numbers)))
(first (int-array 0))
(first (a/words))
(second (a/words))
(first (char-array [\\a]))
(next (a/numbers))
(rest (boolean-array 0))
(inc (first (a/words)))
(inc (first (aclone (int-array 0))))")
        r (check/check-files [api client] {:strict-nil true})]
    (is (= [[10 "TSP2345"] [11 "TSP2531"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["int?" "(s/coll-of int? :kind vector? :min-count 2 :max-count 2)"
            "nil?" "(s/nilable string?)" "nil?" "java.lang.Character"
            "(s/coll-of int? :kind seq? :min-count 1 :max-count 1)"
            "(s/coll-of t/never :kind seq? :max-count 0)"]
           (mapv #(main/type-at r "client.clj" % 1) (range 2 10))))))

(deftest sequence-positions-preserve-typed-map-entries
  (let [api (parse/parse-string
             "people.clj"
             "(ns people)
(defn person [] {:name \"Ada\"})")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [people :as p] [clojure.spec.alpha :as s] [clojure.string :as str]))
(key (first (p/person)))
(val (first (seq (p/person))))
(first {})
(second (p/person))
(str/upper-case (val (first (next {:a \"one\" :b \"two\"}))))
(val (first (mapv first [(p/person)])))
(inc (val (first (p/person))))
(s/fdef value :args (s/cat :m (s/map-of keyword? string?)) :ret (s/nilable string?))
(defn value [m] (when-let [entry (first m)] (val entry)))")
        r (check/check-files [api client] {:strict-nil true})]
    (is (= [[8 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= [":name" "\"Ada\"" "nil?" "nil?" "string?" "\"Ada\""]
           (mapv #(main/type-at r "client.clj" % 1) (range 2 8))))))

(deftest some-returns-the-first-truthy-result
  (let [r (checked "(some (fn [_] \"found\") [1 2])
(some (fn [_] false) [1])
(some 42 [])
(some string? [\"known\"])
(some string? [1])
(str/upper-case (some (fn [_] \"found\") [1]))
(some 42 [1])
(some inc 42)
(some string?)
(s/fdef non-nil :args (s/cat :x any?) :ret some?)
(defn non-nil [_] false)
(some non-nil [1])")]
    (is (= [[8 "TSP2345"] [9 "TSP2345"] [10 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["\"found\"" "nil?" "nil?" "true" "nil?" "string?"]
           (mapv #(at r %) (range 2 8))))
    (is (= "(s/nilable some?)" (at r 13)))))

(deftest every-distinguishes-empty-and-proved-predicate-results
  (let [r (checked "(every? 42 [])
(every? string? [\"one\" \"two\"])
(every? string? [1])
(every? (fn [_] nil) [1])
(every? (fn [_] true) [])
(every? inc 42)
(every? string?)
(every? 42 [1])")]
    (is (= [[7 "TSP2345"] [8 "TSP2554"] [9 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["true" "true" "false" "false" "true"] (mapv #(at r %) (range 2 7))))))

(deftest not-any-tracks-absence-of-truthy-predicate-results
  (let [r (checked "(not-any? 42 [])
(not-any? string? [1 2])
(not-any? string? [\"match\"])
(not-any? (fn [_] :truthy) [1])
(not-any? (fn [_] false) [1])
(not-any? string? 42)
(not-any? string? [] :extra)
(not-any? 42 [1])")]
    (is (= [[7 "TSP2345"] [8 "TSP2554"] [9 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["true" "true" "false" "false" "true"] (mapv #(at r %) (range 2 7))))))

(deftest not-every-checks-the-predicate-and-negates-its-result
  (let [r (checked "(not-every? 42 [])
(not-every? string? [\"one\"])
(not-every? string? [1 2])
(not-every? (fn [_] nil) [1])
(not-every? string? 42)
(not-every? string?)
(not-every? 42 [1])")]
    (is (= [[6 "TSP2345"] [7 "TSP2554"] [8 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["false" "false" "true" "true"] (mapv #(at r %) (range 2 6))))))

(deftest run-checks-callbacks-only-when-an-element-is-present
  (let [r (checked "(run! 42 [])
(run! (fn [_ _] true) nil)
(run! inc [1 2])
(run! (fn [_] (throw (Exception.))) [1])
(run! 42 [1])
(run! inc 42)
(run! inc)")]
    (is (= [[6 "TSP2345"] [7 "TSP2345"] [8 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["nil?" "nil?" "nil?" "t/never"] (mapv #(at r %) (range 2 6))))))

(deftest group-by-bounds-groups-and-skips-unreachable-callbacks
  (let [r (checked "(group-by (fn [_] :bucket) [1 2])
(get (group-by 42 []) :bucket :missing)
(get (group-by (fn [_ _] true) nil) :bucket :missing)
(group-by (fn [_] (throw (Exception.))) [1])
(group-by identity 42)
(group-by identity)
(group-by 42 [1])")]
    (is (= [[6 "TSP2345"] [7 "TSP2554"] [8 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["(s/map-of keyword? (s/coll-of int? :kind vector? :min-count 1 :max-count 2))"
            ":missing" ":missing" "t/never"]
           (mapv #(at r %) (range 2 6))))))

(deftest max-key-checks-numeric-keys-and-returns-a-candidate
  (let [r (checked "(max-key 42 :only)
(max-key count \"a\" \"bb\")
(str/upper-case (max-key count \"a\" \"bb\" \"ccc\"))
(max-key inc 1 2)
(max-key (fn [_] :not-numeric) 1 2)
(max-key (fn [x] (subs x 1)) \"one\" 42)
(max-key count)")]
    (is (= [[6 "TSP2345"] [7 "TSP2345"] [7 "TSP2345"] [8 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= [":only" "#{\"a\" \"bb\"}" "string?" "#{1 2}"]
           (mapv #(at r %) (range 2 6))))))

(deftest min-key-checks-all-candidates-and-keeps-their-result-type
  (let [r (checked "(min-key 42 \"only\")
(min-key count \"aa\" \"b\")
(min-key inc 100 200 300)
(min-key (fn [_] \"not-numeric\") 1 2)
(min-key 42 1 2)
(min-key)")]
    (is (= [[5 "TSP2345"] [6 "TSP2345"] [7 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["\"only\"" "#{\"aa\" \"b\"}" "#{100 200 300}"]
           (mapv #(at r %) (range 2 5))))))

(deftest key-checks-map-entry-input-and-preserves-callback-results
  (let [r (checked "(key (first {:name \"Ada\"}))
(mapv key (seq {:name \"Ada\"}))
(key (java.util.AbstractMap$SimpleEntry. :name 1))
(key [1 2])
(key 42)
(key nil)
(key)
(mapv key [42])")]
    (is (= [[5 "TSP2345"] [6 "TSP2345"] [7 "TSP2531"] [8 "TSP2554"] [9 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= [":name" "(s/coll-of keyword? :kind vector? :min-count 1 :max-count 1)" "t/unknown"]
           (mapv #(at r %) (range 2 5))))))

(deftest val-checks-map-entry-input-and-retains-value-types
  (let [r (checked "(val (first {:name \"Ada\"}))
(val (first {:missing nil}))
(mapv val (seq {:name \"Ada\"}))
(val (java.util.AbstractMap$SimpleEntry. :name 1))
(val 42)
(val nil)
(val [1 2])
(val :one :two)
(mapv val [42])
(mapv inc (mapv val (seq {:name \"Ada\"})))")]
    (is (= [[6 "TSP2345"] [7 "TSP2531"] [8 "TSP2345"] [9 "TSP2554"] [10 "TSP2345"] [11 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["\"Ada\"" "nil?" "(s/coll-of string? :kind vector? :min-count 1 :max-count 1)" "t/unknown"]
           (mapv #(at r %) (range 2 6))))))

(deftest first-and-second-check-direct-and-callback-invocations
  (let [r (checked "(first [1])
(second [:head \"tail\"])
(first nil)
(second [1])
(first (throw (Exception.)))
(mapv first [[1] [2]])
(first 42)
(clojure.core/second :wrong)
(first [42] :extra)
(second)
(mapv first [42])
(mapv second [[1] [2]] [[3] [4]])
(let [first (fn [x y] x)] (mapv first [1] [2]))")]
    (is (= [[8 "TSP2345"] [9 "TSP2345"] [10 "TSP2554"] [11 "TSP2554"] [12 "TSP2345"] [13 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["1" "\"tail\"" "nil?" "nil?" "t/never"
            "(s/coll-of int? :kind vector? :min-count 2 :max-count 2)"]
           (mapv #(at r %) (range 2 8))))))

(deftest conj-grows-vectors-with-known-lengths-and-positions
  (let [r (checked "(conj)
(conj [1] \"word\")
(get (conj [1] nil :tail) 1 :missing)
(get (conj [1] nil :tail) 2)
(get (into [1] [nil :tail]) 2)
(conj [1] (throw (Exception.)))
(s/def ::parts (s/coll-of string? :kind vector? :min-count 2 :max-count 4))
(s/fdef close-fragments :args (s/cat :parts ::parts) :ret ::parts)
(defn close-fragments [parts] (-> parts drop-last vec (conj \")\")))")]
    (is (empty? (:diagnostics r)) (pr-str (:diagnostics r)))
    (is (= ["(s/coll-of t/never :kind vector? :max-count 0)"
            "(s/coll-of (t/or int? string?) :kind vector? :min-count 2 :max-count 2)"
            "nil?" ":tail" ":tail" "t/never"]
           (mapv #(at r %) (range 2 8))))))

(deftest vector-growth-keeps-loop-and-reducer-types
  (let [r (checked "(s/fdef loop-label :args (s/cat :flag boolean?) :ret string?)
(defn loop-label [flag]
  (loop [pieces [] label \"ready\"]
    (if flag
      (recur (conj pieces label) label)
      (str/upper-case label))))
(s/fdef append-if :args (s/cat :flags (s/coll-of boolean? :kind vector?))
  :ret (s/coll-of string? :kind vector?))
(defn append-if [flags]
  (reduce (fn [acc flag] (if flag (conj acc \"next\") acc)) [] flags))")]
    (is (empty? (:diagnostics r)) (pr-str (:diagnostics r)))))

(deftest pop-preserves-vector-prefix-and-rejects-known-empty-branches
  (let [r (checked "(get (pop [\"kept\" :tail]) 0)
(pop [nil])
(pop nil)
(pop [])
(pop)
(s/fdef maybe-pop :args (s/cat :flag boolean?) :ret vector?)
(defn maybe-pop [flag] (pop (if flag [] [1])))
(s/def ::input (s/coll-of string? :kind vector? :min-count 1 :max-count 2))
(s/def ::result (s/coll-of string? :kind vector? :min-count 2 :max-count 3))
(s/fdef joined-fragments :args (s/cat :parts ::input) :ret ::result)
(defn joined-fragments [parts] (pop (into [] (concat parts [\"name\" \",\"]))))")]
    (is (= [[5 "TSP330-02"] [6 "TSP2554"] [8 "TSP330-02"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["\"kept\"" "(s/coll-of t/never :kind vector? :max-count 0)" "nil?" "t/never"]
           (mapv #(at r %) (range 2 6))))))

(deftest peek-reads-the-last-known-vector-position
  (let [r (checked "(peek [1 \"tail\"])
(peek [])
(peek nil)
(peek [1 nil])
(peek (pop [\"kept\" :tail]))
(peek)
(s/def ::parts (s/tuple int? string?))
(s/fdef last-fragment :args (s/cat :parts ::parts) :ret string?)
(defn last-fragment [parts] (peek parts))
(s/fdef bounded :args (s/cat :xs (s/coll-of string? :kind vector? :min-count 1)) :ret string?)
(defn bounded [xs] (peek xs))
(s/fdef maybe-empty :args (s/cat :xs (s/coll-of string? :kind vector? :max-count 2)) :ret string?)
(defn maybe-empty [xs] (peek xs))")]
    (is (= [[7 "TSP2554"] [14 "TSP2322"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["\"tail\"" "nil?" "nil?" "nil?" "\"kept\""]
           (mapv #(at r %) (range 2 7))))))

(deftest last-uses-length-and-known-sequence-positions
  (let [r (checked "(last (reverse [\"kept\" 1]))
(last [])
(last nil)
(last \"a\")
(last 42)
(last)
(s/def ::versions (s/coll-of string? :kind vector? :min-count 1 :max-count 3))
(s/fdef latest :args (s/cat :versions ::versions) :ret string?)
(defn latest [versions] (last (sort versions)))
(s/fdef maybe-empty :args (s/cat :xs (s/coll-of string? :kind seq? :max-count 3)) :ret string?)
(defn maybe-empty [xs] (last xs))")]
    (is (= [[6 "TSP2345"] [7 "TSP2554"] [12 "TSP2322"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["\"kept\"" "nil?" "nil?" "java.lang.Character"]
           (mapv #(at r %) (range 2 6))))))

(deftest array-constructors-carry-their-jvm-class
  (let [r (checked "(byte-array [1 2])
(boolean-array [true])
(object-array nil)
(alength (double-array 2 [1.0]))
(int-array \"bad-size\" [1])
(byte-array :bad-source)
(object-array 2 nil)")]
    (is (= ["[B" "[Z" "[Ljava.lang.Object;" "2"] (mapv #(at r %) [2 3 4 5])))
    (is (= [[6 "TSP2345"] [7 "TSP2345"] [8 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))))

(deftest make-array-infers-the-component-and-checks-dimensions
  (let [r (checked "(make-array String 2)
(make-array Integer/TYPE 2 3)
(make-array java.lang.Boolean/TYPE 2 0 3)
(make-array String/1 2)
(make-array (Class/forName \"[B\") 2)
(make-array String 1 \\u0002)
(make-array String -0.5)
(let [^objects values (make-array Object 1)] (aset values 0 42))
(make-array String :bad)
(make-array String -1)
(make-array String 0 -1)
(make-array String 2147483648)
(make-array Void/TYPE 2)
(make-array 42 2)
(make-array String)
(defn dynamic-array [^Class component] (make-array component 1))
(let [String 42] (make-array String 2))" {})]
    (is (= ["[Ljava.lang.String;" "[[I" "[[[Z" "[[Ljava.lang.String;" "[[B"
            "[[Ljava.lang.String;" "[Ljava.lang.String;" "42"]
           (mapv #(at r %) (range 2 10))))
    (is (= [[10 "TSP2345"] [11 "TSP2345"] [12 "TSP2345"] [13 "TSP2345"]
            [14 "TSP2345"] [15 "TSP2345"] [16 "TSP2554"] [18 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))))

(deftest make-array-counts-the-component-rank-toward-the-jvm-limit
  (let [dimensions (apply str (repeat 254 " 0"))
        r (checked (str "(make-array String/1" dimensions ")\n"
                        "(make-array String/1" dimensions " 0)") {})]
    (is (= (str (apply str (repeat 255 "[")) "Ljava.lang.String;") (at r 2)))
    (is (= [[3 "TSP2345" "make-array exceeds the JVM limit of 255 array dimensions"]]
           (mapv (juxt (comp :row :pos) :code :message) (:diagnostics r))))))

(deftest make-array-preserves-the-component-across-namespaces
  (let [files [(parse/parse-string "provider.clj"
                                  "(ns provider (:require [clojure.spec.alpha :as s]))
(defn labels [] (make-array String 2))
(s/fdef ints :args (s/cat) :ret #(instance? int/2 %))
(defn ints [] (make-array Integer/TYPE 2 3))")
               (parse/parse-string "consumer.clj"
                                   "(ns consumer (:require [provider :as p]))
(aset (p/labels) 0 \"value\")
(aset (p/labels) 0 42)
(alength (p/ints))")]
        r (check/check-files files {})]
    (is (= [["consumer.clj" 3 20 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) (comp :col :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))))

(deftest into-array-uses-the-explicit-component-class
  (let [r (checked "(into-array String [\"label\" nil])
(alength (into-array java.io.File nil))
(into-array String [42])
(into-array String true)
(let [String (class 42)] (into-array String [42]))")]
    (is (= ["[Ljava.lang.String;" "0" "t/unknown"] (mapv #(at r %) [2 3 6])))
    (is (= [[4 "TSP2345"] [5 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))))

(deftest into-array-checks-class-and-sequence-inputs
  (let [r (checked "(into-array 42 [])
(into-array nil [])
(into-array \"java.lang.String\" [])
(let [String 42] (into-array String []))
(let [component (class \"example\")] (into-array component [\"ok\"]))
(into-array (Object.))
(into-array Object (java.util.Date.))
(into-array Object (java.util.Collections/emptyIterator))
(into-array (StringBuilder. \"abc\"))
(into-array Object (java.util.ArrayList. [\"ok\"]))
(into-array Object {:answer 42})
(into-array Object (list :a :b))
(into-array Object (int-array [1 2]))
(into-array Object nil)
(into-array Object true)
(into-array (reify clojure.lang.Seqable (seq [_] (seq [\"ok\"]))))
(defn unknown-items [items] (into-array items))" {})]
    (is (= [[2 13] [3 13] [4 13] [5 30] [7 13] [8 20] [9 20] [16 20]]
           (mapv (juxt (comp :row :pos) (comp :col :pos)) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (every? #(= "TSP2345" (:code %)) (:diagnostics r)))
    (is (= (repeat 7 "[Ljava.lang.Object;") (mapv #(at r %) [10 11 12 13 14 15 17])))
    (is (= "[Ljava.lang.Object;" (main/type-at r "length.clj" 18 29)))
    (is (= "t/unknown" (at r 6)))))

(deftest into-array-checks-inputs-across-namespace-contracts
  (let [files [(parse/parse-string "provider.clj"
                                  "(ns provider (:require [clojure.spec.alpha :as s]))
(s/fdef component :args (s/cat :name string?) :ret #(instance? Class %))
(defn component [^String name] (Class/forName name))
(s/fdef timestamp :args (s/cat) :ret #(instance? java.util.Date %))
(defn timestamp [] (java.util.Date.))")
               (parse/parse-string "consumer.clj"
                                   "(ns consumer (:require [provider :as p]))
(into-array (p/component \"java.lang.String\") [\"ok\"])
(into-array (p/timestamp) [])
(into-array (p/timestamp))
(alength (into-array String [\"ok\"]))")]
        r (check/check-files files {})]
    (is (= [["consumer.clj" 3 13 "TSP2345"] ["consumer.clj" 4 13 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) (comp :col :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= "1" (main/type-at r "consumer.clj" 5 1)))))

(deftest explicit-reference-into-array-checks-boxed-components
  (let [r (checked "(into-array Integer [(int 42) nil])
(into-array Integer [42])
(into-array Long [42 nil])
(into-array Long [(int 42)])
(into-array Byte [(byte 42) nil])
(into-array Byte [(short 42)])
(into-array Short [(short 42)])
(into-array Short [(byte 42)])
(into-array Float [(float 1.5)])
(into-array Float [1.5])
(into-array Double [1.5 nil])
(into-array Double [(float 1.5)])
(into-array Number [42 (int 42) 1.5 nil])
(into-array Integer (int-array [42]))
(into-array Long (int-array [42]))
(into-array Integer (make-array Integer 1))
(into-array Long (into-array Integer [(int 42)]))
(into-array String (int-array [42]))
(into-array String (into-array Long [42]))
(into-array Character \"abc\")
(into-array String \"abc\")
(into-array String/1 [(into-array String [\"ok\"]) nil])
(into-array String/1 [(object-array [\"bad\"])])
(alength (into-array Long (int-array 0)))
(alength (into-array String \"\"))
(defn opaque [^Number x] (into-array Integer [x]))
(defn from-unknown [xs] (into-array Integer xs))" {})]
    (is (= ["[Ljava.lang.Integer;" "[Ljava.lang.Long;" "[Ljava.lang.Byte;" "[Ljava.lang.Short;"
            "[Ljava.lang.Float;" "[Ljava.lang.Double;" "[Ljava.lang.Number;" "[Ljava.lang.Integer;"
            "[Ljava.lang.Integer;" "[Ljava.lang.Character;" "[[Ljava.lang.String;" "0" "0"]
           (mapv #(at r %) [2 4 6 8 10 12 14 15 17 21 23 25 26])))
    (is (= (conj (mapv #(vector % "TSP2345") [3 5 7 9 11 13 16 18 19 20 22 24]) [27 "TSP210-01"])
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= 22 (-> r :diagnostics first :pos :col)))
    (is (= "java.lang.Short is not assignable to (s/nilable java.lang.Byte)"
           (-> r :diagnostics (nth 2) :message)))))

(deftest explicit-reference-into-array-preserves-contracts-across-namespaces
  (let [files [(parse/parse-string "provider.clj"
                                  "(ns provider (:require [clojure.spec.alpha :as s]))
(s/fdef boxed :args (s/cat :items (s/tuple int?)) :ret #(instance? Integer/1 %))
(defn boxed [[item]] (into-array Integer [(int item) nil]))
(defn flags [] (into-array Boolean [true false]))")
               (parse/parse-string "consumer.clj"
                                   "(ns consumer (:require [provider :as p]))
(into-array Integer (p/boxed [42]))
(alength (into-array Boolean (p/flags)))
(into-array Long (p/boxed [42]))")]
        r (check/check-files files {})]
    (is (= [["consumer.clj" 4 18 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) (comp :col :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= "2" (main/type-at r "consumer.clj" 3 1)))))

(deftest implicit-into-array-checks-the-first-runtime-component
  (let [r (checked "(into-array [\"one\" nil \"two\"])
(into-array [nil \"one\" 42])
(into-array [])
(alength (into-array nil))
(into-array [42 43])
(into-array [(byte 1) (byte 2)])
(into-array (int-array [1 2]))
(alength (into-array \"ab\"))
(into-array [\"one\" 42])
(into-array [42 4.5])
(into-array [(byte 1) 2])
(aset (into-array [\"one\"]) 0 42)
(aset (into-array [nil]) 0 42)
(defn opaque [x] (into-array [x]))
(defn written [x] (aset (into-array [x]) 0 42))" {})]
    (is (= ["[Ljava.lang.String;" "[Ljava.lang.Object;" "[Ljava.lang.Object;" "0"
            "[Ljava.lang.Long;" "[Ljava.lang.Byte;" "[Ljava.lang.Integer;" "2" "42"]
           (mapv #(at r %) [2 3 4 5 6 7 8 9 14])))
    (is (= "[Ljava.lang.Object;" (main/type-at r "length.clj" 15 18)))
    (is (= [[10 20 "TSP2345"] [11 17 "TSP2345"] [12 23 "TSP2345"]
            [13 30 "TSP2345"] [16 44 "TSP210-01"]]
           (mapv (juxt (comp :row :pos) (comp :col :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))))

(deftest implicit-into-array-preserves-contracts-across-namespaces
  (let [files [(parse/parse-string "provider.clj"
                                  "(ns provider (:require [clojure.spec.alpha :as s]))
(s/fdef from-spec :args (s/cat :items (s/coll-of string? :kind vector? :min-count 1))
  :ret #(instance? String/1 %))
(defn from-spec [items] (into-array items))
(defn labels [] (into-array [\"one\" \"two\"]))")
               (parse/parse-string "consumer.clj"
                                   "(ns consumer (:require [provider :as p]))
(aset (p/from-spec [\"one\"]) 0 \"new\")
(alength (p/labels))
(aset (p/labels) 0 42)")]
        r (check/check-files files {})]
    (is (= [["consumer.clj" 4 20 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) (comp :col :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= "2" (main/type-at r "consumer.clj" 3 1)))))

(deftest explicit-primitive-into-array-checks-casts-and-boxed-values
  (let [r (checked "(into-array Integer/TYPE [(byte 1) \\b])
(into-array Byte/TYPE [127])
(into-array Short/TYPE [32767])
(into-array Character/TYPE [65535])
(into-array Float/TYPE [1.25])
(into-array Boolean/TYPE [true false])
(into-array Long/TYPE [1 \\a])
(into-array Double/TYPE [1 2.5 \\a])
(into-array Double/TYPE (int-array [1 2]))
(alength (into-array Boolean/TYPE nil))
(alength (into-array Character/TYPE \"ab\"))
(aget (into-array Integer/TYPE [1]) 0)
(into-array Integer/TYPE [nil])
(into-array Integer/TYPE [\"bad\"])
(into-array Byte/TYPE [127 128])
(into-array Short/TYPE [32767 32768])
(into-array Character/TYPE [65535 65536])
(into-array Integer/TYPE [2147483647 2147483648])
(into-array Float/TYPE [1 3.5e38])
(into-array Float/TYPE [\\a])
(into-array Boolean/TYPE [1])
(into-array Long/TYPE [1.5])
(into-array Long/TYPE [1N])
(into-array Double/TYPE [1M])
(into-array Long/TYPE (double-array [1]))
(into-array Void/TYPE [])
(defn from-number [^Number n] (into-array Long/TYPE [n]))" {})]
    (is (= ["[I" "[B" "[S" "[C" "[F" "[Z" "[J" "[D" "[D" "0" "2" "int?"]
           (mapv #(at r %) (range 2 14))))
    (is (= (conj (mapv #(vector % "TSP2345") (range 14 28)) [28 "TSP210-01"])
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= 27 (-> r :diagnostics first :pos :col)))))

(deftest explicit-primitive-into-array-preserves-contracts-across-namespaces
  (let [files [(parse/parse-string "provider.clj"
                                  "(ns provider (:require [clojure.spec.alpha :as s]))
(s/fdef indices :args (s/cat :values (s/tuple int? int?)) :ret #(instance? int/1 %))
(defn indices [values] (into-array Integer/TYPE values))
(defn flags [] (into-array Boolean/TYPE [true false]))")
               (parse/parse-string "consumer.clj"
                                   "(ns consumer (:require [provider :as p]))
(aset-int (p/indices [1 2]) 0 3)
(alength (p/flags))
(aset-double (p/indices [1 2]) 0 3.5)")]
        r (check/check-files files {})]
    (is (= [["consumer.clj" 4 14 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) (comp :col :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= "2" (main/type-at r "consumer.clj" 3 1)))))

(deftest aclone-preserves-array-classes-for-reads-and-writes
  (let [r (checked "(aclone (into-array String [\"old\"]))
(aclone (int-array [1]))
(aclone (into-array int/1 [(int-array 1)]))
(let [^objects values (into-array String [\"old\"]) copy (aclone values)] (aset copy 0 42))
(let [copy (aclone (into-array String [\"old\"]))] (aset copy 0 \"new\"))
(let [copy (aclone (object-array 1))] (aset copy 0 42))
(let [copy (aclone (float-array 1))] (aset copy 0 3.5))
(let [copy (aclone (make-array Float/TYPE 1))] (aset copy 0 3.5))
(aclone [:not-an-array])
(aclone nil)
(aclone)
(aclone (int-array 1) (int-array 1))
(defn wide [^objects values] (aset (aclone values) 0 42))" {})]
    (is (= [[5 "TSP2345"] [9 "TSP210-01"] [10 "TSP2345"] [11 "TSP2531"]
            [12 "TSP2554"] [13 "TSP2554"] [14 "TSP210-01"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["[Ljava.lang.String;" "[I" "[[I" "\"new\"" "42" "double?"]
           (mapv #(at r %) [2 3 4 6 7 8])))))

(deftest aclone-preserves-the-array-contract-across-namespaces
  (let [files [(parse/parse-string "provider.clj"
                                  "(ns provider (:require [clojure.spec.alpha :as s]))
(defn labels [] (aclone (into-array String [\"old\"])))
(s/fdef copied-ints :args (s/cat :values #(instance? int/1 %)) :ret #(instance? int/1 %))
(defn copied-ints [values] (aclone values))")
               (parse/parse-string "consumer.clj"
                                   "(ns consumer (:require [provider :as p]))
(aset (p/labels) 0 \"value\")
(aset (p/labels) 0 42)
(aget (p/copied-ints (int-array [1])) 0)")]
        r (check/check-files files {})]
    (is (= [["consumer.clj" 3 20 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) (comp :col :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= "int?" (main/type-at r "consumer.clj" 4 1)))))

(deftest areduce-checks-the-types-reached-on-later-iterations
  (let [r (checked "(let [a (int-array [1 2])]
  (areduce a i acc 0 (+ acc (aget a i))))
(areduce (int-array [1 2]) i acc 0 (if (number? acc) \"text\" acc))
(areduce (int-array [1 2]) i acc 0 (if (= i 0) \"text\" (inc acc)))
(areduce (int-array [1 2]) i acc nil (str acc))
(areduce [:not-array] i acc 0 acc)
(areduce nil i acc 0 acc)
(areduce (int-array 1) i acc 0)
(areduce (int-array 1) :idx acc 0 acc)
(areduce (int-array 1) i i 0 i)
(defn opaque [^ints a f] (areduce a i acc 0 (f acc)))
(areduce (int-array [1 2]) i acc i (inc acc))
(areduce (int-array [1 2]) i acc [] (conj acc i))
(areduce (int-array [1 2]) i acc nil (clojure.lang.Reduced. i))" {})]
    (is (= [[5 "TSP2345"] [7 "TSP2345"] [8 "TSP2531"] [9 "TSP2554"]
            [10 "TSP2345"] [11 "TSP2345"] [12 "TSP210-01"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["int?" "(t/or int? string?)" "(s/nilable string?)" "int?"
            "(s/nilable clojure.lang.Reduced)"]
           (mapv #(at r %) [2 4 6 13 15])))
    (is (= #{:accumulator} (:reasons (last (:diagnostics r)))))))

(deftest areduce-joins-array-evidence-on-the-back-edge
  (let [r (checked "(defn replace-array [^ints input ^objects alternate]
  (areduce input i acc (object-array 1)
    (do (aset acc 0 42) alternate)))" {})]
    (is (= [[4 21 "TSP210-01"]]
           (mapv (juxt (comp :row :pos) (comp :col :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))))

(deftest areduce-preserves-the-result-across-namespaces
  (let [files [(parse/parse-string "provider.clj"
                                  "(ns provider (:require [clojure.spec.alpha :as s]))
(s/fdef sum-array :args (s/cat :values #(instance? int/1 %)) :ret int?)
(defn sum-array [values] (areduce values i acc 0 (+ acc (aget values i))))
(defn states [^ints values]
  (areduce values i acc 0 (if (number? acc) \"ready\" acc)))")
               (parse/parse-string "consumer.clj"
                                   "(ns consumer (:require [provider :as p]))
(inc (p/sum-array (int-array [1 2])))
(inc (p/states (int-array [1 2])))
(p/sum-array (long-array 1))")]
        r (check/check-files files {})]
    (is (= [["consumer.clj" 3 6 "TSP2345"] ["consumer.clj" 4 14 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) (comp :col :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))))

(deftest amap-checks-the-value-written-to-the-clone
  (let [r (checked "(amap (into-array String [\"old\"]) i copy \"label\")
(amap (into-array String [\"old\"]) i copy 42)
(amap (int-array [4 5]) i copy i)
(amap (byte-array [1]) i copy i)
(amap (byte-array [1]) i copy (byte i))
(amap (byte-array [1]) i copy (aget copy i))
(amap (float-array [1.0]) i copy 3.5)
(amap (make-array Float/TYPE 1) i copy 3.5)
(amap [:not-array] i copy \"label\")
(let [^objects values (into-array String [\"old\"])] (amap values i copy i))
(let [mapped (amap (make-array Object 1) i copy :label)] (aset mapped 0 42))
(amap (int-array [1]) i i 0)
(amap (int-array [1]) :idx copy 0)
(amap (int-array [1]) i copy)" {})]
    (is (= [[3 "TSP2345"] [5 "TSP2345"] [9 "TSP210-01"] [10 "TSP2345"]
            [11 "TSP2345"] [13 "TSP2345"] [14 "TSP2345"] [15 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["[Ljava.lang.String;" "[I" "[B" "[B" "[F" "42"]
           (mapv #(at r %) [2 4 6 7 8 12])))))

(deftest amap-preserves-the-result-array-across-namespaces
  (let [files [(parse/parse-string "provider.clj"
                                  "(ns provider)
(defn labels [] (amap (into-array String [\"old\"]) i copy (aget copy i)))")
               (parse/parse-string "consumer.clj"
                                   "(ns consumer (:require [provider :as p]))
(aset (p/labels) 0 \"value\")
(aset (p/labels) 0 42)")]
        r (check/check-files files {})]
    (is (= [["consumer.clj" 3 20 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) (comp :col :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))))

(deftest to-array-preserves-reference-array-components-and-checks-inputs
  (let [r (checked "(to-array (into-array String [\"old\"]))
(to-array (make-array Integer/TYPE 2 3))
(let [^objects values (into-array String [\"old\"])] (aset (to-array values) 0 42))
(aset (to-array [1 2]) 0 :changed)
(aset (to-array (int-array [1 2])) 1 :boxed)
(alength (to-array nil))
(alength (to-array \"𝄞\"))
(alength (to-array [1 2]))
(alength (to-array (int-array [1 2])))
(aset (to-array (java.util.ArrayList. [1])) 0 :ok)
(to-array {:one 1})
(to-array true)
(to-array (StringBuilder. \"xy\"))
(defn seqable-only [^clojure.lang.Seqable xs] (to-array xs))
(defn unknown [xs] (to-array xs))
(to-array)
(defn wide [^objects values] (aset (to-array values) 0 42))
(alength (to-array '(1 2)))
(alength (to-array (list 1 2 3)))
(alength (to-array (quote ())))
(defn abstract-list [^clojure.lang.IPersistentList values] (to-array values))" {})]
    (is (= ["[Ljava.lang.String;" "[[I" ":changed" ":boxed" "0" "2" "2" "2" ":ok"
            "[Ljava.lang.Object;" "2" "3" "0"]
           (mapv #(at r %) [2 3 5 6 7 8 9 10 11 12 19 20 21])))
    (is (= [[4 "TSP2345"] [13 "TSP2345"] [14 "TSP2345"] [15 "TSP2345"]
            [16 "TSP210-01"] [17 "TSP2554"] [18 "TSP210-01"] [22 "TSP210-01"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))))

(deftest to-array-preserves-the-component-and-length-across-namespaces
  (let [files [(parse/parse-string "provider.clj"
                                  "(ns provider)
(defn labels [] (to-array (into-array String [\"first\"])))
(defn converted [] (to-array [1 2]))")
               (parse/parse-string "consumer.clj"
                                   "(ns consumer (:require [provider :as p]))
(aset (p/labels) 0 \"value\")
(aset (p/labels) 0 42)
(aset (p/converted) 0 :tag)
(aget (p/converted) 2)")]
        r (check/check-files files {})]
    (is (= [["consumer.clj" 3 20 "TSP2345"] ["consumer.clj" 5 21 "TSP330-02"]]
           (mapv (juxt :file (comp :row :pos) (comp :col :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))))

(deftest to-array-2d-checks-its-collection-and-row-conversions
  (let [r (checked "(to-array-2d [[1 2] [3]])
(alength (to-array-2d [[1 2] [3]]))
(alength (to-array-2d (list [1] [2])))
(alength (to-array-2d '()))
(alength (to-array-2d [(int-array [1 2]) nil \"ab\" {:a 1} '(1)]))
(to-array-2d [[1] true])
(to-array-2d (list [1] false))
(to-array-2d nil)
(to-array-2d {:a [1]})
(to-array-2d (int-array [1]))
(to-array-2d)
(defn opaque [^java.util.Collection rows] (to-array-2d rows))
(defn iterable [^Iterable rows] (to-array-2d rows))
(aset (to-array-2d [[1]]) 0 (to-array [2]))
(aset (to-array-2d [[1]]) 0 \"not a row\")" {})]
    (is (= ["[[Ljava.lang.Object;" "2" "2" "0" "5" "[Ljava.lang.Object;"]
           (mapv #(at r %) [2 3 4 5 6 15])))
    (is (= [[7 19 "TSP2345"] [8 24 "TSP2345"] [9 14 "TSP2531"]
            [10 14 "TSP2345"] [11 14 "TSP2345"] [12 1 "TSP2554"]
            [13 56 "TSP210-01"] [14 46 "TSP2345"] [16 29 "TSP2345"]]
           (mapv (juxt (comp :row :pos) (comp :col :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))))

(deftest to-array-2d-preserves-outer-array-facts-across-namespaces
  (let [files [(parse/parse-string "provider.clj"
                                  "(ns provider)
(defn grid [] (to-array-2d [[1 2] [3]]))")
               (parse/parse-string "consumer.clj"
                                   "(ns consumer (:require [provider :as p]))
(alength (p/grid))
(aset (p/grid) 0 (to-array [3]))
(aget (p/grid) 2)
(aset (p/grid) 0 \"bad\")")]
        r (check/check-files files {})]
    (is (= [["consumer.clj" 4 16 "TSP330-02"] ["consumer.clj" 5 18 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) (comp :col :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= "2" (main/type-at r "consumer.clj" 2 1)))))

(deftest jvm-array-access-checks-the-converted-index
  (let [r (checked "(aget (int-array [4 5]) 1)
(aget (int-array [4 5]) 2)
(aget (int-array [4 5]) -1)
(aget (int-array [4 5]) 1.9)
(aget (int-array [4 5]) 2.9)
(aget (int-array [4]) \\u0000)
(aset (aclone (object-array 1)) 1 \"bad\")
(aset-int (int-array 1) 4294967296 42)
(aget (int-array [4]) 4294967296)
(aset-int (int-array 1) 0 42)
(defn at-end [^ints values] (aget values -1))
(defn choice [flag] (aget (int-array 1) (if flag 0 1)))
(amap (int-array 0) i copy (aget copy 0))" {})]
    (is (= [[3 "TSP330-02"] [4 "TSP330-02"] [6 "TSP330-02"] [8 "TSP330-02"]
            [9 "TSP330-02"] [12 "TSP330-02"] [13 "TSP330-02"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["int?" "t/never" "int?" "int?" "42"]
           (mapv #(at r %) [2 3 5 7 11])))
    (is (= "array index 2.9 (converted to 2) is out of bounds for length 2"
           (:message (nth (:diagnostics r) 2))))
    (is (= "array index 1 may be out of bounds for length 1"
           (:message (last (:diagnostics r)))))))

(deftest nested-array-access-reports-the-first-proven-out-of-bounds-index
  (let [r (checked "(aget (make-array Integer/TYPE 1 1) 1 0)
(aset (make-array Object 1 1) 1 0 :bad)
(aset-int (make-array Integer/TYPE 1 1) 1 0 42)
(aget (make-array Integer/TYPE 1 1) \\u0000 0)
(aget (make-array Integer/TYPE 1 1) 0 \\u0000)" {})]
    (is (= [[2 37 "TSP330-02"] [3 31 "TSP330-02"] [4 41 "TSP330-02"]
            [5 37 "TSP2531"] [6 37 "TSP2531"] [6 39 "TSP2345"]]
           (mapv (juxt (comp :row :pos) (comp :col :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))))

(deftest array-index-bounds-use-the-allocated-length-across-namespaces
  (let [files [(parse/parse-string "provider.clj"
                                  "(ns provider)
(defn values [] (aclone (int-array [4 5])))")
               (parse/parse-string "consumer.clj"
                                   "(ns consumer (:require [provider :as p]))
(aget (p/values) 1)
(aget (p/values) 2)
(aset-int (p/values) 2 42)")]
        r (check/check-files files {})]
    (is (= [["consumer.clj" 3 18 "TSP330-02"] ["consumer.clj" 4 22 "TSP330-02"]]
           (mapv (juxt :file (comp :row :pos) (comp :col :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))))

(deftest aget-uses-the-jvm-component-and-checks-indices
  (let [r (checked "(aget (byte-array [1]) 0)
(aget (char-array [\\a]) 0)
(aget (into-array String [\"value\" nil]) 0)
(defn guarded [^\"[[I\" array] (when-some [row (aget array 0)] (aget row 0)))
(defn unchecked [^\"[[I\" array] (aget array 0 0))
(aget [1] 0)
(aget (int-array [1]) :bad)
(aget)" {})]
    (is (= ["int?" "java.lang.Character" "(s/nilable string?)"] (mapv #(at r %) [2 3 4])))
    (is (= [[6 "TSP2531"] [7 "TSP2345"] [8 "TSP2345"] [9 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))))

(deftest aset-preserves-reference-array-components-through-local-hints
  (let [r (checked "(let [^objects values (into-array String [\"param\"])]
  (aset values 0 42))
(let [^objects values (into-array String [\"param\"]) alias values]
  (clojure.core/aset alias 0 \"value\"))
(let [^objects values (object-array 1)] (aset values 0 42))
(aset (into-array String [\"param\"]) 0 nil)
(aset (into-array Integer [nil]) 0 42)
(aset (into-array Long [nil]) 0 42)" {})]
    (is (= [[3 18 "TSP2345"] [8 36 "TSP2345"]]
           (mapv (juxt (comp :row :pos) (comp :col :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["\"value\"" "42" "nil?" "42"] (mapv #(at r %) [4 6 7 9])))))

(deftest aset-checks-each-array-alternative
  (let [r (checked "(defn checked-write [flag]
  (aset (if flag (object-array 1) (into-array String [\"x\"])) 0 42))
(defn unchecked-write [flag ^objects external]
  (aset (if flag (object-array 1) external) 0 42))
(defn nil-write [^objects external] (aset external 0 nil))
(defn nested [^\"[[Ljava.lang.String;\" matrix]
  (clojure.core/aset matrix 0 0 42))
(aset (object-array 1) :bad-index nil)
(aset [] 0 nil)
(aset nil 0 nil)
(aset (object-array 1) 0)
(aset (int-array 1) 0 42)" {})]
    (is (= [[3 "TSP2345"] [5 "TSP210-01"] [8 "TSP2531"] [8 "TSP2345"]
            [9 "TSP2345"] [10 "TSP2345"] [11 "TSP2531"] [12 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= "int?" (at r 13)))))

(deftest aset-checks-primitive-overloads-and-returns-the-stored-type
  (let [r (checked "(aset (byte-array 1) 0 42)
(aset (byte-array 1) 0 (byte 42))
(let [^bytes values (byte-array 1)] (aset values 0 42))
(aset (boolean-array 1) 0 true)
(aset (boolean-array 1) 0 nil)
(aset (int-array 1) 0 3.5)
(aset (int-array 1) 0 (byte 42))
(aset (float-array 1) 0 3.5)
(let [a (float-array 1) alias a] (aset alias 0 3.5))
(let [^Object a (float-array 1)] (aset a 0 3.5))
(aset (int-array 1) 0 2147483648)
(aset (char-array 1) 0 \\a)" {})]
    (is (= [[2 "TSP2345"] [4 "TSP2345"] [6 "TSP2345"] [7 "TSP2345"]
            [11 "TSP210-01"] [12 "TSP210-01"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["int?" "boolean?" "int?" "double?" "double?" "java.lang.Character"]
           (mapv #(at r %) [3 5 8 9 10 13])))))

(deftest variadic-aset-uses-array-set-and-preserves-the-original-value
  (let [r (checked "(defn put-long [^long/2 matrix]
  (aset matrix \\u0000 0 42))
(defn wrong-width [^int/2 matrix]
  (aset matrix 0 0 42))
(defn put-int [^int/2 matrix]
  (aset matrix 0 0 (byte 42)))
(defn wrong-index [^int/2 matrix]
  (aset matrix 0 \\u0000 (int 42)))" {})]
    (is (= [[3 "TSP2531"] [5 "TSP2531"] [5 "TSP2345"] [7 "TSP2531"]
            [9 "TSP2531"] [9 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= "42" (main/type-at r "length.clj" 3 3)))))

(deftest aset-uses-array-classes-from-another-namespace
  (let [files [(parse/parse-string "provider.clj"
                                  "(ns provider)
(defn labels ^\"[Ljava.lang.String;\" [] (into-array String [\"label\"]))
(defn aset [array index value] value)")
               (parse/parse-string "consumer.clj"
                                   "(ns consumer (:require [provider :as p]))
(aset (p/labels) 0 \"value\")
(aset (p/labels) 0 42)
(p/aset (p/labels) 0 42)")]
        r (check/check-files files {})]
    (is (= [["consumer.clj" 3 20 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) (comp :col :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))))

(deftest aset-infers-a-primitive-result-across-namespaces
  (let [files [(parse/parse-string "provider.clj"
                                  "(ns provider)
(defn store! [^float/1 values] (aset values 0 3.5))")
               (parse/parse-string "consumer.clj"
                                   "(ns consumer (:require [provider :as p] [clojure.spec.alpha :as s]))
(s/fdef store-float! :args (s/cat) :ret double?)
(defn store-float! [] (p/store! (float-array 1)))
(p/store! (double-array 1))")]
        r (check/check-files files {})]
    (is (= [["consumer.clj" 4 11 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) (comp :col :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= "double?" (main/type-at r "consumer.clj" 3 23)))))

(deftest primitive-aset-checks-jvm-widening-and-clojure-coercion
  (let [r (checked "(aset-int (int-array 1) 0 42)
(aset-int (long-array 1) 0 42)
(aset-int (short-array 1) 0 42)
(aset-int (int-array 1) 0 \"forty-two\")
(aset-boolean (boolean-array 1) 0 nil)
(aset-boolean (boolean-array 1) 0 \"enabled\")
(aset-byte (byte-array 1) 0 128)
(aset-char (int-array 1) 0 \\a)
(aset-char (short-array 1) 0 \\a)
(aset-float (double-array 1) 0 \\a)
(aset-float (double-array 1) 0 1.0)
(aset-long (double-array 1) 0 42)
(aset-double (float-array 1) 0 42)
(aset-short (int-array 1) 0 0)
(aset-byte (object-array 1) 0 42)
(aset-byte)
(aset-byte nil 0 42)
(aset-int (int-array 1) \\u0000 42)
(defn update-first [^int/2 matrix]
  (when-some [row (aget matrix 0)] (aset-int row 0 5)))
(defn update-matrix [^int/2 matrix]
  (aset-int matrix \\u0000 0 5))" {})]
    (is (= [[4 "TSP2345"] [5 "TSP2345"] [8 "TSP2345"] [10 "TSP2345"]
            [11 "TSP2345"] [14 "TSP2345"] [16 "TSP2345"] [17 "TSP2554"]
            [18 "TSP2531"] [19 "TSP2345"] [23 "TSP2531"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["42" "42" "nil?" "\"enabled\"" "java.lang.Character" "1.0"]
           (mapv #(at r %) [2 3 6 7 9 12])))))

(deftest primitive-aset-works-in-a-cross-namespace-spec-contract
  (let [files [(parse/parse-string "provider.clj"
                                  "(ns provider
  (:refer-clojure :exclude [aset-int])
  (:require [clojure.spec.alpha :as s]))
(s/fdef store! :args (s/cat :target #(instance? long/1 %) :value number?) :ret number?)
(defn store! [target value] (clojure.core/aset-int target 0 value))
(defn aset-int [array index value] value)")
               (parse/parse-string "consumer.clj"
                                   "(ns consumer (:require [provider :as p]))
(p/store! (long-array 1) 42)
(p/store! (int-array 1) 42)
(p/aset-int (short-array 1) 0 \"value\")")]
        r (check/check-files files {})]
    (is (= [["consumer.clj" 3 11 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) (comp :col :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))))

(deftest array-class-symbol-hints-check-array-contracts-across-namespaces
  (let [files [(parse/parse-string "provider.clj"
                                  "(ns provider (:import [java.io File]))
(defn labels-count [^String/1 values] (alength values))
(defn files-count [^File/1 files] (alength files))
(defn row-value [^int/2 matrix] (when-some [row (aget matrix 0)] (aget row 0)))
(defn wrong-store [^String/1 values] (aset values 0 42))")
               (parse/parse-string "consumer.clj"
                                   "(ns consumer (:require [provider :as p]))
(p/labels-count (into-array String [\"value\"]))
(p/labels-count (int-array 1))
(p/files-count (into-array java.io.File nil))
(p/files-count (object-array 1))
(p/row-value (into-array int/1 [(int-array [42])]))")]
        r (check/check-files files {})]
    (is (= [["consumer.clj" 3 17 "TSP2345"]
            ["consumer.clj" 5 16 "TSP2345"]
            ["provider.clj" 5 53 "TSP2345"]]
           (sort (map (juxt :file (comp :row :pos) (comp :col :pos) :code) (:diagnostics r))))
        (pr-str (:diagnostics r)))))

(deftest alength-preserves-known-lengths-and-drops-conflicting-facts
  (let [r (checked "(alength (aclone (int-array [1 2 3])))
(alength (amap (byte-array [1]) i copy (byte i)))
(alength (into-array String [\"a\" \"b\"]))
(alength (make-array Integer/TYPE 2 3))
(alength (float-array 2.5 [1.0]))
(alength (object-array nil))
(defn same-size [flag]
  (alength (if flag (int-array 1) (int-array 1))))
(defn mixed-size [flag]
  (alength (if flag (int-array 1) (int-array 2))))
(defn unknown-size [flag ^ints values]
  (alength (if flag (int-array 1) values)))
(defn reversed [flag ^ints values]
  (alength (if flag values (int-array 1))))
(defn mixed-box [flag]
  (aset (into-array Long [nil]) 0 (if flag (alength (int-array 3)) 3)))" {})]
    (is (= ["3" "1" "2" "2" "2" "0"] (mapv #(at r %) (range 2 8))))
    (is (= ["1" "int?" "int?" "int?"]
           (mapv #(main/type-at r "length.clj" % 3) [9 11 13 15])))
    (is (= ["TSP210-01"] (codes r)) (pr-str (:diagnostics r)))))

(deftest areduce-checks-only-reachable-steps-for-zero-and-one-elements
  (let [r (checked "(areduce (make-array String 0) i acc :empty (inc acc))
(areduce (int-array [1]) i acc 0 (if (= i 0) \"one\" (inc acc)))
(areduce (int-array [1]) i acc \"bad\" (inc acc))
(defn short-array [flag]
  (areduce (if flag (int-array 0) (long-array 1)) i acc :empty \"one\"))" {})]
    (is (= [":empty" "\"one\""] (mapv #(at r %) [2 3])))
    (is (= "#{:empty \"one\"}" (main/type-at r "length.clj" 6 3)))
    (is (= [[4 "TSP2345"]] (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))))

(deftest a-known-array-length-is-visible-to-other-namespaces
  (let [files [(parse/parse-string "provider.clj"
                                  "(ns provider)
(defn fixed [] (aclone (int-array 3)))")
               (parse/parse-string "consumer.clj"
                                   "(ns consumer (:require [provider :as p] [clojure.spec.alpha :as s]))
(s/fdef three :args (s/cat :value #{3}) :ret #{3})
(defn three [value] value)
(three (alength (p/fixed)))
(three (alength (int-array 2)))")]
        r (check/check-files files {})]
    (is (= [["consumer.clj" 5 8 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) (comp :col :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))))

(deftest alength-checks-array-hints-and-returns-int
  (let [r (checked "(let [^bytes body (byte-array 3)] (alength body))
(let [^objects values (object-array 2)] (alength values))
(alength [:not-an-array])
(alength nil)
(alength)
(defn unchecked [values] (alength values))" {})]
    (is (= ["3" "2"] (mapv #(at r %) [2 3])))
    (is (= [[4 "TSP2345"] [5 "TSP2531"] [6 "TSP2554"] [7 "TSP210-01"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r))))
  (let [r (checked "(defn size [^bytes body] (if (some? body) (alength body) 0))")]
    (is (empty? (:diagnostics r)) (pr-str (:diagnostics r)))))

(deftest pmap-and-doall-preserve-bounds-and-original-shape
  (let [r (checked "(doall 1 [:head \"tail\"])
(doall nil)
(nth (doall :unused []) 0 :empty)
(doall :bad [1])
(doall 42)
(doall)
(pmap +)
(pmap inc false)
(s/def ::names (s/coll-of string? :kind vector? :min-count 1 :max-count 3))
(s/def ::packages (s/coll-of string? :kind seq? :min-count 1 :max-count 3))
(s/fdef realized :args (s/cat :names ::names) :ret ::packages)
(defn realized [names] (->> names (pmap str/upper-case) doall))
(s/fdef zipped :args (s/cat :xs (s/coll-of int? :min-count 3 :max-count 5)
                          :ys (s/coll-of int? :min-count 1 :max-count 2))
  :ret (s/coll-of int? :kind seq? :min-count 1 :max-count 2))
(defn zipped [xs ys] (doall 1 (pmap + xs ys)))
(nth (doall 1 [:head \"tail\"]) 0)")]
    (is (= [[5 "TSP2345"] [6 "TSP2345"] [7 "TSP2554"] [8 "TSP2554"] [9 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["(s/coll-of (t/or keyword? string?) :kind vector?)" "nil?" ":empty" ":head"]
           (mapv #(at r %) [2 3 4 18])))))

(deftest butlast-returns-a-bounded-prefix-or-nil
  (let [r (checked "(first (butlast [\"kept\" :tail]))
(butlast [1])
(butlast nil)
(butlast \"ab\")
(butlast 42)
(butlast)
(s/def ::path (s/coll-of string? :kind vector? :min-count 2 :max-count 4))
(s/def ::prefix (s/coll-of string? :kind vector? :min-count 1 :max-count 3))
(s/fdef prefix :args (s/cat :path ::path) :ret ::prefix)
(defn prefix [path] (into [] (butlast path)))
(s/fdef maybe-single :args (s/cat :xs (s/coll-of string? :kind seq? :min-count 1 :max-count 3))
  :ret (s/coll-of string? :kind seq? :min-count 1 :max-count 2))
(defn maybe-single [xs] (butlast xs))")]
    (is (= [[6 "TSP2345"] [7 "TSP2554"] [14 "TSP2322"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["\"kept\"" "nil?" "nil?" "(s/coll-of java.lang.Character :kind seq? :min-count 1 :max-count 1)"]
           (mapv #(at r %) (range 2 6))))))

(deftest count-equality-refines-length-in-both-branches
  (let [r (checked "(s/def ::pair (s/coll-of string? :kind seq? :min-count 1 :max-count 2))
(s/fdef choose :args (s/cat :pair ::pair) :ret string?)
(defn choose [pair]
  (if (= 1 (count pair)) (first pair) (second pair)))
(s/fdef label :args (s/cat :xs (s/nilable (s/coll-of string? :kind vector?))) :ret string?)
(defn label [xs]
  (if (= (count xs) 0) \"empty\" (first xs)))
(s/fdef exact :args (s/cat :xs (s/coll-of string? :kind vector?)) :ret string?)
(defn exact [xs]
  (let [alias xs
        other? (not= (count alias) 2)]
    (if other? \"other\" (second xs))))
(let [xs nil] (if (= 0 (count xs)) :zero (subs 42 0)))")]
    (is (empty? (:diagnostics r)) (pr-str (:diagnostics r)))
    (is (= ":zero" (at r 14)))))

(deftest count-refinement-retains-interior-gaps-and-element-nilability
  (let [r (checked "(s/fdef without-two :args (s/cat :xs (s/coll-of string? :kind vector? :min-count 1 :max-count 3)) :ret any?)
(defn without-two [xs]
  (if (not= 2 (count xs))
    xs
    nil))
(s/fdef nullable-element :args (s/cat :xs (s/coll-of (s/nilable string?) :kind vector?)) :ret string?)
(defn nullable-element [xs] (if (= 1 (count xs)) (first xs) \"empty\"))
(s/fdef shadowed :args (s/cat :xs (s/coll-of string? :kind vector?)) :ret string?)
(defn shadowed [xs]
  (let [count (fn [_] 2)]
    (if (= 2 (count xs)) (second xs) \"other\")))
(s/fdef rebound :args (s/cat :xs (s/coll-of string? :kind vector?)) :ret string?)
(defn rebound [xs]
  (with-redefs [clojure.core/count (fn [_] 2)]
    (if (= 2 (count xs)) (second xs) \"other\")))")]
    (is (= [[8 "TSP2322"] [11 "TSP2322"] [15 "TSP2322"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= "(t/or (s/coll-of string? :kind vector? :min-count 1 :max-count 1) (s/coll-of string? :kind vector? :min-count 3 :max-count 3))"
           (main/type-at r "length.clj" 5 5)))))

(deftest ordered-count-comparisons-refine-both-branches
  (doseq [opts [{} {:strict-nil true}]]
    (let [r (checked "(s/def ::names (s/coll-of string? :kind vector? :min-count 1 :max-count 3))
(s/fdef split :args (s/cat :xs ::names) :ret any?)
(defn split [xs]
  (let [alias xs
        short? (< (count alias) 2)]
    (if short?
      xs
      xs)))
(s/fdef choices :args (s/cat :xs ::names) :ret string?)
(defn choices [xs]
  (str (str/upper-case (if (<= (count xs) 1) (first xs) (second xs)))
       (str/upper-case (if (> (count xs) 1) (second xs) (first xs)))
       (str/upper-case (if (>= (count xs) 2) (second xs) (first xs)))
       (str/upper-case (if (< 1 (count xs)) (second xs) (first xs)))
       (str/upper-case (if (<= 2 (count xs)) (second xs) (first xs)))
       (str/upper-case (if (> 2 (count xs)) (first xs) (second xs)))
       (str/upper-case (if (>= 1 (count xs)) (first xs) (second xs)))))
(let [xs nil] (if (> 0 (count xs)) (subs 42 0) :empty))
(let [xs [1]] (if (<= (count xs) 9223372036854775807) :in-range (subs 42 0)))
(s/fdef not-enough :args (s/cat :xs ::names) :ret string?)
(defn not-enough [xs] (if (< (count xs) 3) (second xs) \"other\"))
(s/fdef shadowed :args (s/cat :xs ::names) :ret string?)
(defn shadowed [xs]
  (let [< (fn [_ _] false)]
    (if (< (count xs) 2) \"short\" (second xs))))
(s/fdef nullable :args (s/cat :xs (s/nilable ::names)) :ret string?)
(defn nullable [xs] (if (>= (count xs) 1) (first xs) \"empty\"))" opts)]
      (is (= [[22 "TSP2322"] [25 "TSP2322"]]
             (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
          (pr-str (:diagnostics r)))
      (is (= ["(s/coll-of string? :kind vector? :min-count 1 :max-count 1)"
              "(s/coll-of string? :kind vector? :min-count 2 :max-count 3)"]
             (mapv #(main/type-at r "length.clj" % 7) [8 9])))
      (is (= [":empty" ":in-range"] (mapv #(at r %) [19 20]))))))

(deftest bounds-from-spec-cross-namespace-boundaries
  (let [schema (parse/parse-string "schema.clj"
                 "(ns schema (:require [clojure.spec.alpha :as s]))
(s/def ::labels (s/coll-of string? :kind vector? :min-count 1 :max-count 2))")
        api (parse/parse-string "api.clj"
              "(ns api (:require [clojure.spec.alpha :as s] [schema :as schema]))
(s/fdef first-label :args (s/cat :xs ::schema/labels) :ret string?)
(defn first-label [xs] (first xs))
(s/fdef forward :args (s/cat :xs vector?) :ret string?)
(defn forward [xs] (first-label xs))")
        client (parse/parse-string "client.clj"
                 "(ns client (:require [api :as api]))
(api/first-label [])
(api/first-label [\"a\"])
(api/first-label [\"a\" \"b\"])
(api/first-label [\"a\" \"b\" \"c\"])
(api/first-label [nil])")]
    (doseq [opts [{} {:strict-nil true}]]
      (let [r (check/check-files [client api schema] opts)
            ds (:diagnostics r)]
        (is (= [["api.clj" 5 "TSP210-01"] ["client.clj" 2 "TSP330-01"]
                ["client.clj" 5 "TSP330-01"] ["client.clj" 6 "TSP2345"]]
               (mapv (juxt :file (comp :row :pos) :code) ds)) (pr-str ds))
        (is (= #{:collection-length} (:reasons (first ds))))
        (is (= ["string?" "string?"] (mapv #(main/type-at r "client.clj" % 1) [3 4])))))))

(deftest seq-proves-presence-on-both-the-result-and-the-original-value
  (doseq [opts [{} {:strict-nil true}]]
    (let [r (checked "(s/fdef label :args (s/cat :xs (s/coll-of string? :kind vector?)) :ret (s/nilable string?))
(defn label [xs]
  (if-let [s (seq xs)]
    (str (str/upper-case (first xs)) (str/upper-case (first s)))
    (first xs)))
(s/fdef captured :args (s/cat :xs (s/coll-of string? :kind vector?)) :ret (s/nilable string?))
(defn captured [xs]
  (let [items (seq xs)]
    (when items (str/upper-case (first xs)))))
(s/fdef maybe-nil :args (s/cat :xs (s/coll-of (s/nilable string?) :kind vector?)) :ret (s/nilable string?))
(defn maybe-nil [xs] (when-let [s (seq xs)] (str/upper-case (first s))))" opts)]
      (is (= [["TSP2531" 12]] (mapv (juxt :code (comp :row :pos)) (:diagnostics r)))
          (pr-str (:diagnostics r))))))

(deftest empty-predicate-refines-both-branches
  (doseq [opts [{} {:strict-nil true}]]
    (let [r (checked "(s/def ::names (s/coll-of string? :kind vector? :max-count 3))
(s/fdef label :args (s/cat :xs (s/nilable ::names)) :ret string?)
(defn label [xs] (if (empty? xs) \"none\" (first xs)))
(s/fdef head :args (s/cat :xs ::names) :ret (s/nilable string?))
(defn head [xs] (when-not (clojure.core/empty? xs) (first xs)))
(s/fdef captured :args (s/cat :xs ::names) :ret any?)
(defn captured [xs]
  (let [alias xs
        nothing? (empty? alias)]
    (if nothing?
      xs
      xs)))
(let [xs nil] (if (empty? xs) :empty (subs 42 0)))
(let [xs [:item]] (if (empty? xs) (subs 42 0) (first xs)))
(s/fdef nullable :args (s/cat :xs (s/coll-of (s/nilable string?) :kind vector? :max-count 2)) :ret string?)
(defn nullable [xs] (if (empty? xs) \"none\" (first xs)))
(s/fdef shadowed :args (s/cat :xs ::names) :ret string?)
(defn shadowed [xs]
  (let [empty? (fn [_] false)]
    (if (empty? xs) \"none\" (first xs))))
(s/fdef rebound :args (s/cat :xs ::names) :ret string?)
(defn rebound [xs]
  (with-redefs [clojure.core/empty? (fn [_] false)]
    (if (empty? xs) \"none\" (first xs))))
(s/fdef via-binding :args (s/cat :xs ::names) :ret string?)
(defn via-binding [xs] (if-let [nothing? (empty? xs)] \"none\" (first xs)))
(let [xs [:item]] (if-some [nothing? (empty? xs)] nothing? (subs 42 0)))" opts)]
      (is (= [[17 "TSP2322"] [20 "TSP2322"] [24 "TSP2322"]]
             (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
          (pr-str (:diagnostics r)))
      (is (= ["(s/coll-of string? :kind vector? :max-count 0)"
              "(s/coll-of string? :kind vector? :min-count 1 :max-count 3)"]
             (mapv #(main/type-at r "length.clj" % 7) [12 13])))
      (is (= [":empty" ":item"] (mapv #(at r %) [14 15]))))))

(deftest tails-and-destructuring-preserve-position-and-presence
  (let [r (checked "(seq [])
(first nil)
(first (seq [\"one\"]))
(first (rest [1]))
(next [1])
(first (next [1 2]))
(let [[_ & tail] [1]] tail)
(let [[_ & tail] [1 2]] (first tail))
(s/fdef after-first :args (s/cat :xs (s/coll-of string? :kind vector? :min-count 2)) :ret string?)
(defn after-first [xs] (first (rest xs)))
(s/fdef head :args (s/cat :xs (s/+ string?)) :ret string?)
(defn head [& xs] (first xs))")]
    (is (empty? (:diagnostics r)) (pr-str (:diagnostics r)))
    (is (= ["nil?" "nil?" "\"one\"" "nil?" "nil?" "2" "nil?" "2"]
           (mapv #(at r %) (range 2 10))))))

(deftest fixed-size-specs-drive-nth-get-in-and-apply
  (let [r (checked "(s/fdef second-label :args (s/cat :xs (s/coll-of string? :kind vector? :count 2)) :ret string?)
(defn second-label [xs] (nth xs 1 :missing))
(s/fdef via-path :args (s/cat :xs (s/coll-of string? :kind vector? :count 2)) :ret string?)
(defn via-path [xs] (get-in xs [1] :missing))
(s/fdef via-apply :args (s/cat :xs (s/coll-of int? :kind vector? :count 2)) :ret int?)
(defn via-apply [xs] (apply (fn [a b] (+ a b)) xs))
(s/fdef beyond :args (s/cat :xs (s/coll-of string? :kind vector? :max-count 2)) :ret any?)
(defn beyond [xs] (nth xs 2))
(nth [nil] 0 :absent)
(nth [1] 1 :absent)
(nth [1] \"0\")
(nth [1] 4294967297)")]
    (is (= ["TSP330-02" "TSP2345" "TSP330-02"] (codes r)) (pr-str (:diagnostics r)))
    (is (= ["nil?" ":absent"] (mapv #(at r %) [10 11])))))

(deftest branches-and-collection-operations-cannot-invent-a-minimum
  (let [r (checked "(s/fdef needs-one :args (s/cat :xs (s/coll-of int? :kind vector? :min-count 1)) :ret any?)
(defn needs-one [xs] xs)
(defn join [flag] (needs-one (if flag [1] [])))
(s/fdef capped :args (s/cat :xs (s/coll-of int? :kind vector? :max-count 1)) :ret any?)
(defn capped [xs] xs)
(s/fdef grow :args (s/cat :xs (s/coll-of int? :kind vector? :max-count 1)) :ret any?)
(defn grow [xs] (capped (conj xs 2)))
(s/fdef filtered :args (s/cat :xs (s/coll-of string? :kind vector? :min-count 1)) :ret string?)
(defn filtered [xs] (str/upper-case (first (filter identity xs))))")]
    (is (= ["TSP2345" "TSP330-01" "TSP2531"] (codes r)) (pr-str (:diagnostics r)))))

(deftest nth-selects-finite-indices-from-spec-and-control-flow
  (doseq [opts [{} {:strict-nil true}]]
    (let [r (checked "(s/def ::positions #{0 2})
(s/def ::parts (s/tuple string? keyword? string?))
(s/fdef text :args (s/cat :xs ::parts :i ::positions) :ret string?)
(defn text [xs i] (nth xs i :missing))
(s/fdef computed :args (s/cat :flag boolean?) :ret string?)
(defn computed [flag] (nth [\"a\" :tag \"b\"] (if flag 0 2)))
(s/fdef fallback :args (s/cat :i #{0 4}) :ret (s/nilable string?))
(defn fallback [i] (nth [nil :tag] i \"missing\"))
(s/fdef out-of-bounds :args (s/cat :i #{0 4}) :ret string?)
(defn out-of-bounds [i] (nth [\"hit\" :tag] i))
(s/fdef overflow :args (s/cat :i #{0 4294967297}) :ret string?)
(defn overflow [i] (nth [\"hit\" :tag] i \"missing\"))
(s/fdef maybe-present :args (s/cat :i #{0 2} :xs (s/coll-of string? :min-count 1 :max-count 4)) :ret string?)
(defn maybe-present [i xs] (nth xs i :missing))" opts)]
      (is (= [[11 "TSP330-02" "index 4 may be out of bounds for length 2"]
              [13 "TSP330-02" "index 4294967297 cannot be represented as a JVM int"]
              [15 "TSP2322" "returns (t/or string? :missing), but the signature says string?"]]
             (mapv (juxt (comp :row :pos) :code :message) (:diagnostics r)))
          (pr-str (:diagnostics r))))))

(deftest first-on-an-absent-collection-propagates-nil
  (let [r (checked "(defn docker-result [success?]
                        (if success? {:output [\"0.0.0.0:5432\"]} {:error :failed}))
                      (defn port [success?]
                        (if-let [p (-> (docker-result success?) :output first (str/split #\":\") last)]
                          (Integer/parseInt p)
                          nil))" {})]
    (is (= ["TSP2531"] (codes r)) (pr-str (:diagnostics r)))))

(deftest count-round-trips-and-unguarded-upper-bounds-are-not-assumed
  (let [ctx {:fail #(throw (ex-info "unexpected type syntax" {:type %}))}
        vector-type (t/parse-type ctx '(s/coll-of int? :kind vector? :count 2))
        seq-type (t/parse-type ctx '(s/coll-of int? :kind seq? :count 2))]
    (is (= [2 2] (length/bounds vector-type)))
    (is (= vector-type (t/parse-type ctx (read-string (t/show vector-type)))))
    (is (= [2 nil] (length/bounds seq-type)))
    (is (s/valid? (s/coll-of int? :kind seq? :count 2) (map identity [1 2 3])))
    (is (not (s/valid? (s/coll-of int? :kind vector? :count 2) [1 2 3]))))
  (let [r (checked "(s/def ::dynamic (s/coll-of int? :min-count n))
(s/def ::inconsistent (s/coll-of int? :min-count 3 :max-count 2))
(s/fdef only-empty :args (s/cat :xs (s/coll-of int? :kind vector? :max-count 0)) :ret nil?)
(defn only-empty [xs] (first xs))
(only-empty [])")]
    (is (= ["TSP9002" "TSP9002"] (codes r)) (pr-str (:diagnostics r)))))

(deftest recursive-collections-need-a-finite-base
  (let [r (checked "(s/def ::nest (s/coll-of ::nest :kind vector? :min-count 1))")]
    (is (= ["TSP295-02"] (codes r)) (pr-str (:diagnostics r)))))

(deftest generic-element-inference-keeps-the-collection-bound
  (let [r (checked "(defn head {:t/sig '(t/all [a] [(s/coll-of a :kind vector? :min-count 1) :-> a])}
                       [xs] (first xs))
(str/upper-case (head [\"valid\"]))
(head [])")]
    (is (= 1 (count (:diagnostics r))) (pr-str (:diagnostics r)))
    (is (= 5 (get-in r [:diagnostics 0 :pos :row])))))
