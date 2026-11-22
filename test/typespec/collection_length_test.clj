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
