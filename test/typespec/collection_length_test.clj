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
    (is (= ["[B" "[Z" "[Ljava.lang.Object;" "int?"] (mapv #(at r %) [2 3 4 5])))
    (is (= [[6 "TSP2345"] [7 "TSP2345"] [8 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))))

(deftest into-array-uses-the-explicit-component-class
  (let [r (checked "(into-array String [\"label\" nil])
(alength (into-array java.io.File nil))
(into-array String [42])
(into-array String true)
(let [String (class 42)] (into-array String [42]))")]
    (is (= ["[Ljava.lang.String;" "int?" "t/unknown"] (mapv #(at r %) [2 3 6])))
    (is (= [[4 "TSP2345"] [5 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
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

(deftest alength-checks-array-hints-and-returns-int
  (let [r (checked "(let [^bytes body (byte-array 3)] (alength body))
(let [^objects values (object-array 2)] (alength values))
(alength [:not-an-array])
(alength nil)
(alength)
(defn unchecked [values] (alength values))" {})]
    (is (= ["int?" "int?"] (mapv #(at r %) [2 3])))
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
