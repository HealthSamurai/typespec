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

(deftest into-contextualizes-direct-map-and-filter-transducers
  (let [forms ["(into [] (map str) [1 2])"
               "(into [:seed] (clojure.core/map #(str/upper-case %)) [\"abc\"])"
               "(into #{} (filter string?) [\"abc\" 42])"
               "(into [] (filter #(when (string? %) %)) [\"abc\" 42])"
               "(into [] (filter (fn [_] false)) [1 2])"
               "(into [] (map :name) [])"
               "(into [] (map (fn [{:keys [name]}] (str name))) nil)"]
        result (check-source (str/join "\n" forms))
        start (inc (count (str/split-lines spec-source)))]
    (is (empty? (codes result)) (pr-str (:diagnostics result)))
    (is (= ["(s/coll-of string? :kind vector?)"
            "(s/coll-of (t/or keyword? string?) :kind vector?)"
            "(s/coll-of string? :kind set?)"
            "(s/coll-of string? :kind vector?)"
            "(s/coll-of t/never :kind vector?)"
            "(s/coll-of t/never :kind vector?)"
            "(s/coll-of t/never :kind vector?)"]
           (mapv #(main/type-at result "callbacks.clj" % 1) (range start (+ start (count forms))))))))

(deftest into-reports-incorrect-callbacks-and-keeps-opaque-transducers-incomplete
  (let [result (check-source "(into [] (map inc) [\"bad\"])
(into [] (filter (fn [x y] true)) [1])
(into [] (map #(inc %)) [\"bad\"])
(into [] (map str) 42)
(into [] identity [1])
(let [map (fn [f] identity)] (into [] (map inc) [1]))")]
    (is (= ["TSP2345" "TSP2554" "TSP2345" "TSP2345" "TSP210-01" "TSP210-01"]
           (codes result)) (pr-str (:diagnostics result)))
    (is (= "(s/coll-of t/unknown :kind vector?)" (main/type-at result "callbacks.clj" 10 1)))
    (is (every? #(= #{:unsupported-function} (:reasons %)) (take-last 2 (:diagnostics result)))))
  (let [r (check-source "(into [] identity [1])" {:strict true})]
    (is (= [["TSP210-01" :error]] (mapv (juxt :code :severity) (:diagnostics r)))))
  (let [r (check-source "(into [] (map (fn [n] (str #_(t/any \"fixture\") n))) [1])")]
    (is (= {:cast 0 :ignore 0 :any 1} (:trust r)))))

(deftest into-contextualizes-a-transducer-from-the-source-spec
  (let [result (check-source "(s/def ::entry (s/keys :req-un [::name]))
(s/def ::name string?)
(s/fdef names :args (s/cat :xs (s/coll-of ::entry)) :ret (s/coll-of string? :kind vector?))
(defn names [xs]
  (into [] (map (fn [{:keys [name]}] (str/upper-case name))) xs))
(s/fdef invalid-names :args (s/cat :xs (s/coll-of ::entry)) :ret vector?)
(defn invalid-names [xs]
  (into [] (map (fn [{:keys [name]}] (inc name))) xs))")]
    (is (= ["TSP2345"] (codes result)) (pr-str (:diagnostics result)))
    (is (= "(s/coll-of string? :kind vector?)" (main/type-at result "callbacks.clj" 9 3)))))

(deftest into-checks-map-indexed-transducers
  (let [result (check-source "(into [] (map-indexed (fn [i v] (str (inc i) (str/upper-case v)))) [\"a\"])
(into [] (comp (filter string?) (clojure.core/map-indexed #(str %1 (str/upper-case %2)))) [42 \"a\"])
(into [:seed] (map-indexed (fn [_] :unused)) [])
(into [] (map-indexed :item) [\"a\"])
(into [] (map-indexed (fn [i v] (subs i 1))) [\"a\"])
(into [] (map-indexed (fn [i] i)) [1])
(into [] (map-indexed #{0 1}) [1])
(into [] (map-indexed [0 1]) [1])
(let [map-indexed (fn [_] identity)] (into [] (map-indexed identity) [1]))
(second (into [:seed] (map-indexed (fn [_] :unused)) []))")
        ds (:diagnostics result)]
    (is (= [[9 "TSP2345"] [10 "TSP2554"] [11 "TSP2554"] [12 "TSP2554"] [13 "TSP210-01"]]
           (mapv (juxt (comp :row :pos) :code) ds))
        (pr-str ds))
    (is (= ["(s/coll-of string? :kind vector?)"
            "(s/coll-of string? :kind vector?)"
            "(s/coll-of keyword? :kind vector?)"
            "(s/coll-of string? :kind vector?)"]
           (mapv #(main/type-at result "callbacks.clj" % 1) (range 5 9))))
    (is (= "nil?" (main/type-at result "callbacks.clj" 14 1)))))

(deftest into-map-indexed-checks-a-spec-from-another-namespace
  (let [api (parse/parse-string
             "labels.clj"
             "(ns sample.labels (:require [clojure.spec.alpha :as s]))
(s/fdef label :args (s/cat :i int? :value string?) :ret string?)
(defn label [i value] (str i \":\" (subs value 0)))")
        client (parse/parse-string
                "client.clj"
                "(ns sample.client (:require [sample.labels :as labels] [clojure.spec.alpha :as s]))
(s/fdef render :args (s/cat :xs (s/coll-of string?)) :ret (s/coll-of string? :kind vector?))
(defn render [xs] (into [] (map-indexed labels/label) xs))
(into [] (comp (filter string?) (map-indexed labels/label)) [42 \"a\"])
(into [] (map-indexed labels/label) [42])")
        ds (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 5 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) ds))
        (pr-str ds))))

(deftest into-keep-transducers-drop-only-nil
  (let [result (check-source "(into [] (keep (fn [_] false)) [1])
(into [:seed] (keep (fn [_] nil)) [1])
(into [] (keep-indexed (fn [i v] (when (string? v) (str i v)))) [42 \"a\"])
(into [] (keep-indexed (fn [i _] (subs i 0))) [1])
(into [:seed] (keep-indexed (fn [_] :unused)) [])
(into [] (comp (keep (fn [v] (when (string? v) v))) (map str/upper-case)) [42 \"a\"])"
                             {:strict-nil true})
        ds (:diagnostics result)]
    (is (= [[8 "TSP2345"]] (mapv (juxt (comp :row :pos) :code) ds)) (pr-str ds))
    (is (= ["(s/coll-of boolean? :kind vector?)"
            "(s/coll-of keyword? :kind vector?)"
            "(s/coll-of string? :kind vector?)"
            "(s/coll-of keyword? :kind vector?)"
            "(s/coll-of string? :kind vector?)"]
           (mapv #(main/type-at result "callbacks.clj" % 1) [5 6 7 9 10])))))

(deftest into-keep-transducers-use-specs-from-another-namespace
  (let [api (parse/parse-string
              "labels.clj"
              "(ns labels (:require [clojure.spec.alpha :as s]))
(s/fdef indexed :args (s/cat :i int? :value string?) :ret (s/nilable string?))
(defn indexed [i value] (when i (subs value 0)))")
        client (parse/parse-string
                 "client.clj"
                 "(ns client (:require [labels :as l] [clojure.string :as str]))
(into [] (comp (keep-indexed l/indexed) (map str/upper-case)) [\"a\"])
(into [] (keep-indexed l/indexed) [42])")
        ds (:diagnostics (check/check-files [api client] {:strict-nil true}))]
    (is (= [["client.clj" 3 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) ds))
        (pr-str ds))))

(deftest into-mapcat-checks-the-callback-and-flattens-its-result
  (let [result (check-source "(into [] (comp (mapcat (fn [v] [v])) (map str/upper-case)) [\"a\"])
(into [] (mapcat (fn [_] 42)) [\"a\"])
(into [] (mapcat (fn [s] s)) [\"abc\"])
(into [:seed] (mapcat (fn [_] nil)) [1])
(second (into [:seed] (mapcat (fn [_ _] :unused)) []))
(into [] (mapcat (fn [_] (eduction (map inc) [1 2]))) [1])")
        ds (:diagnostics result)]
    (is (= [[6 "TSP2345" "mapcat cannot reduce callback result 42"]]
           (mapv (juxt (comp :row :pos) :code :message) ds))
        (pr-str ds))
    (is (= ["(s/coll-of string? :kind vector?)"
            "(s/coll-of java.lang.Character :kind vector?)"
            "(s/coll-of keyword? :kind vector?)"
            "nil?"]
           (mapv #(main/type-at result "callbacks.clj" % 1) [5 7 8 9])))))

(deftest into-mapcat-uses-a-callback-contract-from-another-namespace
  (let [api (parse/parse-string
             "parts.clj"
             "(ns parts (:require [clojure.spec.alpha :as s] [clojure.string :as str]))
(s/fdef words :args (s/cat :text string?) :ret (s/nilable (s/coll-of string? :kind vector?)))
(defn words [text] (when-not (str/blank? text) (str/split text #\":\")))")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [parts :as p] [clojure.string :as str]))
(into [] (comp (mapcat p/words) (map str/upper-case)) [\"a:b\" \"\"])
(into [] (mapcat p/words) [42])")
        ds (:diagnostics (check/check-files [api client] {:strict-nil true}))]
    (is (= [["client.clj" 3 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) ds))
        (pr-str ds))))

(deftest into-cat-flattens-one-level-and-checks-input
  (let [result (check-source "(into [] cat [[1 2] nil [3]])
(into [] (comp (map (fn [v] [v])) clojure.core/cat (map str/upper-case)) [\"a\"])
(into [] cat [\"abc\"])
(second (into [:seed] cat []))
(into [] cat [42])
(let [cat identity] (into [] cat [[1]]))")
        ds (:diagnostics result)]
    (is (= [[9 "TSP2345"] [10 "TSP210-01"]]
           (mapv (juxt (comp :row :pos) :code) ds))
        (pr-str ds))
    (is (= "cat cannot reduce input int?" (:message (first ds))))
    (is (= ["(s/coll-of int? :kind vector?)"
            "(s/coll-of string? :kind vector?)"
            "(s/coll-of java.lang.Character :kind vector?)"
            "nil?"]
           (mapv #(main/type-at result "callbacks.clj" % 1) [5 6 7 8])))))

(deftest into-cat-preserves-types-from-a-callback-in-another-namespace
  (let [api (parse/parse-string
             "parts.clj"
             "(ns parts (:require [clojure.spec.alpha :as s]))
(s/fdef words :args (s/cat :text string?) :ret (s/coll-of string? :kind vector?))
(defn words [text] [text])")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [parts :as p] [clojure.core :as c] [clojure.string :as str]))
(into [] (comp (map p/words) c/cat (map str/upper-case)) [\"a\"])
(into [] (comp (map p/words) c/cat (map inc)) [\"a\"])")
        ds (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 3 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) ds))
        (pr-str ds))))

(deftest into-take-and-drop-preserve-types-and-check-the-count
  (let [result (check-source "(into [] (comp (drop 1) (take 2) (map str/upper-case)) [\"a\" \"b\" \"c\"])
(second (into [:seed] (comp (take 0) (map (fn [a b] :unused)) (filter (fn [] true))) [1]))
(second (into [:seed] (take -1) [1]))
(into [] (take 1.5) [1 2 3])
(into [] (drop \"bad\") [1])
(into [] (take nil) [1])
(into [:seed] (take \"unused\") [])
(into [] (map #(inc %)) [])
(let [take (fn [_] identity)] (into [] (take 1) [1]))")
        ds (:diagnostics result)]
    (is (= [[9 "TSP2345"] [10 "TSP2345"] [13 "TSP210-01"]]
           (mapv (juxt (comp :row :pos) :code) ds))
        (pr-str ds))
    (is (= ["(s/coll-of string? :kind vector?)"
            "nil?"
            "nil?"
            "(s/coll-of int? :kind vector?)"
            "(s/coll-of keyword? :kind vector?)"
            "(s/coll-of t/never :kind vector?)"]
           (mapv #(main/type-at result "callbacks.clj" % 1) [5 6 7 8 11 12])))))

(deftest into-take-and-drop-preserve-source-contracts-from-another-namespace
  (let [source (parse/parse-string
                "labels.clj"
                "(ns labels (:require [clojure.spec.alpha :as s]))
(s/fdef labels :args (s/cat) :ret (s/coll-of string? :kind vector?))
(defn labels [] [\"a\" \"b\"])")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [labels :as labels] [clojure.spec.alpha :as s] [clojure.string :as str]))
(s/fdef render :args (s/cat) :ret (s/coll-of string? :kind vector?))
(defn render [] (into [] (comp (drop 1) (clojure.core/take 2) (map str/upper-case)) (labels/labels)))
(into [] (comp (drop 1) (take 2) (map inc)) (labels/labels))")
        ds (:diagnostics (check/check-files [source client] {}))]
    (is (= [["client.clj" 4 "TSP2345" "inc takes number?, but gets string?"]]
           (mapv (juxt :file (comp :row :pos) :code :message) ds))
        (pr-str ds))))

(deftest into-predicate-transducers-select-the-emitted-elements
  (let [result (check-source "(into [] (comp (remove nil?) (map inc)) [nil 1 2])
(into [] (comp (take-while string?) (map str/upper-case)) [\"a\" 1 \"b\"])
(into [] (drop-while string?) [\"a\" 1 \"b\"])
(second (into [:seed] (drop-while string?) [\"a\" \"b\"]))
(second (into [:seed] (remove (fn [_] :skip)) [1]))
(second (into [:seed] (take-while (fn [_] nil)) [1]))
(into [] (remove nil?) [nil false])
(into [] (comp (drop-while string?) (map inc)) [\"a\" 1 \"b\"])
(into [] (take-while (fn [_ _] true)) [1])
(into [:seed] (drop-while (fn [_ _] true)) [])
(let [remove (fn [_] identity)] (into [] (remove string?) [1]))"
                             {:strict-nil true})
        ds (:diagnostics result)]
    (is (= [[12 "TSP2345"] [13 "TSP2554"] [15 "TSP210-01"]]
           (mapv (juxt (comp :row :pos) :code) ds))
        (pr-str ds))
    (is (= ["(s/coll-of int? :kind vector?)"
            "(s/coll-of string? :kind vector?)"
            "(s/coll-of (t/or string? int?) :kind vector?)"
            "nil?"
            "nil?"
            "nil?"
            "(s/coll-of boolean? :kind vector?)"
            "(s/coll-of keyword? :kind vector?)"]
           (mapv #(main/type-at result "callbacks.clj" % 1) [5 6 7 8 9 10 11 14])))))

(deftest into-predicate-transducers-use-guards-from-another-namespace
  (let [api (parse/parse-string
             "parts.clj"
             "(ns parts (:require [clojure.spec.alpha :as s]))
(s/fdef numeric? :args (s/cat :value any?) :ret boolean?)
(defn numeric? [value] (number? value))")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [parts :as p] [clojure.spec.alpha :as s] [clojure.string :as str]))
(s/fdef upper :args (s/cat) :ret (s/coll-of string? :kind vector?))
(defn upper [] (into [] (comp (remove p/numeric?) (map str/upper-case)) [\"a\" 1 \"b\"]))
(into [] (comp (take-while p/numeric?) (map inc)) [1 \"a\" 2])
(into [] (comp (remove p/numeric?) (map inc)) [1 \"a\"])")
        ds (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 5 "TSP2345" "inc takes number?, but gets string?"]]
           (mapv (juxt :file (comp :row :pos) :code :message) ds))
        (pr-str ds))))

(deftest into-partition-all-emits-nonempty-vectors-and-checks-capacity
  (let [result (check-source "(into [] (partition-all 2) [1 2 3])
(into [] (comp (take 1) (partition-all 2) (map #(first %))) [1 2 3])
(into [] (partition-all 0) [1 2 3])
(into [] (partition-all 1.9) [1 2 3])
(into [] (partition-all -0.5) [1 2 3])
(second (into [:seed] (comp (partition-all 2) (map (fn [_ _] :unused))) []))
(second (into [:seed] (comp (take 0) (partition-all 2)) [1]))
(into [] (partition-all -1) [])
(into [] (partition-all 2147483648) [])
(into [] (partition-all \"bad\") [])
(into [] (comp (partition-all 2) (map #(nth % 2))) [1 2 3])
(let [partition-all (fn [_] identity)] (into [] (partition-all 2) [1]))"
                             {:strict-nil true})
        ds (:diagnostics result)]
    (is (= [[12 "TSP2345"] [13 "TSP2345"] [14 "TSP2345"] [15 "TSP330-02"] [16 "TSP210-01"]]
           (mapv (juxt (comp :row :pos) :code) ds))
        (pr-str ds))
    (is (= ["(s/coll-of (s/coll-of int? :kind vector? :min-count 1 :max-count 2) :kind vector?)"
            "(s/coll-of int? :kind vector?)"
            "(s/coll-of (s/coll-of int? :kind vector? :min-count 1) :kind vector?)"
            "(s/coll-of (s/coll-of int? :kind vector? :min-count 1 :max-count 1) :kind vector?)"
            "(s/coll-of (s/coll-of int? :kind vector? :min-count 1) :kind vector?)"
            "nil?"
            "nil?"]
           (mapv #(main/type-at result "callbacks.clj" % 1) (range 5 12))))))

(deftest into-partition-all-preserves-types-from-another-namespace
  (let [api (parse/parse-string
             "numbers.clj"
             "(ns numbers (:require [clojure.spec.alpha :as s]))
(s/fdef numbers :args (s/cat) :ret (s/coll-of int? :kind vector?))
(defn numbers [] [1 2 3])")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [numbers :as n] [clojure.spec.alpha :as s]))
(s/def ::chunk (s/coll-of int? :kind vector? :min-count 1 :max-count 2))
(s/fdef groups :args (s/cat) :ret (s/coll-of ::chunk :kind vector?))
(defn groups [] (into [] (clojure.core/partition-all 2) (n/numbers)))
(s/fdef heads :args (s/cat :size number?) :ret (s/coll-of int? :kind vector?))
(defn heads [size] (into [] (comp (partition-all size) (map #(first %))) (n/numbers)))
(into [] (comp (partition-all 2) (map inc)) (n/numbers))")
        ds (:diagnostics (check/check-files [api client] {:strict-nil true}))]
    (is (= [["client.clj" 7 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) ds))
        (pr-str ds))))

(deftest into-composes-transducers-in-element-order
  (let [forms ["(into [] (comp (map str) (map str/upper-case)) [1 2])"
               "(into #{} (comp (filter string?) (map count)) [1 \"name\"])"
               "(into [] (clojure.core/comp (comp (map str)) (filter string?) (map count)) [1])"
               "(into [] (comp) [1 2])"
               "(into [42] (comp (filter (fn [_] false)) (map str)) [1])"
               "(into [] (comp (map :name) (map str)) [])"]
        result (check-source (str/join "\n" forms))
        start (inc (count (str/split-lines spec-source)))]
    (is (empty? (codes result)) (pr-str (:diagnostics result)))
    (is (= ["(s/coll-of string? :kind vector?)"
            "(s/coll-of int? :kind set?)"
            "(s/coll-of int? :kind vector?)"
            "(s/coll-of int? :kind vector?)"
            "(s/coll-of int? :kind vector?)"
            "(s/coll-of t/never :kind vector?)"]
           (mapv #(main/type-at result "callbacks.clj" % 1) (range start (+ start (count forms))))))))

(deftest into-reports-failing-transducer-links-and-opaque-stages
  (let [forms ["(into [] (comp (map str/upper-case) (map str)) [1])"
               "(into [] (comp (map str) (map inc)) [1])"
               "(into [] (comp (map inc) identity (filter (fn [_] false))) [\"bad\"])"
               "(into [] (comp (filter (fn [_] false)) identity) [])"
               "(let [comp (fn [& stages] identity)] (into [] (comp (map str) (map inc)) [1]))"
               "(into [] (comp identity (map (fn [_] (inc \"bad\")))) [1])"]
        result (check-source (str/join "\n" forms))
        start (inc (count (str/split-lines spec-source)))
        diagnostics (:diagnostics result)]
    (is (= ["TSP2345" "TSP2345" "TSP2345" "TSP210-01" "TSP210-01" "TSP210-01" "TSP210-01" "TSP2345"]
           (codes result)) (pr-str diagnostics))
    (is (= (mapv #(+ start %) [0 1 2 2 3 4 5 5])
           (mapv (comp :row :pos) diagnostics)))
    (is (= (inc (str/index-of (second forms) "inc")) (get-in diagnostics [1 :pos :col])))
    (is (= (inc (str/index-of (nth forms 2) "identity")) (get-in diagnostics [3 :pos :col])))
    (is (= "(s/coll-of t/unknown :kind vector?)" (main/type-at result "callbacks.clj" (+ start 3) 1))))
  (let [result (check-source "(into [] (comp (map #(str #_(t/any \"fixture\") %)) (map count)) [1])")]
    (is (empty? (codes result)) (pr-str (:diagnostics result)))
    (is (= {:cast 0 :ignore 0 :any 1} (:trust result)))))

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

(deftest recursive-callback-calls-use-the-implemented-arities
  (let [res (check-source
             "(map (fn node [x & [option]] (if option (+ x option) (node x 1))) [1 2])
(map (fn node [x & [option]] (if option x (apply node [x 1]))) [1 2])
(map (fn node ([x] (node x 1)) ([x y] (str x y))) [1])
(use-name (fn node [name & [option]] (if (str/blank? name) (node \"Ada\") name)))
(use-name (fn node [name & [option]] (if (str/blank? name) (use-name node) name)))
(map (fn node [x & [option]] (node)) [1 2])
(map (fn node [x] (node x 1)) [1 2])
(map (fn node [x & [option]] (apply node [])) [1 2])
(map (fn node [^String x & [option]] (if option x (node 42 :opt))) [\"a\"])")
        ds (:diagnostics res)]
    (is (= [[10 "TSP2554"] [11 "TSP2554"] [12 "TSP2554"] [13 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) ds))
        (pr-str ds))
    (is (= "node expects 1+ argument(s), got 0" (get-in ds [0 :message])))))

(deftest recursive-callback-uses-context-from-another-namespace
  (let [api (parse/parse-string
             "api.clj"
             "(ns visitors.api (:require [clojure.spec.alpha :as s]))
(s/def ::visitor (s/fspec :args (s/cat :name string?) :ret string?))
(s/fdef visit :args (s/cat :f ::visitor) :ret string?)
(defn visit [f] (f \"Ada\"))")
        client (parse/parse-string
                "client.clj"
                "(ns visitors.client (:require [visitors.api :as api] [clojure.string :as str]))
(api/visit (fn step [^String name & [option]]
  (if option name (str (step name :present)))))
(api/visit (fn step [^String name & [option]]
  (if option name (str (step 42 :present)))))
(api/visit (fn step [name & [option]] (str/upper-case name)))")
        ds (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 5 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) ds))
        (pr-str ds))))

(deftest map-indexed-infers-the-callback-and-lazy-result
  (let [result (check-source "(clojure.core/map-indexed #(str (inc %1) (str/upper-case %2)) [\"abc\"])
(->> [\"abc\"] (map-indexed #(str %1 %2)) (into []))
(map-indexed (fn [i x] (str i x)) nil)
(map-indexed (fn [i x] (str i x)) [])
(let [map-indexed str] (map-indexed :left :right))
(first (map-indexed str []))
(s/def ::entry (s/tuple string? int?))
(s/fdef labels :args (s/cat :xs (s/coll-of ::entry :min-count 1 :max-count 3))
  :ret (s/coll-of string? :kind seq? :min-count 1 :max-count 3))
(defn labels [xs] (map-indexed (fn [i [label n]] (str (+ i n) (str/upper-case label))) xs))")]
    (is (empty? (codes result)) (pr-str (:diagnostics result)))
    (is (= ["(s/coll-of string? :kind seq? :min-count 1 :max-count 1)"
            "(s/coll-of string? :kind vector? :min-count 1 :max-count 1)"
            "(s/coll-of t/never :kind seq? :max-count 0)"
            "(s/coll-of t/never :kind seq? :max-count 0)"
            "string?"
            "nil?"]
           (mapv #(main/type-at result "callbacks.clj" % 1) (range 5 11))))))

(deftest map-indexed-checks-callbacks-and-arity
  (let [result (check-source "(map-indexed (fn [i x] (subs i 1)) [\"abc\"])
(map-indexed (fn [i x] (inc x)) [\"abc\"])
(map-indexed (fn [i] i) [\"abc\"])
(map-indexed (fn [i x] x) 42)
(map-indexed)
(map-indexed str [1] [2])
(map-indexed (fn [i x] (str i x)))")]
    (is (= [[5 "TSP2345"] [6 "TSP2345"] [7 "TSP2554"] [8 "TSP2345"] [9 "TSP2554"] [10 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))
    (is (= "t/unknown" (main/type-at result "callbacks.clj" 11 1)))))

(deftest map-indexed-resolves-the-callback-spec-across-namespaces
  (let [api (parse/parse-string "indexed/api.clj"
                               "(ns indexed.api (:require [clojure.spec.alpha :as s]))
                                (s/fdef label :args (s/cat :index int? :item string?) :ret string?)
                                (defn label [i item] (str i item))")
        caller (parse/parse-string "indexed/caller.clj"
                                  "(ns indexed.caller (:require [clojure.spec.alpha :as s] [indexed.api :as api]))
(s/fdef good :args (s/cat :xs (s/coll-of string?)) :ret (s/coll-of string? :kind seq?))
(defn good [xs] (map-indexed api/label xs))
(s/fdef bad :args (s/cat :xs (s/coll-of int?)) :ret (s/coll-of string? :kind seq?))
(defn bad [xs] (map-indexed api/label xs))")
        result (check/check-files [caller api] {})]
    (is (= [["indexed/caller.clj" 5 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))))

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

(deftest reduce-kv-checks-reducer-spec-across-namespaces
  (let [api (parse/parse-string "fold/api.clj"
                               "(ns fold.api (:require [clojure.spec.alpha :as s]))
                                (s/def ::acc (s/coll-of int? :kind vector?))
                                (s/fdef step :args (s/cat :acc ::acc :key string? :value int?) :ret ::acc)
                                (defn step [acc _ value] (conj acc value))")
        caller (parse/parse-string "fold/caller.clj"
                                  "(ns fold.caller (:require [clojure.spec.alpha :as s] [fold.api :as api]))
(s/fdef good :args (s/cat :m (s/map-of string? int?)) :ret ::api/acc)
(defn good [m] (reduce-kv api/step [] m))
(s/fdef wrong-value :args (s/cat :m (s/map-of string? string?)) :ret ::api/acc)
(defn wrong-value [m] (reduce-kv api/step [] m))
(s/fdef wrong-key :args (s/cat :m (s/map-of keyword? int?)) :ret ::api/acc)
(defn wrong-key [m] (reduce-kv api/step [] m))
(defn- wrong-seed [] (reduce-kv api/step [\"bad\"] {\"a\" 1}))")
        result (check/check-files [caller api] {})]
    (is (= [["fold/caller.clj" 5 "TSP2345"]
            ["fold/caller.clj" 7 "TSP2345"]
            ["fold/caller.clj" 8 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))))

(deftest reduce-kv-contextualizes-key-and-value
  (let [result (check-source "(clojure.core/reduce-kv (fn [acc k v] (conj acc (str (str/upper-case k) (inc v)))) [] {\"a\" 1 \"b\" 2})
(reduce-kv #(+ %1 %2 %3) 0 [1 2])
(reduce-kv (fn [acc _ {:keys [n]}] (+ acc n)) 0 {:a {:n 2}})
(reduce-kv (fn [acc k v] (inc k)) 0 {:a 1})
(reduce-kv (fn [acc k v] (inc v)) 0 {\"a\" \"bad\"})
(reduce-kv (fn [acc _] acc) 0 {:a 1})
(reduce-kv (fn [acc k v] acc) 0)
(s/fdef sum-indices :args (s/cat :v vector?) :ret int?)
(defn sum-indices [v] (reduce-kv (fn [acc i _] (+ acc i)) 0 v))")]
    (is (= [[8 "TSP2345"] [9 "TSP2345"] [10 "TSP2554"] [11 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))
    (is (= ["(s/coll-of string? :kind vector? :min-count 1)" "int?" "int?"]
           (mapv #(main/type-at result "callbacks.clj" % 1) [5 6 7])))))

(deftest reduce-kv-checks-the-accumulator-only-when-stepping
  (let [result (check-source "(let [m {}] (reduce-kv (fn [acc k] \"unused\") :seed m))
(reduce-kv (fn [acc k] \"unused\") :seed nil)
(reduce-kv (fn [acc k v] \"changed\") 0 {:a 1})
(reduce-kv (fn [acc k v] (if (string? acc) (inc acc) \"changed\")) 0 {:a 1 :b 2})
(s/fdef maybe-fold :args (s/cat :m (s/nilable (s/map-of keyword? int?))) :ret (s/or :init keyword? :step string?))
(defn maybe-fold [m] (reduce-kv (fn [acc k v] \"changed\") :seed m))
(reduce-kv (fn [acc k v] acc) 0 (throw (Exception.)))")]
    (is (= [[8 "TSP2345"]] (mapv (juxt (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))
    (is (= [":seed" ":seed" "\"changed\"" "t/never"]
           (mapv #(main/type-at result "callbacks.clj" % 1) [5 6 7 11])))))
