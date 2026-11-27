(ns typespec.tuples-and-rows-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.spec.alpha :as s]
            [typespec.check :as check]
            [typespec.main :as main]
            [typespec.parse :as parse]))

(defn- checked [source]
  (check/check-files [(parse/parse-string "sample.clj" (str "(ns sample (:require [clojure.spec.alpha :as s]))\n" source))]
                     {:strict-nil true}))

(defn- codes [result] (mapv :code (:diagnostics result)))
(defn- type-at [result row] (main/type-at result "sample.clj" row 1))

(deftest update-vals-preserves-required-fields-and-vector-kind
  (let [api (parse/parse-string "rows.clj" "(ns rows (:require [clojure.spec.alpha :as s]))
(s/def ::amount int?)
(s/fdef raw :args (s/cat) :ret (s/keys :req-un [::amount]))
(defn raw [] {:amount 2})")
        client (parse/parse-string "client.clj" "(ns client (:require [rows :as api] [clojure.string :as str]))
(str/upper-case (:amount (update-vals (api/raw) str)))
(inc (:amount (update-vals (api/raw) str)))
(first (update-vals [1 2] inc))
(:absent (update-vals nil 42) :default)
(update-vals {:amount \"bad\"} inc)
(update-vals 42 inc)
(update-vals {})")
        r (check/check-files [api client] {})]
    (is (= [[3 "TSP2345"] [6 "TSP2345"] [7 "TSP2345"] [8 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r))))
    (is (= ["int?" ":default"] (mapv #(main/type-at r "client.clj" % 1) [4 5]))))
  (is (= [2 3] (update-vals [1 2] inc)))
  (is (= {} (update-vals nil 42)))
  (is (thrown? ClassCastException (update-vals {:amount "bad"} inc))))

(deftest update-keys-checks-key-callbacks-and-preserves-value-types
  (let [r (checked "(inc (:target (update-keys {:source 2} (fn [_] :target))))
(:target (update-keys {:source 2} (fn [_] :target)))
(:target (update-keys nil 42) :default)
(get (update-keys [\"one\" \"two\"] str) \"0\" \"missing\")
(subs (:target (update-keys {:source 2} (fn [_] :target))) 0)
(update-keys {:source 2} inc)
(update-keys 42 str)
(update-keys {})")]
    (is (= ["int?" "2" ":default" "(t/or \"missing\" string?)"]
           (mapv #(type-at r %) [2 3 4 5])))
    (is (= [[6 "TSP2345"] [7 "TSP2345"] [8 "TSP2345"] [9 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (= {"0" "one" "1" "two"} (update-keys ["one" "two"] str)))
  (is (thrown? ClassCastException (update-keys {:source 2} inc))))

(deftest merge-with-checks-only-colliding-keys-and-preserves-other-fields
  (let [r (checked "(:n (merge-with + {:n 2 :other \"word\"} {:n 3}))
(:other (merge-with + {:n 2 :other \"word\"} {:n 3}))
(:value (merge-with 42 nil {:value \"word\"}))
(merge-with + nil nil)
(merge-with +)
(merge-with + {:n 2} {:n \"bad\"})
(merge-with 42 {:n 2} {:n 3})
(merge-with)
(merge-with + false)")]
    (is (= ["int?" "\"word\"" "\"word\"" "nil?" "nil?" "nil?"] (mapv #(type-at r %) [2 3 4 5 6 10])))
    (is (= [[7 "TSP2345"] [8 "TSP2345"] [9 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (= {:n 5 :other "word"} (merge-with + {:n 2 :other "word"} {:n 3})))
  (is (thrown? ClassCastException (merge-with + {:n 2} {:n "bad"}))))

(deftest rename-keys-reads-original-fields-and-preserves-swaps
  (let [r (checked "(get (clojure.set/rename-keys {:a 2 :b \"word\"} {:a :b :b :a}) :a)
(get (clojure.set/rename-keys {:a 2 :b \"word\"} {:a :b :b :a}) :b)
(:a (clojure.set/rename-keys {:a 2} {:a :b}) :default)
(clojure.set/rename-keys nil {:a :b})
(inc (:b (clojure.set/rename-keys {:a \"wrong\"} {:a :b})))
(clojure.set/rename-keys {})")]
    (is (= ["\"word\"" "2" ":default" "nil?"] (mapv #(type-at r %) [2 3 4 5])))
    (is (= [[6 "TSP2345"] [7 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (= {:a "word" :b 2} (clojure.set/rename-keys {:a 2 :b "word"} {:a :b :b :a}))))

(deftest map-invert-swaps-key-value-types-and-accounts-for-collisions
  (let [r (checked "(:ready (clojure.set/map-invert {:source :ready}))
(:ready (clojure.set/map-invert {:left :ready :right :ready}))
(:missing (clojure.set/map-invert nil) :default)
(name (:ready (clojure.set/map-invert {:source :ready})))
(inc (:ready (clojure.set/map-invert {:source :ready})))
(clojure.set/map-invert 42)
(clojure.set/map-invert)")]
    (is (= [":source" "#{:left :right}" ":default" "\"source\""] (mapv #(type-at r %) [2 3 4 5])))
    (is (= [[6 "TSP2345"] [7 "TSP2345"] [8 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (= {:ready :source} (clojure.set/map-invert {:source :ready})))
  (is (contains? #{:left :right} (:ready (clojure.set/map-invert {:left :ready :right :ready})))))

(deftest project-preserves-selected-field-types-and-set-cardinality
  (let [r (checked "(:n (first (clojure.set/project #{{:n 2 :other \"word\"}} [:n])))
(:other (first (clojure.set/project #{{:n 2 :other \"word\"}} [:n])) :default)
(bounded-count 0 (clojure.set/project nil [:n]))
(inc (:n (first (clojure.set/project #{{:n \"bad\"}} [:n]))))
(clojure.set/project 42 [:n])
(clojure.set/project #{})")]
    (is (= ["2" ":default" "0"] (mapv #(type-at r %) [2 3 4])))
    (is (= [[5 "TSP2345"] [6 "TSP2345"] [7 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (= #{{:n 2}} (clojure.set/project #{{:n 2 :other "word"}} [:n])))
  (is (= #{} (clojure.set/project nil [:n]))))

(deftest relation-rename-preserves-the-values-at-renamed-fields
  (let [r (checked "(:destination (first (clojure.set/rename #{{:source 2}} {:source :destination})))
(:source (first (clojure.set/rename #{{:source 2}} {:source :destination})) :default)
(bounded-count 0 (clojure.set/rename nil {:source :destination}))
(inc (:destination (first (clojure.set/rename #{{:source \"bad\"}} {:source :destination}))))
(clojure.set/rename 42 {:source :destination})
(clojure.set/rename #{})")]
    (is (= ["2" ":default" "0"] (mapv #(type-at r %) [2 3 4])))
    (is (= [[5 "TSP2345"] [6 "TSP2345"] [7 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (= #{{:destination 2}} (clojure.set/rename #{{:source 2}} {:source :destination})))
  (is (= #{} (clojure.set/rename nil {:source :destination}))))

(deftest set-select-checks-the-predicate-and-preserves-set-kind
  (let [r (checked "(bounded-count 0 (clojure.set/select (fn [_] true) #{1 2}))
(bounded-count 0 (clojure.set/select (fn [_] false) #{1 2}))
(clojure.set/select inc nil)
(clojure.set/select (fn [_] true) [\"word\"])
(clojure.set/select inc #{\"bad\"})
(clojure.set/select inc 42)
(clojure.set/select inc)")]
    (is (= ["2" "0" "nil?" "(s/coll-of string? :kind vector?)"]
           (mapv #(type-at r %) [2 3 4 5])))
    (is (= [[6 "TSP2345"] [7 "TSP2345"] [8 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))))
  (is (= #{1} (clojure.set/select number? #{1 :two})))
  (is (thrown? ClassCastException (clojure.set/select inc #{"bad"}))))

(deftest tuple-contracts-check-positions-and-length
  (let [r (checked "(s/fdef pair :args (s/cat :p (s/tuple int? string?)) :ret string?)
(defn pair [[id text]] (subs text id))
(pair [1 \"text\"])
(pair [\"text\" 1])
(pair [1])
(pair [1 \"text\" 2])
(defn forward [^clojure.lang.IPersistentVector v] (pair v))")]
    (is (= ["TSP2345" "TSP330-01" "TSP330-01" "TSP210-01"] (codes r)) (pr-str (:diagnostics r)))
    (is (= #{:tuple-length} (:reasons (last (:diagnostics r)))))
    (is (= "string?" (type-at r 4)))))

(deftest homogeneous-spec-checks-the-known-positions-before-their-widened-type
  (let [api (parse/parse-string "rows.clj" "(ns rows (:require [clojure.spec.alpha :as s]))
(s/def ::states (s/coll-of #{:ready :done} :kind vector?))
(s/fdef states :args (s/cat :values ::states) :ret ::states)
(defn states [values] values)
(s/fdef labels :args (s/cat) :ret (s/coll-of qualified-ident? :kind vector?))
(defn labels [] [:ready/code 'done/code])")
        client (parse/parse-string "client.clj" "(ns client (:require [rows :as rows] [clojure.spec.alpha :as s]))
(let [states [:ready :done]] (rows/states states))
(let [states [:ready :bad]] (rows/states states))
(s/fdef unexpected :args (s/cat) :ret (s/coll-of qualified-ident? :kind vector?))
(defn unexpected [] [:ready/code 'plain])
(mapv namespace (rows/labels))")
        result (check/check-files [api client] {})]
    (is (= [["client.clj" 3 "TSP2345"] ["client.clj" 5 "TSP2322"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))
    (is (= "(s/coll-of string? :kind vector?)" (main/type-at result "client.clj" 6 1))))
  (is (s/valid? (s/coll-of #{:ready :done} :kind vector?) [:ready :done]))
  (is (not (s/valid? (s/coll-of #{:ready :done} :kind vector?) [:ready :bad]))))

(deftest positions-distinguish-absence-nil-and-defaults
  (let [r (checked "(let [[id text & rest :as pair] [1 \"text\" nil]] (nth pair 1))
(first [nil 1])
(second [1])
(nth [nil] 0 :missing)
(nth [nil] 1 :missing)
(nth [nil] 1)
#_(t/ignore TSP330-02 \"Verifier fixture\") (nth [1] 2)
#_(t/ignore TSP330-02 \"Fixed\") (nth [1] 0)
(let [[id & tail] [1 \"text\"]] (first tail))")]
    (is (= ["TSP330-02" "TSP2578"] (codes r)) (pr-str (:diagnostics r)))
    (is (= {:cast 0 :any 0 :ignore 2} (:trust r)))
    (is (= ["\"text\"" "nil?" "nil?" "nil?" ":missing" "\"text\""]
           (mapv #(type-at r %) [2 3 4 5 6 10])))))

(deftest tuple-positions-bind-generic-parameters
  (let [r (checked "(defn text {:t/sig '(t/all [a] [(s/tuple int? a) :-> a])} [[_ value]] value)
(text [1 \"Ada\"])
(text [1 true])")]
    (is (empty? (codes r)) (pr-str (:diagnostics r)))
    (is (= ["\"Ada\"" "true"] (mapv #(type-at r %) [3 4])))))

(deftest named-map-transformations-preserve-caller-fields
  (let [r (checked "(defn normalize [m] (-> m (assoc :version 1) (dissoc :temp)))
(let [f normalize] (:name (f {:name \"Ada\" :version \"old\" :temp true})))
(:enabled (normalize {:enabled false}))
(:version (normalize {:version \"old\"}))
(:temp (normalize {:temp true}))
(defn remove-dynamic [m k] (dissoc m k))
(:name (remove-dynamic {:name \"Ada\"} :temp))")]
    (is (empty? (codes r)) (pr-str (:diagnostics r)))
    (is (= ["\"Ada\"" "false" "int?" "nil?" "t/unknown"]
           (mapv #(type-at r %) [3 4 5 6 8])))))

(deftest map-transformations-use-spec-across-namespaces
  (let [consumer "(ns client (:require [provider :as p]))
(:id (p/stamp {:app/name \"Ada\" :id 7}))
(p/stamp {:app/name 42})"
        provider "(ns provider (:require [clojure.spec.alpha :as s]))
(s/def :app/name string?)
(s/def ::required (s/keys :req [:app/name]))
(s/fdef stamp :args (s/cat :m ::required) :ret map?)
(defn stamp [m] (assoc m :app/name (clojure.string/upper-case (:app/name m))))"
        result (check/check-files [(parse/parse-string "client.clj" consumer)
                                   (parse/parse-string "provider.clj" provider)] {})]
    (is (= ["TSP2322"] (codes result)) (pr-str (:diagnostics result)))
    (is (= "7" (main/type-at result "client.clj" 2 1)))))

(deftest uncertain-var-roots-do-not-prove-a-map-transformation
  (let [r (checked "(defn stamp [m] (assoc m :version 1))
(with-redefs [stamp (fn [_] {})] (:id (stamp {:id 7})))
(defn ^:dynamic *stamp* [m] (assoc m :version 1))
(:id (*stamp* {:id 7}))")]
    (is (empty? (codes r)) (pr-str (:diagnostics r)))
    (is (= ["t/unknown" "t/unknown"] (mapv #(type-at r %) [3 5])))))
