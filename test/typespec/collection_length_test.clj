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
