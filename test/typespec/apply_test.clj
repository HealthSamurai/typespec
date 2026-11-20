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

(deftest spread-arguments-are-inferred-once
  (let [r (checked "(apply str [#_(t/any \"fixture\") 42])")]
    (is (empty? (codes r)) (pr-str (:diagnostics r)))
    (is (= 1 (get-in r [:trust :any])))))
