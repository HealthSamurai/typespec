(ns typespec.function-composition-test
  (:require [clojure.test :refer [deftest is]]
            [typespec.check :as check]
            [typespec.main :as main]
            [typespec.parse :as parse]))

(defn- checked [source]
  (check/check-files [(parse/parse-string "composition.clj"
                                         (str "(ns sample (:require [clojure.string :as str]))\n" source))]
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

(deftest opaque-or-shadowed-functions-stay-conservative
  (let [result (checked "(defn no-proof [f x] (comp inc f) (partial subs x))
                         (let [comp (fn [x] x) partial (fn [x] x)]
                           (comp 1) (partial 2))
                         (let [f (fn ([^String x] x) ([^long x & xs] x))]
                           (partial f 1))")]
    (is (= ["TSP210-01" "TSP210-01" "TSP210-01"] (codes result)) (pr-str (:diagnostics result)))
    (is (= #{:unsupported-function} (get-in result [:diagnostics 2 :reasons]))))
  (let [result (checked "(defn comp [x] x)
                         (comp 42)
                         (inc ((clojure.core/comp inc count) \"x\"))")]
    (is (empty? (codes result)) (pr-str (:diagnostics result)))))
