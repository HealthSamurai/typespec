(ns typespec.types-regression-test
  (:require [clojure.test :refer [deftest is testing]]
            [typespec.types :as t]))

(def string-t (t/prim :string))
(def number-t (t/prim :number))
(def int-t (t/prim :int))
(defn ar [ps ret] {:params ps :ret ret})
(defn ft [& ars] {:t :fn :arities (vec ars)})
(defn rest-ar [ps rest-t ret] (assoc (ar ps ret) :rest rest-t))

(deftest safe-unknown
  (is (false? (t/sub? {} t/unknown string-t)))
  (is (true? (t/sub? {} string-t t/unknown)))
  (is (true? (t/sub? {} t/unknown t/unknown)))
  (is (true? (t/sub? {} t/any string-t)))
  (is (true? (t/sub? {} string-t t/any)))
  (is (true? (t/sub? {} t/never string-t)))
  (is (false? (t/sub? {} (t/ref-t :missing) string-t)))
  (is (false? (t/sub? {:specs {:cycle {:type (t/ref-t :cycle)}}}
                          (t/ref-t :cycle) string-t)))
  (is (false? (t/sub? {} {:t :coll :of t/unknown} {:t :coll :of string-t})))
  (is (false? (t/sub? {} (t/union t/unknown string-t) string-t))))

(deftest finite-proof-budget
  (is (false? (binding [t/*depth* 13] (t/sub? {} string-t string-t))))
  (let [nest (fn [x] (nth (iterate #(hash-map :t :coll :of %) x) 20))]
    (is (false? (t/sub? {} (nest int-t) (nest string-t))))
    ;; A compatible but unproved deep comparison is conservative, too.
    (is (false? (t/sub? {} (nest int-t) (nest number-t)))))
  (let [nest #(nth (iterate (fn [x] {:t :coll :of x}) %) 20)]
    (is (= {:status :inconclusive :reasons #{:depth}}
           (t/sub-result {} (nest int-t) (nest number-t)))))
  (is (= {:status :inconclusive :reasons #{:unknown}}
         (t/sub-result {} t/unknown string-t)))
  (is (= :mismatch (:status (t/sub-result {} int-t string-t))))
  (is (= :proved (:status (t/sub-result {} int-t number-t)))))

(deftest function-variance
  (is (true? (t/sub? {} (ft (ar [number-t] int-t)) (ft (ar [int-t] number-t)))))
  (is (false? (t/sub? {} (ft (ar [int-t] int-t)) (ft (ar [number-t] number-t)))))
  (is (false? (t/sub? {} (ft (ar [number-t] number-t)) (ft (ar [int-t] int-t)))))
  (is (false? (t/sub? {} (ft (ar [string-t] t/unknown)) (ft (ar [string-t] string-t)))))
  (is (true? (t/sub? {} (ft (ar [t/unknown] string-t)) (ft (ar [string-t] string-t)))))
  (is (false? (t/sub? {} (ft (ar [string-t] string-t)) (ft (ar [t/unknown] string-t)))))
  (is (true? (t/sub? {} (ft (ar [] int-t)) (t/prim :fn))))
  (is (false? (t/sub? {} (t/prim :fn) (ft (ar [] int-t)))))
  (is (false? (t/sub? {} (ft (ar [] int-t)) (t/class-type "java.lang.String"))))
  (is (true? (t/sub? {} (ft (ar [] int-t)) (t/class-type "clojure.lang.IFn")))))

(deftest arities-and-rest
  (is (false? (t/sub? {} (ft (ar [] int-t)) (ft (ar [int-t] int-t)))))
  (is (true? (t/sub? {} (ft (ar [] int-t) (ar [number-t] int-t))
                         (ft (ar [] number-t) (ar [int-t] number-t)))))
  (is (false? (t/sub? {} (ft (ar [] int-t))
                          (ft (ar [] number-t) (ar [int-t] number-t)))))
  (is (true? (t/sub? {} (ft (rest-ar [] number-t int-t))
                         (ft (ar [int-t int-t] number-t)))))
  (is (false? (t/sub? {} (ft (ar [int-t] int-t))
                          (ft (rest-ar [int-t] int-t int-t)))))
  (is (true? (t/sub? {} (ft (rest-ar [number-t] number-t int-t))
                         (ft (rest-ar [int-t int-t] int-t number-t)))))
  (is (false? (t/sub? {} (ft (rest-ar [int-t int-t] number-t int-t))
                          (ft (rest-ar [int-t] int-t number-t)))))
  (is (false? (t/sub? {} (ft (rest-ar [] int-t int-t))
                          (ft (rest-ar [] number-t number-t)))))
  (testing "overlapping dispatch and keyword arguments are not guessed"
    (is (false? (t/sub? {} (ft (ar [int-t] string-t) (rest-ar [] int-t int-t))
                            (ft (ar [int-t] int-t)))))
    (is (false? (t/sub? {} (ft (assoc (ar [] int-t) :kwargs {:t :map :req {} :opt {}}))
                            (ft (ar [] number-t)))))))
(deftest opaque-function-contracts
  (let [f (ft (assoc (ar [] t/unknown) :kwargs {:t :map :req {} :opt {}}))]
    (is (true? (t/sub? {} f f)))
    (is (= :proved (:status (t/sub-result {} f f))))
    (is (= {:status :inconclusive :reasons #{:unsupported-function}}
           (t/sub-result {} f (ft (ar [] string-t)))))))
