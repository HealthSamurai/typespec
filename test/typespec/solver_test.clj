(ns typespec.solver-test
  (:require [clojure.test :refer [deftest is testing]]
            [typespec.types :as t]
            [typespec.solver :as s]))

(defn parse [f] (t/parse-type {:fail #(throw (ex-info "Invalid type" {:form %}))} f))
(def identity-t (parse '(t/all [a] [a :-> a])))
(defn invoke [ty arg]
  (let [state (s/instantiate ty)]
    (s/solve {} (s/constrain state arg (get-in state [:type :arities 0 :params 0]) :argument))))

(deftest independent-instantiation
  (let [a (s/instantiate identity-t) b (s/instantiate identity-t)]
    (is (not= (:vars a) (:vars b)))
    (is (= :proved (:status (invoke identity-t (t/prim :string)))))
    (is (= (t/prim :string) (get-in (invoke identity-t (t/prim :string)) [:type :arities 0 :ret])))
    (is (= (t/prim :int) (get-in (invoke identity-t (t/prim :int)) [:type :arities 0 :ret])))))

(deftest grammar-and-roundtrip
  (doseq [f ['(t/all [a] [a :-> a]) '(t/all [(a :< number?)] [a :-> a])]]
    (is (= (t/show (parse f)) (t/show (parse (read-string (t/show (parse f))))))))
  (doseq [f ['(t/all [a a] [a :-> a]) '(t/all [a] [b :-> a])
             '(t/all [(a :< b)] [a :-> a]) '(t/all [a] [a & a :-> a])
             '(t/all [a] [(t/all [b] [b :-> b]) :-> a])
             '(t/all [a] ([a :-> a] [a a :-> a]))]]
    (is (thrown? clojure.lang.ExceptionInfo (parse f)))))

(deftest bounded-and-rigid
  (let [ty (parse '(t/all [(a :< number?)] [a :-> a]))
        rigid (s/open-rigid ty)
        a (get-in rigid [:arities 0 :ret])]
    (is (= :proved (:status (invoke ty (t/prim :int)))))
    (is (= :mismatch (:status (invoke ty (t/prim :string)))))
    (is (= :proved (:status (t/sub-result {} a (t/prim :number)))))
    (is (= :mismatch (:status (t/sub-result {} (t/prim :string) a))))
    (is (= :mismatch (:status (t/sub-result {} (t/prim :int) a))))
    (is (= :proved (:status (t/sub-result {} a a))))))

(deftest incomplete-is-not-proof
  (is (= :inconclusive (:status (s/solve {} (s/instantiate identity-t)))))
  (doseq [x [t/unknown t/any]] (is (= :inconclusive (:status (invoke identity-t x)))))
  (let [state (s/instantiate identity-t) a (first (:vars state))
        sol (s/solve {} (-> state (assoc :budget 0)
                           (s/constrain {:t :coll :of (t/prim :int)} {:t :coll :of a} :deep)))]
    (is (= :inconclusive (:status sol)))
    (is (empty? (:substitution sol))))
  (is (= :inconclusive (:status (binding [t/*depth* 13] (t/sub-result {} (t/prim :int) (t/prim :int)))))))

(deftest variance-and-hof
  (let [ty (parse '(t/all [a b] [(s/coll-of a) [a :-> b] :-> (s/coll-of b)]))
        state (s/instantiate ty) [xs cb] (get-in state [:type :arities 0 :params])
        state (s/constrain state {:t :coll :of (t/prim :int)} xs :collection)
        partial (s/solve {} state)
        callback (parse '[number? :-> string?])
        result (s/solve {} (s/constrain state callback cb :callback))]
    (is (= :inconclusive (:status partial)))
    (is (= (t/prim :int) (get-in (s/substitute cb (:substitution partial)) [:arities 0 :params 0])))
    (is (= :proved (:status result)))
    (is (= (t/prim :string) (get-in result [:type :arities 0 :ret :of])))
    (is (= :mismatch (:status (s/solve {} (s/constrain state (parse '[string? :-> string?]) cb :callback)))))))

(deftest capture-free
  (let [id (get-in identity-t [:binders 0 :id])]
    (is (= identity-t (s/substitute identity-t {id (t/prim :string)}))))
  (let [a (s/open-rigid identity-t) b (s/open-rigid identity-t)]
    (is (= :mismatch (:status (t/sub-result {} (get-in a [:arities 0 :ret]) (get-in b [:arities 0 :ret])))))))

(deftest chained-lowers-and-alpha
  (let [ty (parse '(t/all [a b] [a b :-> a]))
        state (s/instantiate ty) [a b] (:vars state)
        cs [[(t/prim :string) a] [b a] [(t/prim :int) b]]
        run (fn [pairs] (s/solve {} (reduce (fn [st [l r]] (s/constrain st l r :chain)) state pairs)))]
    (is (= :proved (:status (run cs))))
    (is (= :proved (:status (run (reverse cs)))))
    (is (= (:substitution (run cs)) (:substitution (run (reverse cs))))))
  (is (t/alpha-equivalent? identity-t (parse '(t/all [z] [z :-> z]))))
  (is (not (t/alpha-equivalent? identity-t (parse '(t/all [(z :< number?)] [z :-> z])))))
  (is (thrown? clojure.lang.ExceptionInfo (parse '(t/all [a] [(t/fn string?) :-> a])))))
