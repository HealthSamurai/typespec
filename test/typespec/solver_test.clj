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

(defn keys-t [req opt] {:t :map :req req :opt opt})
(defn compare-t
  ([env l r] (compare-t env l r 12))
  ([env l r budget]
   (s/solve env (s/constrain {:vars [] :budget budget :constraints []} l r :map-test))))

(deftest structural-keys-presence-and-values
  (let [i (t/prim :int) n (t/prim :number) st (t/prim :string)
        target (keys-t {:id n} {:label st})]
    (is (= :proved (:status (compare-t {} (keys-t {:id i :extra st} {}) target))))
    (is (= :proved (:status (compare-t {} (keys-t {:id i :unused t/unknown} {}) target))))
    (is (= :proved (:status (compare-t {} (keys-t {:id i} {:label st}) target))))
    (doseq [source [(keys-t {} {}) (keys-t {} {:id i})
                    (keys-t {:id st} {}) (keys-t {:id i :label i} {})
                    (keys-t {:id i} {:label i})]]
      (is (= :mismatch (:status (compare-t {} source target)))))
    (let [result (compare-t {} (keys-t {} {:id i}) target)]
      (is (= :map-test (get-in result [:issues 0 :origin])))
      (is (= [[:key :id]] (get-in result [:issues 0 :path]))))
    (doseq [u [t/unknown t/any]]
      (is (= :inconclusive (:status (compare-t {} (keys-t {:id u} {}) target)))))))

(deftest bounded-named-reference-evidence
  (let [i (t/prim :int)
        env {:specs {:id {:type i}
                     :response {:type (keys-t {:id (t/ref-t :id)} {})}
                     :alias {:type (t/ref-t :response)}
                     :loop {:type (t/ref-t :loop)}
                     :a {:type (t/ref-t :b)} :b {:type (t/ref-t :a)}
                     :tree {:type (keys-t {:next (t/ref-t :tree)} {})}}}
        response (t/ref-t :response)]
    (is (= :proved (:status (compare-t env (t/ref-t :alias) response))))
    (is (= :mismatch (:status (compare-t env (keys-t {:id (t/prim :string)} {}) response))))
    (doseq [ref [:missing :loop :a :tree]]
      (is (= :inconclusive (:status (compare-t env (t/ref-t ref) (t/ref-t ref))))))
    (is (= :inconclusive (:status (compare-t env response response 1))))
    (is (= :proved (:status (compare-t env response response 4))))
    (let [callback (fn [ret] {:t :fn :arities [{:params [i] :ret ret}]})]
      (is (= :proved (:status (compare-t env (callback response) (callback (t/ref-t :alias))))))
      (is (= :mismatch (:status (compare-t env (callback (keys-t {} {})) (callback response))))))
    (let [state (s/instantiate identity-t) v (first (:vars state))]
      (is (= :proved (:status (s/solve env (s/constrain state response v :argument)))))
      (is (= :inconclusive (:status (s/solve env (s/constrain state (t/ref-t :missing) v :argument))))))))

(deftest generic-map-field-constraints
  (let [state (s/instantiate identity-t) v (first (:vars state))
        target (keys-t {:value v} {})
        result (s/solve {} (s/constrain state (keys-t {:value (t/prim :int)} {}) target :field))]
    (is (= :proved (:status result)))
    (is (= (t/prim :int) (get (:substitution result) (:id v))))
    (is (= :mismatch (:status (s/solve {} (s/constrain state (keys-t {} {:value (t/prim :int)}) target :field)))))
    (is (= :inconclusive (:status (s/solve {} (s/constrain state (keys-t {} {}) (keys-t {} {:value v}) :field)))))
    (is (= :proved (:status (s/solve {} (s/constrain state (keys-t {} {:value (t/prim :int)}) (keys-t {} {:value v}) :field)))))))

(deftest named-bounds-and-post-substitution-refs
  (let [env {:specs {:test/numeric {:type (t/prim :number)}}}
        ty (assoc-in identity-t [:binders 0 :bound] (t/ref-t :test/numeric))
        state (s/instantiate ty) v (first (:vars state))]
    (is (= :proved (:status (s/solve env (s/constrain state (t/prim :int) v :bound)))))
    (is (= :mismatch (:status (s/solve env (s/constrain state (t/prim :string) v :bound)))))
    (is (= :inconclusive (:status (s/solve {} (s/constrain state (t/prim :int) v :bound))))))
  ;; A generic bound can itself be a named type; normalization must occur after
  ;; substitution as well as on the original constraints.
  (let [env {:specs {:value {:type (t/prim :int)}}}
        state (s/instantiate identity-t) v (first (:vars state))]
    (is (= :proved (:status (s/solve env (-> state
                                           (s/constrain (t/ref-t :value) v :lower)
                                           (s/constrain v (t/prim :number) :upper))))))))
