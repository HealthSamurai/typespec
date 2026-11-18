(ns typespec.review-fixes-test
  "One regression per finding of docs/review-fable-5.2.md (F1..F13)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [typespec.check :as check]
            [typespec.parse :as parse]
            [typespec.solver :as s]
            [typespec.types :as t]))

(def prelude
  "(ns r (:require [clojure.spec.alpha :as s]))
   (s/def ::title string?)
   (s/def ::nick (s/nilable string?))
   (s/def ::item (s/keys :req-un [::title]))
   (s/def ::user (s/keys :req-un [::title ::nick]))
   (defn unk [x] x)
   (defn id {:t/sig '(t/all [a] [a :-> a])} [x] x)
   (defn pick {:t/sig '(t/all [a] [a a :-> a])} [x y] x)
   (defn two {:t/sig '(t/all [a b] [a b :-> a])} [x y] x)
   (defn app {:t/sig '(t/all [a b] [a (t/fn [a :-> b]) :-> b])} [x f] (f x))
   (defn app2 {:t/sig '(t/all [a b] [a a (t/fn [a :-> b]) :-> b])} [x y f] (f x))
   (defn iter {:t/sig '(t/all [a] [a (t/fn [a :-> a]) :-> a])} [x f] (f x))
   (defn fold {:t/sig '(t/all [a b] [(t/fn [b a :-> b]) b (s/coll-of a) :-> b])}
     [f init xs] (reduce f init xs))
   (defn or-else {:t/sig '(t/all [a] [(s/nilable a) a :-> a])} [x d] (if (nil? x) d x))
   (defn int-id {:t/sig '[int? :-> int?]} [n] n)\n")

(defn checked
  ([source] (checked source {}))
  ([source opts] (check/check-files [(parse/parse-string "r.clj" (str prelude source))] opts)))
(defn codes [r] (mapv :code (:diagnostics r)))
(defn errors [r] (filterv #(= :error (:severity %)) (:diagnostics r)))
(defn clean? [r] (empty? (:diagnostics r)))

(defn keys-t [req opt] {:t :map :req req :opt opt})
(defn compare-t [env l r]
  (:status (s/solve env (s/constrain {:vars [] :constraints []} l r :test))))

;; F1: vector? is no evidence about elements, also below a union or a field.
(deftest f1-bare-collection-is-not-proof-at-any-depth
  (let [st (t/prim :string) v (t/prim :vector)
        strings {:t :coll :of st}
        nilable #(t/union % t/nil-t)]
    (doseq [[l r] [[v strings]
                   [v (nilable strings)]
                   [(keys-t {:xs v} {}) (keys-t {:xs strings} {})]
                   [(keys-t {:xs v} {}) (nilable (keys-t {:xs strings} {}))]
                   [{:t :coll :of v} {:t :coll :of (nilable strings)}]
                   [(t/prim :map) (nilable {:t :map-of :k (t/prim :keyword) :v st})]]]
      (is (= :inconclusive (compare-t {} l r)) (str (t/show l) " to " (t/show r))))
    (is (= :proved (compare-t {} {:t :coll :kind :vector :of st} (nilable strings)))))
  (doseq [sig ["(s/nilable (s/coll-of string?))" "(s/coll-of string?)"]]
    (let [r (checked (str "(defn names {:t/sig '(t/all [a] [a " sig " :-> a])} [x names] x)
                           (defn f {:t/sig '[vector? :-> int?]} [v] (names 1 v))"))]
      (is (= ["TSP210-01"] (codes r)) sig))))

;; F2: a literal seed does not freeze the callback parameter type.
(deftest f2-literal-seed-with-accumulating-callback
  (doseq [body ["(defn f {:t/sig '[(s/coll-of int?) :-> int?]} [xs] (fold (fn [acc x] (+ acc x)) 0 xs))"
                "(defn f {:t/sig '[(s/coll-of string?) :-> string?]} [xs] (fold (fn [acc x] (str acc x)) \"\" xs))"
                "(defn f {:t/sig '[:-> int?]} [] (iter 1 (fn [x] (inc x))))"
                "(defn f {:t/sig '[:-> int?]} [] (iter 1 #(inc %)))"]]
    (is (clean? (checked body)) body))
  (testing "an empty seed gives no context: not checked, never a mismatch"
    (let [r (checked "(defn f {:t/sig '[(s/coll-of int?) :-> (s/coll-of int?)]} [xs]
                        (fold (fn [acc x] (conj acc x)) [] xs))")]
      (is (= ["TSP210-01"] (codes r)) (pr-str (:diagnostics r)))))
  (testing "real mistakes are still errors"
    (is (some #{"TSP2322"} (codes (checked "(defn f {:t/sig '[(s/coll-of int?) :-> string?]} [xs]
                                              (fold (fn [acc x] (+ acc x)) 0 xs))"))))
    (is (some #{"TSP2345"} (codes (checked "(defn f {:t/sig '[(s/coll-of string?) :-> int?]} [xs]
                                              (fold (fn [acc x] (+ acc x)) 0 xs))")))))
  (testing "a literal union that no callback returns stays precise"
    (is (clean? (checked "(s/def ::status #{:a :b})
                          (defn f {:t/sig '[::status :-> int?]} [s] (app s (fn [x] (case x :a 1 :b 2))))")))
    (is (some #{"TSP8001"} (codes (checked "(s/def ::status #{:a :b})
                                            (defn f {:t/sig '[::status :-> int?]} [s] (app s (fn [x] (case x :a 1))))"))))))

;; F3: (s/nilable a) as a parameter type.
(deftest f3-variable-inside-a-target-union
  (doseq [body ["(defn f {:t/sig '[::user :-> string?]} [u] (or-else (:nick u) \"anon\"))"
                "(defn f {:t/sig '[::user string? :-> string?]} [u d] (or-else (:nick u) d))"
                "(defn f {:t/sig '[:-> string?]} [] (or-else nil \"anon\"))"]]
    (is (clean? (checked body)) body))
  (let [r (checked "(defn f {:t/sig '[(s/nilable int?) :-> string?]} [n] (or-else n \"x\"))")]
    (is (= ["TSP2322"] (codes r)) (pr-str (:diagnostics r))))
  (testing "solver: the lower bound comes through the union, nil is absorbed"
    (let [st (s/instantiate (t/parse-type {:fail #(throw (ex-info "bad" {:form %}))}
                                          '(t/all [a] [(s/nilable a) :-> a])))
          v (first (:vars st)) param (get-in st [:type :arities 0 :params 0])
          solved (s/solve {} (s/constrain st (t/union (t/prim :string) t/nil-t) param :arg))]
      (is (= :proved (:status solved)))
      (is (= (t/prim :string) (get (:substitution solved) (:id v))))
      (is (= :proved (:status (s/solve {} (-> st (s/constrain t/nil-t param :arg)
                                              (s/constrain (t/prim :int) v :arg)))))))))

;; F4: a candidate built next to an unknown lower bound is not a type.
(deftest f4-incomplete-candidates-do-not-leak
  (testing "no false error from a partial candidate"
    (doseq [body ["(defn f {:t/sig '[:-> ::item]} [] {:title (pick 1 (unk \"s\"))})"
                  "(defn f {:t/sig '[:-> ::item]} [] {:title (pick (unk \"s\") 1)})"
                  "(defn f {:t/sig '[:-> int?]} [] (app2 \"s\" (unk 1) (fn [x] (int-id x))))"
                  "(defn h {:t/sig '[(s/coll-of string?) :-> int?]} [xs] 1)
                   (defn f {:t/sig '[:-> int?]} [] (h [(pick 1 (unk \"s\"))]))"]]
      (let [r (checked body)]
        (is (empty? (errors r)) (pr-str (:diagnostics r)))
        (is (some #{"TSP210-01"} (codes r))))))
  (testing "a fully known result of an unchecked call is still checked"
    (is (some #{"TSP2322"} (codes (checked "(defn f {:t/sig '[:-> string?]} [] (two 1 (unk 2)))"))))
    (is (some #{"TSP2345"} (codes (checked "(defn g {:t/sig '[string? :-> string?]} [s] s)
                                            (defn f {:t/sig '[:-> string?]} [] (let [x (two 1 (unk 2))] (g x)))")))))
  (testing "an unknown argument still reports the call once, without a second diagnostic"
    (is (= ["TSP210-01"] (codes (checked "(defn f {:t/sig '[any? :-> string?]} [x] (id x))")))))
  (testing "solver marks the variable"
    (let [st (s/instantiate (t/parse-type {:fail identity} '(t/all [a b] [a b :-> a])))
          [a b] (:vars st)
          solved (s/solve {} (-> st (s/constrain (t/prim :int) a :x) (s/constrain t/unknown a :y)
                                 (s/constrain (t/prim :string) b :z)))]
      (is (contains? (:uncertain solved) (:id a)))
      (is (not (contains? (:uncertain solved) (:id b)))))))

;; F5: a macro contract never replaces the check of a function or a built-in form.
(deftest f5-contracts-apply-to-macros-only
  (let [contract '{:shape :binding-body :bindings [int? int?] :ret any?}
        source "(defn run-it {:t/sig '[(s/coll-of int?) int? :-> int?]} [v n] n)
                (defn f {:t/sig '[int? int? :-> int?]} [a b] (run-it [a b] \"oops\"))"]
    (let [r (checked source {:macro-contracts {'r/run-it contract}})]
      (is (some #{"TSP9004"} (codes r)))
      (is (some #{"TSP2345"} (codes r)) "the call is still checked by its signature"))
    (testing "a key in the current namespace does not capture core forms"
      (is (clean? (checked "(defn f {:t/sig '[int? :-> int?]} [x] (let [a x] (inc a)))"
                           {:macro-contracts {'r/let '{:shape :binding-body :bindings [string? string?] :ret any?}}})))
      (is (= ["TSP2339"] (codes (checked "(defn f {:t/sig '[::item :-> any?]} [x] (when-let [t (:titel x)] t))"
                                         {:macro-contracts {'r/when-let contract}})))))
    (testing "a form with a built-in rule cannot be given a contract"
      (is (some #{"TSP9004"} (codes (checked "" {:macro-contracts {'clojure.spec.alpha/def contract}})))))
    (testing "a macro of the current namespace still works unqualified"
      (let [r (checked "(defmacro with-two [bindings & body] nil)
                        (with-two [a b] (subs a 0))"
                       {:macro-contracts {'r/with-two contract}})]
        (is (some #{"TSP2345"} (codes r)) (pr-str (:diagnostics r)))))))

;; F6: shared named maps expand as a tree; the work is bounded, not the depth only.
(deftest f6-wide-named-maps-are-bounded
  (let [w 30 d 5
        env {:specs (into {:x/t0 {:type (t/prim :int)}}
                          (for [i (range 1 (inc d))]
                            [(keyword "x" (str "t" i))
                             {:type (keys-t (into {} (for [j (range w)]
                                                        [(keyword (str "f" j)) (t/ref-t (keyword "x" (str "t" (dec i))))]))
                                            {})}]))}
        st (s/instantiate (t/parse-type {:fail identity} '(t/all [a] [a :-> a])))
        v (first (:vars st))
        started (System/nanoTime)
        solved (s/solve env (s/constrain st (t/ref-t :x/t5) v :arg))
        ms (/ (- (System/nanoTime) started) 1e6)]
    (is (= :inconclusive (:status solved)))
    (is (contains? (into #{} (mapcat :reasons (:issues solved))) :size))
    (is (< ms 5000) (str ms " ms")))
  (testing "ordinary sizes are still proved"
    (let [env {:specs {:x/leaf {:type (t/prim :int)}
                       :x/row {:type (keys-t (into {} (for [j (range 20)] [(keyword (str "f" j)) (t/ref-t :x/leaf)])) {})}
                       :x/doc {:type (keys-t (into {} (for [j (range 20)] [(keyword (str "r" j)) (t/ref-t :x/row)])) {})}}}]
      (is (= :proved (compare-t env (t/ref-t :x/doc) (t/ref-t :x/doc)))))))

;; F7: an arbitrary type parameter is not a string.
(deftest f7-unbounded-rigid-is-a-mismatch
  (is (= ["TSP2322"] (codes (checked "(defn bad {:t/sig '(t/all [a] [a :-> string?])} [x] x)"))))
  (is (some #(= :error (:severity %))
            (:diagnostics (checked "(defn bad {:t/sig '(t/all [a] [a :-> int?])} [x] (inc x))"))))
  (doseq [ok ["(defn ok {:t/sig '(t/all [a] [a :-> (s/nilable a)])} [x] x)"
              "(defn ok {:t/sig '(t/all [a] [a :-> any?])} [x] x)"
              "(defn ok {:t/sig '(t/all [a] [a :-> string?])} [x] (str x))"
              "(defn ok {:t/sig '(t/all [(a :< number?)] [a :-> number?])} [x] (inc x))"
              "(defn ok {:t/sig '(t/all [a] [a :-> a])} [x] (id x))"
              "(defn ok {:t/sig '(t/all [a] [a :-> a])} [x] (app x (fn [v] v)))"
              "(defn ok {:t/sig '(t/all [a] [a boolean? :-> a])} [x c] (if c (two x (unk 1)) x))"]]
    (is (empty? (errors (checked ok))) ok)))

;; F8: #(...) is a contextual callback too.
(deftest f8-reader-fn-callback-gets-context
  (is (clean? (checked "(defn f {:t/sig '[::item :-> string?]} [x] (app x #(:title %)))")))
  (is (some #{"TSP2339"} (codes (checked "(defn f {:t/sig '[::item :-> any?]} [x] (app x #(:titel %)))"))))
  (is (some #{"TSP2322"} (codes (checked "(defn f {:t/sig '[::item :-> int?]} [x] (app x #(:title %)))"))))
  (let [app3 "(defn app3 {:t/sig '(t/all [a b c] [a b (t/fn [a b :-> c]) :-> c])} [x y f] (f x y))\n"]
    (is (clean? (checked (str app3 "(defn f {:t/sig '[int? string? :-> string?]} [n s] (app3 n s #(subs %2 %1)))"))))
    (is (some #{"TSP2345"} (codes (checked (str app3 "(defn f {:t/sig '[int? string? :-> string?]} [n s] (app3 n s #(subs %1 %2)))"))))))
  (testing "%& has no fixed arity: body checked without context, call not proved"
    (is (= ["TSP210-01"] (codes (checked "(defn f {:t/sig '[int? :-> any?]} [n] (app n #(count %&)))"))))))

;; F9: any? as a declared parameter type accepts everything.
(deftest f9-top-target-is-proved
  (is (clean? (checked "(defn with-any {:t/sig '(t/all [a] [a any? :-> a])} [x y] x)
                        (defn f {:t/sig '[:-> int?]} [] (with-any 1 (unk 2)))")))
  (is (= :proved (compare-t {} (t/prim :int) t/any)))
  (is (= :proved (compare-t {} (keys-t {:a t/unknown} {}) t/unknown)))
  (testing "a missing reference is not a top type"
    (is (= :inconclusive (compare-t {} (t/prim :int) (t/ref-t :missing/type))))
    (is (= :inconclusive (compare-t {} (t/prim :int) (t/union (t/ref-t :missing/type) t/nil-t))))))

;; F10: _ may repeat in a binding vector.
(deftest f10-underscore-binders
  (let [contract '{:shape :binding-body :bindings [:r/item map?] :ret (t/or map? nil?)}
        run #(checked % {:macro-contracts {'system/defstart contract}})]
    (is (clean? (run "(system/defstart [_ _] nil)")))
    (is (clean? (run "(system/defstart [item _] (:title item) nil)")))
    (is (= ["TSP7032"] (codes (run "(system/defstart [x x] nil)"))))))

;; F11: one fixed arity against another of a different length is a mismatch.
(deftest f11-callback-arity-mismatch
  (doseq [cb ["(fn [a b] \"s\")" "(fn [] \"s\")" "#(str %1 %2)"]]
    (let [r (checked (str "(defn f {:t/sig '[::item :-> string?]} [x] (app x " cb "))"))]
      (is (some #{"TSP2345"} (codes r)) cb)))
  (let [f1 {:t :fn :arities [{:params [(t/prim :int)] :ret (t/prim :int)}]}
        f2 {:t :fn :arities [{:params [(t/prim :int) (t/prim :int)] :ret (t/prim :int)}]}]
    (is (= :mismatch (compare-t {} f2 f1)))
    (is (= :mismatch (compare-t {} f1 f2)))))

;; F12: the example's lifecycle contract admits the nil config upstream passes.
(deftest f12-example-config-may-be-nil
  (let [cfg (read-string (slurp "examples/context-web-pg/typespec.edn"))
        config-type (get-in cfg [:macro-contracts 'system/defstart :bindings 1])
        parsed (t/parse-type {:aliases {} :imports {} :fail #(throw (ex-info "bad" {:form %}))} config-type)]
    (is (t/nilable? {} parsed) (pr-str config-type)))
  (testing "a nilable config must be guarded before use"
    (let [contract '{:shape :binding-body :bindings [:r/item (t/or :r/user nil?)] :ret any?}
          run #(checked % {:macro-contracts {'system/defstart contract}})]
      (is (some #{"TSP2531"} (codes (run "(system/defstart [ctx config] (subs (:title config) 0))"))))
      (is (clean? (run "(system/defstart [ctx config] (when config (subs (:title config) 0)))"))))))

;; F13: a collection predicate of one kind never fits another :kind.
(deftest f13-contradicting-collection-kind
  (let [st (t/prim :string)
        coll (fn [kind] {:t :coll :of st :kind kind})]
    (is (= :mismatch (:status (t/sub-result {} (t/prim :set) (coll :vector)))))
    (is (= :mismatch (:status (t/sub-result {} (t/prim :vector) (coll :set)))))
    (testing "the gradual rule is unchanged where the kinds agree or are unknown"
      (is (= :proved (:status (t/sub-result {} (t/prim :vector) (coll :vector)))))
      (is (= :proved (:status (t/sub-result {} (t/prim :vector) (coll nil)))))
      (is (= :proved (:status (t/sub-result {} (t/prim :coll) (coll :vector)))))))
  (let [r (checked "(defn v {:t/sig '[(s/coll-of string? :kind vector?) :-> int?]} [xs] 1)
                    (defn f {:t/sig '[set? :-> int?]} [xs] (v xs))")]
    (is (= ["TSP2345"] (codes r)) (pr-str (:diagnostics r)))))
