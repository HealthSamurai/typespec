(ns typespec.var-initialization-test
  (:require [clojure.test :refer [deftest is]]
            [typespec.check :as check]
            [typespec.main :as main]
            [typespec.parse :as parse]))

(defn- checked
  ([provider consumer] (checked provider consumer {}))
  ([provider consumer opts]
   (check/check-files [(parse/parse-string "consumer.clj"
                                         (str "(ns consumer (:require [provider :as p]))\n" consumer))
                       (parse/parse-string "provider.clj" (str "(ns provider)\n" provider))]
                      opts)))

(defn- codes [result] (mapv :code (:diagnostics result)))

(deftest distinguishes-a-var-from-its-root-value
  (let [result (checked "(defn init [] (def context :ready))"
                        "#'p/context\n(var p/context)\np/context\np/contex")]
    (is (= ["TSP721-01" "TSP2305"] (codes result)) (pr-str (:diagnostics result)))
    (is (= [4 5] (mapv #(get-in % [:pos :row]) (:diagnostics result))))
    (is (= #{:possibly-unbound-var} (get-in result [:diagnostics 0 :reasons])))
    (is (= "clojure.lang.Var" (main/type-at result "consumer.clj" 2 1)))
    (is (= "clojure.lang.Var" (main/type-at result "consumer.clj" 3 1)))
    (is (= "t/unknown" (main/type-at result "consumer.clj" 4 1))))
  (let [result (checked "(declare pending)" "p/pending" {:strict true})]
    (is (= [["TSP721-01" :error]] (mapv (juxt :code :severity) (:diagnostics result))))))

(deftest distinguishes-nil-from-an-omitted-initializer
  (let [result (checked "(def no-init)
                         (def nil-root nil)
                         (do (defonce once nil))
                         (defn maybe-replace [] (def nil-root :later))
                         (declare later)
                         (defn read-later [] later)
                         (defmulti dispatch identity)
                         (defmethod dispatch :read [_] (def method-var later))
                         (def later nil)
                         (declare initialized)
                         (defn initialized [] nil)"
                        "p/no-init\np/nil-root\np/once\np/later\n(p/initialized)\np/method-var")]
    (is (= ["TSP721-01" "TSP721-01"] (codes result)) (pr-str (:diagnostics result)))
    (is (= [["consumer.clj" 2] ["consumer.clj" 7]]
           (mapv (juxt :file (comp :row :pos)) (:diagnostics result))))))

(deftest follows-local-sequences-without-assuming-calls-to-initializers
  (let [result (checked "(declare root)
                         (def snapshot root)
                         (do (def root nil) (def later root))
                         (defn initialize [] (do (def local nil)) local)
                         (defn conditional [flag] (when flag (def maybe-root nil)) maybe-root)
                         (declare handler)
                         (defn install [] (def handler identity))"
                        "(p/initialize)\np/local\n(p/handler 1)\np/root")]
    (is (= (repeat 4 "TSP721-01") (codes result)) (pr-str (:diagnostics result)))
    (is (= [["consumer.clj" 3] ["consumer.clj" 4] ["provider.clj" 3] ["provider.clj" 6]]
           (mapv (juxt :file (comp :row :pos)) (:diagnostics result))))))

(deftest finds-defs-in-code-and-skips-data-and-opaque-macros
  (let [result (checked "(defmacro discard [& _] nil)
                         (defn init [flag] (if flag (def branch nil) nil))
                         (defn lazy-init [] #(do (def deferred nil)))
                         (comment (def only-comment nil))
                         '(def quoted nil)
                         (discard (def macro-data nil))
                         (let [x 1] (case x (def case-label) nil))
                         (let [declare identity] (declare shadowed))
                         (defn explicit [] (clojure.core/declare nested-declaration))
                         (def f (fn ([x] (def from-fn nil)) ([] nil)))"
                        "p/branch\np/deferred\np/from-fn\np/nested-declaration\np/only-comment\np/quoted\np/macro-data\np/case-label\np/shadowed")
        consumer-diags (filter #(= "consumer.clj" (:file %)) (:diagnostics result))]
    (is (= (concat (repeat 4 "TSP721-01") (repeat 5 "TSP2305"))
           (mapv :code consumer-diags)) (pr-str (:diagnostics result)))))

(deftest declared-signatures-do-not-establish-root-initialization
  (let [result (checked "(declare make)
                         (defn install [] (def make identity))
                         (clojure.spec.alpha/fdef make :args (clojure.spec.alpha/cat :x string?) :ret string?)"
                        "(p/make 1)")]
    (is (= ["TSP721-01"] (codes result)) (pr-str (:diagnostics result)))
    (is (= "t/unknown" (main/type-at result "consumer.clj" 2 1)))))

(deftest only-the-eager-wrapper-proves-initialization
  (let [result (checked "(time (def measured nil))
                         (def task (future (def async-root nil)))
                         (def lazy-value (lazy-seq (def deferred-root nil) nil))
                         (defn install [] (time (def local-root nil)) local-root)"
                        "p/measured\np/async-root\np/deferred-root\np/local-root")]
    (is (= (repeat 3 "TSP721-01") (codes result)) (pr-str (:diagnostics result)))
    (is (= [["consumer.clj" 3] ["consumer.clj" 4] ["consumer.clj" 5]]
           (mapv (juxt :file (comp :row :pos)) (:diagnostics result))))))

(deftest a-foreign-binding-vector-does-not-prove-a-global-read
  (let [result (checked "(defn install [] (def state nil))
                         (lifecycle/start [state config]
                           (count [state])
                           provider/state)
                         (count [state])"
                        "p/state")]
    (is (= (repeat 3 "TSP721-01") (codes result)) (pr-str (:diagnostics result)))
    (is (= [["consumer.clj" 2] ["provider.clj" 5] ["provider.clj" 6]]
           (mapv (juxt :file (comp :row :pos)) (:diagnostics result))))))
