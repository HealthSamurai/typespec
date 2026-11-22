(ns typespec.control-flow-test
  (:require [clojure.test :refer [deftest is]]
            [typespec.check :as check]
            [typespec.main :as main]
            [typespec.parse :as parse]))

(defn- checked [source & [opts]]
  (check/check-files [(parse/parse-string "flow.clj" source)] (or opts {})))

(defn- issues [result]
  (mapv (juxt (comp :row :pos) (comp :col :pos) :code :severity) (:diagnostics result)))

(definterface CloseWithArgument
  (close [argument]))

(deftest throw-checks-the-operand-and-arity
  (let [source "(ns flow)
(throw (ex-info \"stop\" {}))
(throw nil)
(throw \"bad\")
(throw)
(throw (Exception.) :extra)
(defn- propagate [x] (throw x))"
        result (checked source)]
    (is (= [[4 8 "TSP2345" :error] [5 1 "TSP2554" :error]
            [6 1 "TSP2554" :error] [7 29 "TSP210-01" :warning]]
           (issues result)) (pr-str (:diagnostics result)))
    (is (= ["t/never" "t/never" "t/never"]
           (mapv #(main/type-at result "flow.clj" % 1) [2 3 4])))
    (is (= #{:unknown} (get-in result [:diagnostics 3 :reasons])))
    (is (= [7 29 "TSP210-01" :error] (last (issues (checked source {:strict true})))))))

(deftest assertions-check-the-condition-and-reachable-message
  (let [source "(ns flow (:require [clojure.spec.alpha :as s]))
(assert (string? (subs 42 0)))
(clojure.core/assert nil (subs 42 0))
(assert :success (subs 42 0))
(assert false {:message \"any value may be rendered\"})
(s/fdef report :args (s/cat :text (s/nilable string?)) :ret nil?)
(defn report [text] (assert (nil? text) (subs text 0)))
(s/fdef require-text :args (s/cat :text (s/nilable string?)) :ret string?)
(defn require-text [text] (assert (string? text)) text)
(assert)
(assert true \"msg\" :extra)"
        result (checked source)]
    (is (= [[2 "TSP2345"] [3 "TSP2345"] [9 "TSP2322"] [10 "TSP2554"] [11 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics result))) (pr-str (:diagnostics result)))
    (is (= "nil?" (main/type-at result "flow.clj" 5 1)))
    (is (= (:diagnostics result) (binding [*assert* false] (:diagnostics (checked source)))))))

(deftest assertion-checking-respects-source-bindings
  (let [result (checked "(ns flow (:refer-clojure :exclude [assert]))
(defn assert [] :ordinary)
(assert)
(let [assert (fn [_] :local)] (assert 42))
(clojure.core/assert true)")]
    (is (empty? (:diagnostics result)))
    (is (= ["keyword?" ":local" "nil?"]
           (mapv #(main/type-at result "flow.clj" % 1) [3 4 5])))))

(deftest finally-affects-normal-continuation
  (let [result (checked "(ns flow (:require [clojure.spec.alpha :as s]))
(do (throw (Exception. \"stop\")) 42)
(try 42 (finally :cleanup))
(try 42 (finally (throw (Exception. \"cleanup\"))))
(try (throw (Exception. \"body\")) (catch Exception e nil) (finally :cleanup))
(try (do (throw (Exception.)) 42) (catch Exception e :caught))
(try 42 (catch Exception e :caught) (finally (throw (Exception.)) :dead))
(try 42 (finally (assert false)))
(try 42 (finally (throw (Exception.)) (inc \"bad\")))
(s/fdef abort :args (s/cat) :ret string?)
(defn abort [] (try 42 (finally (throw (Exception. \"cleanup\")))))")]
    (is (= ["t/never" "42" "t/never" "nil?" ":caught" "t/never" "42" "t/never"]
           (mapv #(main/type-at result "flow.clj" % 1) (range 2 10))))
    (is (= [[9 "TSP2345"]] (mapv (juxt (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))))

(deftest dead-recur-does-not-widen-live-loop-locals
  (let [result (checked "(ns flow (:require [clojure.spec.alpha :as s]))
(s/fdef run :args (s/cat :stop? boolean?) :ret string?)
(defn run [stop?]
  (loop [x \"text\"]
    (if stop?
      (do (throw (Exception.)) (recur 42))
      x)))")]
    (is (empty? (:diagnostics result)) (pr-str (:diagnostics result)))))

(deftest with-open-checks-implicit-close-at-the-initializer
  (let [result (checked "(ns flow (:require [clojure.spec.alpha :as s]))
(s/fdef consume :args (s/cat :r (partial instance? java.io.Reader)) :ret int?)
(defn consume [r] 0)
(with-open [] :empty)
(clojure.core/with-open [reader (java.io.StringReader. \"\")]
  (consume reader))
(with-open [r (javax.naming.InitialContext.)] :legacy)
(with-open [r (Object.)] :missing-method)
(with-open [r nil] :absent-resource)
(with-open [r []] :collection)
(s/fdef maybe-open :args (s/cat :r (s/nilable (partial instance? java.io.Reader))) :ret string?)
(defn maybe-open [r] (with-open [^java.io.Reader x r] \"ok\"))
(with-open [r] :bad-bindings)
(with-open [[r] (Object.)] :bad-binding-name)
(with-open [r (throw (Exception.)) ignored (Object.)] :unreachable)
(s/fdef choose :args (s/cat :r (s/or :reader (partial instance? java.io.Reader)
                                  :legacy (partial instance? javax.naming.Context))) :ret string?)
(defn choose [r] (with-open [x r] \"ok\"))")]
    (is (= [[8 15 "TSP2339" :error] [9 15 "TSP2531" :error] [10 15 "TSP2339" :error]
            [12 52 "TSP2531" :error] [13 12 "TSP2345" :error] [14 12 "TSP2345" :error]]
           (issues result))
        (pr-str (:diagnostics result)))
    (is (= [":empty" "int?" ":legacy" "t/never"]
           (mapv #(main/type-at result "flow.clj" % 1) [4 5 7 15])))))

(deftest with-open-requires-a-zero-argument-close-method
  (let [thread (Thread/currentThread)
        prior (.getContextClassLoader thread)]
    (try
      (.setContextClassLoader thread (.getClassLoader CloseWithArgument))
      (let [result (checked "(ns flow (:import [typespec.control_flow_test CloseWithArgument]))
(defn- use-resource [^CloseWithArgument r]
  (with-open [x r] :done))")]
        (is (= [[3 17 "TSP2554" :error]] (issues result)) (pr-str (:diagnostics result))))
      (finally
        (.setContextClassLoader thread prior)))))

(deftest with-open-reports-opaque-and-platform-resources-in-strict-modes
  (let [source "(ns flow)
(defn- with-resource [r] (with-open [x r] :done))
(defn- with-reader [^java.io.Reader r] (with-open [x r] :done))"]
    (is (empty? (issues (checked source))))
    (is (= [[2 40 "TSP210-01" :error]] (issues (checked source {:strict true}))))
    (is (= [[3 54 "TSP2531" :error]] (issues (checked source {:strict-nil true}))))))

(deftest jvm-control-flow-on-clojure-1-12-4
  (is (thrown? NullPointerException (throw nil)))
  (is (thrown? ClassCastException (throw "bad")))
  (is (= :value (try :value (finally :cleanup))))
  (is (thrown-with-msg? IllegalStateException #"cleanup"
                        (try :value (finally (throw (IllegalStateException. "cleanup"))))))
  (is (nil? (binding [*assert* false] (eval '(assert false (subs 42 0))))))
  (is (thrown? ClassCastException
               (binding [*assert* true] (eval '(assert false (subs 42 0)))))))
