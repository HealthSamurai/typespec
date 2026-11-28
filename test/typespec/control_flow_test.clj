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

(deftest generic-calls-propagate-an-aborting-argument
  (let [api (parse/parse-string "api.clj" "(ns api)
(defn label {:t/sig '(t/all [a] [a :-> string?])} [input] \"value\")
(defn pair-label {:t/sig '(t/all [a] [a int? :-> string?])} [input n] \"pair\")")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a] [clojure.spec.alpha :as s]))
(s/fdef fail-first :args (s/cat) :ret int?)
(defn fail-first []
  (if (a/label (throw (Exception. \"stop\"))) :wrong 1))
(s/fdef fail-last :args (s/cat) :ret int?)
(defn fail-last []
  (if (a/pair-label :input (throw (Exception. \"stop\"))) :wrong 1))
(subs (a/label :input) 0)
(inc (a/label :input))
(a/pair-label :input \"bad\")")
        result (check/check-files [client api] {})]
    (is (= [["client.clj" 9 "TSP2345"] ["client.clj" 10 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))
    (is (= ["t/never" "t/never"] (mapv #(main/type-at result "client.clj" % 3) [4 7])))))

(deftest ordinary-calls-propagate-failure-to-evaluate-eager-forms
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/fdef accept :args (s/cat :value int?) :ret int?)
(defn accept [value] value)
(s/fdef run-callback :args (s/cat :f any?) :ret int?)
(defn run-callback [f]
  (if (f (throw (Exception. \"stop\"))) :wrong 1))
(s/fdef run-checked :args (s/cat) :ret int?)
(defn run-checked []
  (if (accept (throw (Exception. \"stop\"))) :wrong 1))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a] [clojure.spec.alpha :as s]))
(s/fdef stop-on-arg :args (s/cat) :ret int?)
(defn stop-on-arg []
  (if (a/accept (throw (Exception. \"stop\"))) :wrong 1))
(inc (a/run-callback identity))
(inc (a/run-checked))
(inc (stop-on-arg))
(inc (a/accept 42))
(a/accept \"wrong\")
(s/fdef stop-on-function :args (s/cat) :ret int?)
(defn stop-on-function []
  (if ((throw (Exception. \"stop\")) 42) :wrong 1))")
        result (check/check-files [client api] {})
        diagnostics (:diagnostics result)]
    (is (= [["client.clj" 9 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics))
    (is (= ["t/never" "t/never"] (mapv #(main/type-at result "api.clj" % 3) [6 9])))
    (is (= ["t/never" "t/never"] (mapv #(main/type-at result "client.clj" % 3) [4 12])))))

(deftest java-calls-propagate-aborting-receivers-and-arguments
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s])
  (:import [java.util.concurrent.atomic AtomicLong]))
(s/fdef stop-method :args (s/cat) :ret int?)
(defn stop-method []
  (if (.charAt \"text\" (throw (Exception. \"method arg stopped\"))) :wrong 1))
(s/fdef stop-static :args (s/cat) :ret int?)
(defn stop-static []
  (if (Integer/parseInt (throw (Exception. \"static arg stopped\"))) :wrong 1))
(s/fdef stop-new :args (s/cat) :ret int?)
(defn stop-new []
  (if (AtomicLong. (throw (Exception. \"constructor arg stopped\"))) :wrong 1))
(s/fdef stop-receiver :args (s/cat) :ret int?)
(defn stop-receiver []
  (if (.length (throw (Exception. \"receiver stopped\"))) :wrong 1))
(s/fdef stop-qualified :args (s/cat) :ret int?)
(defn stop-qualified []
  (if (String/.length (throw (Exception. \"qualified receiver stopped\"))) :wrong 1))
(defn size ^long [^String text] (.length text))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(inc (a/stop-method))
(inc (a/stop-static))
(inc (a/stop-new))
(inc (a/stop-receiver))
(inc (a/stop-qualified))
(inc (a/size \"ok\"))
(a/size 42)")
        result (check/check-files [client api] {})]
    (is (= [["client.clj" 8 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))
    (is (= (repeat 5 "t/never")
           (mapv #(main/type-at result "api.clj" % 3) [5 8 11 14 17])))))

(deftest keyword-calls-check-arity-and-evaluate-the-default-eagerly
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::value int?)
(s/fdef read-value :args (s/cat :input (s/keys :req-un [::value])) :ret int?)
(defn read-value [{:keys [value] :as input}]
  (:value input value))
(s/fdef halt-on-default :args (s/cat) :ret int?)
(defn halt-on-default []
  (if (:value {:value 5} (throw (Exception. \"default evaluated\"))) :wrong 1))
(s/fdef halt-on-target :args (s/cat) :ret int?)
(defn halt-on-target []
  (if (:value (throw (Exception. \"target evaluated\"))) :wrong 1))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(inc (a/read-value {:value 4}))
(inc (a/halt-on-default))
(inc (a/halt-on-target))
(a/read-value {:value \"bad\"})
(:value)
(:value {:value 4} 2 :extra)")
        result (check/check-files [client api] {})]
    (is (= [["client.clj" 5 "TSP2322"] ["client.clj" 6 "TSP2554"] ["client.clj" 7 "TSP2554"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))
    (is (= ["::value" "t/never" "t/never"]
           (mapv #(main/type-at result "api.clj" % 3) [5 8 11])))))

(deftest type-hints-preserve-expressions-without-a-normal-result
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/fdef stop-hinted-receiver :args (s/cat) :ret int?)
(defn stop-hinted-receiver []
  (if (.length ^String (throw (Exception. \"receiver stopped\"))) :wrong 1))
(s/fdef stop-hinted-result :args (s/cat) :ret int?)
(defn stop-hinted-result [] ^String (throw (Exception. \"result stopped\")))
(s/fdef text :args (s/cat :value string?) :ret int?)
(defn text [value] (.length ^String value))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(inc (a/stop-hinted-receiver))
(inc (a/stop-hinted-result))
(inc (a/text \"ok\"))
(a/text 42)")
        result (check/check-files [client api] {})]
    (is (= [["client.clj" 5 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))
    (is (= (repeat 2 "t/never")
           (mapv (fn [[row col]] (main/type-at result "api.clj" row col))
                 [[4 3] [6 37]])))))

(deftest nil-predicates-check-their-single-eager-argument
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/fdef run :args (s/cat) :ret int?)
(defn run [] (if (nil? (throw (Exception. \"stop\"))) :impossible 1))
(s/fdef halt :args (s/cat) :ret int?)
(defn halt [] (if (some? (throw (Exception. \"stop\"))) 1 :impossible))
(s/fdef takes-bool :args (s/cat :v boolean?) :ret boolean?)
(defn takes-bool [v] v)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(a/takes-bool (nil? nil))
(a/takes-bool (some? false))
(inc (a/run))
(inc (a/halt))
(nil?)
(some?)
(nil? nil (subs 42 0))
(some? false (subs 42 0))
(inc (some? false))")
        result (check/check-files [api client] {})]
    (is (= [[6 "TSP2554"] [7 "TSP2554"] [8 "TSP2554"] [8 "TSP2345"]
            [9 "TSP2554"] [9 "TSP2345"] [10 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))
    (is (every? #(= "client.clj" (:file %)) (:diagnostics result)))
    (is (= ["t/never" "t/never"]
           (mapv (fn [[row col]] (main/type-at result "api.clj" row col)) [[3 14] [5 15]])))))

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

(deftest conditional-exits-narrow-the-rest-of-the-body
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::path (s/nilable string?))
(s/def ::request (s/keys :opt-un [::path]))
(s/fdef suffix :args (s/cat :request ::request) :ret string?)
(defn suffix [{:keys [path] :as request}]
  (when-not (some? path) (throw (IllegalArgumentException. \"Missing path\")))
  (str (subs path 1) (subs (:path request) 1)))
(s/fdef ensure-text :args (s/cat :input (s/or :text string? :code int?)) :ret string?)
(defn ensure-text [input]
  (if (string? input) nil (throw (IllegalArgumentException. \"Need text\")))
  (subs input 1))
(s/fdef speculative :args (s/cat :input ::path :flag boolean?) :ret string?)
(defn speculative [input flag]
  (if flag (when-not (string? input) (throw (IllegalArgumentException.))) nil)
  (subs input 1))
(s/fdef assertion-only :args (s/cat :input ::path) :ret string?)
(defn assertion-only [input]
  (assert (string? input))
  (subs input 1))
(s/fdef recovered :args (s/cat :input ::path) :ret string?)
(defn recovered [input]
  (when (nil? input) (try (throw (Exception.)) (catch Exception _ nil)))
  (subs input 1))
(s/fdef inverted :args (s/cat :input ::path) :ret string?)
(defn inverted [input]
  (if-not (string? input) (throw (IllegalArgumentException.)) nil)
  (subs input 1))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(a/suffix {:path \"abc\"})
(a/ensure-text \"abc\")
(a/ensure-text true)")
        result (check/check-files [client api] {})]
    (is (= [["api.clj" 15 "TSP2531"] ["api.clj" 19 "TSP2531"]
            ["api.clj" 23 "TSP2531"] ["client.clj" 4 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result))))
  (let [suffix (fn [{:keys [path] :as request}]
                 (when-not (some? path) (throw (IllegalArgumentException.)))
                 (str (subs path 1) (subs (:path request) 1)))]
    (is (= "bcbc" (suffix {:path "abc"})))
    (is (thrown? IllegalArgumentException (suffix {})))))

(deftest do-exports-narrowing-from-its-normally-completing-body
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::path (s/nilable string?))
(s/def ::request (s/keys :opt-un [::path]))
(s/fdef suffix :args (s/cat :req ::request) :ret string?)
(defn suffix [{:keys [path] :as request}]
  (do
    (do (when-not (string? path) (throw (IllegalArgumentException.))))
    :validated)
  (str (subs path 1) (subs (:path request) 1)))
(s/fdef shadowed :args (s/cat :path ::path :other ::path) :ret string?)
(defn shadowed [path other]
  (do (let [path other]
        (when-not (string? path) (throw (IllegalArgumentException.)))
        path))
  (subs path 1))
(s/fdef conditional :args (s/cat :path ::path :guard? boolean?) :ret string?)
(defn conditional [path guard?]
  (if guard? (do (when-not (string? path) (throw (IllegalArgumentException.)))) nil)
  (subs path 1))
(s/fdef bind-path :args (s/cat :path ::path) :ret string?)
(defn bind-path [path]
  (do (if-some [bound path] bound (throw (IllegalArgumentException.))))
  (subs path 1))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(a/suffix {:path \"abc\"})
(a/suffix {:path 1})
(a/bind-path \"abc\")")
        issues (:diagnostics (check/check-files [client api] {}))]
    (is (= [["api.clj" 15 "TSP2531"] ["api.clj" 19 "TSP2531"] ["client.clj" 3 "TSP2322"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest conditional-bindings-retain-input-facts-without-leaking-locals
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::maybe (s/nilable string?))
(s/def ::path ::maybe)
(s/def ::row (s/keys :opt-un [::path]))
(s/fdef require-path :args (s/cat :req ::row) :ret string?)
(defn require-path [{:keys [path] :as request}]
  (if-let [present path] present (throw (IllegalArgumentException.)))
  (str (subs path 1) (subs (:path request) 1)))
(s/fdef require-some :args (s/cat :value ::maybe) :ret string?)
(defn require-some [value]
  (if-some [present value] present (throw (IllegalArgumentException.)))
  (subs value 1))
(s/fdef empty-required :args (s/cat :value (s/nilable (s/or :boolean boolean? :text string?))) :ret nil?)
(defn empty-required [value]
  (when-some [value value] (throw (IllegalArgumentException.)))
  value)
(s/fdef still-optional :args (s/cat :value ::maybe :other ::maybe) :ret string?)
(defn still-optional [value other]
  (if-some [value other] value (throw (IllegalArgumentException.)))
  (subs value 1))
(s/fdef still-false :args (s/cat :value (s/nilable boolean?)) :ret nil?)
(defn still-false [value]
  (when-let [value value] (throw (IllegalArgumentException.)))
  value)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(a/require-path {:path \"abc\"})
(a/require-some \"abc\")
(a/require-some 1)
(a/empty-required nil)")
        result (check/check-files [client api] {})]
    (is (= [["api.clj" 20 "TSP2531"] ["api.clj" 24 "TSP2322"] ["client.clj" 4 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))
    (is (re-find #"false" (get-in result [:diagnostics 1 :message]))))
  (let [require-some (fn [value] (if-some [present value] present (throw (IllegalArgumentException.))))
        require-falsy (fn [value] (when-let [value value] (throw (IllegalArgumentException.))) value)]
    (is (false? (require-some false)))
    (is (false? (require-falsy false)))
    (is (thrown? IllegalArgumentException (require-some nil)))))

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
