(ns typespec.macro-contracts-test
  (:require [clojure.test :refer :all]
            [typespec.check :as check]
            [typespec.main :as main]
            [typespec.parse :as parse]))

(def contract '{:shape :binding-body :bindings [:app/context map?] :ret (t/or map? nil?)})
(def contracts {'system/defstart contract
                'system/defstop (assoc contract :ret 'any?)})
(def prelude
  "(ns app (:require [system :as sys :refer [defstart]] [clojure.spec.alpha :as s]))
   (s/def :app/db string?)
   (s/def :app/context (s/keys :req-un [:app/db]))
   (defn query {:t/sig '[:app/context string? :-> map?]} [ctx sql] {})")
(defn checked
  ([body] (checked body contracts))
  ([body cs]
   (check/check-files [(parse/parse-string "app.clj" (str prelude body))]
                      {:macro-contracts cs})))
(defn codes [r] (set (map :code (:diagnostics r))))

(deftest core-wrapper-bodies-use-their-original-positions
  (let [r (checked "\n(time (inc \"bad\"))
(clojure.core/future (inc \"bad\") :done)
(lazy-seq (inc \"bad\") nil)
(lazy-seq 42)
(time)")
        ds (:diagnostics r)]
    (is (= [[5 12 "TSP2345"] [6 27 "TSP2345"] [7 16 "TSP2345"]
            [8 11 "TSP2345"] [9 1 "TSP2554"]]
           (mapv (juxt (comp :row :pos) (comp :col :pos) :code) ds))
        (pr-str ds))))

(deftest core-wrapper-types-reflect-when-their-bodies-run
  (let [r (checked "\n(time :done)
(future :done)
(future (throw (Exception.)))
(lazy-seq (map inc [1 2]))
(lazy-seq nil)
(lazy-seq (throw (Exception.)))
(time (throw (Exception.)))
(let [time (fn [x y] y)] (time 1 :local))")]
    (is (empty? (codes r)) (pr-str (:diagnostics r)))
    (is (= [":done" "java.util.concurrent.Future" "java.util.concurrent.Future"
            "(s/coll-of int? :kind seq?)" "(s/coll-of t/never :kind seq?)"
            "(s/coll-of t/never :kind seq?)" "t/never" "keyword?"]
           (mapv #(main/type-at r "app.clj" % 1) (range 5 13))))))

(deftest conditional-steps-check-reachable-calls-in-source-order
  (let [r (checked "\n(cond-> 1 true (+ \"bad\"))
                       (cond->> \"bad\" true (+ 1))
                       (cond-> 1 false (+ \"skipped\") true inc)
                       (cond-> 1 true inc (inc \"bad test\") inc)
                       (let [__threaded \"bad\"] (cond-> 1 true (+ __threaded)))")
        ds (:diagnostics r)]
    (is (= (repeat 4 "TSP2345") (mapv :code ds)) (pr-str ds))
    (is (= [5 6 8 9] (mapv (comp :row :pos) ds)))
    (is (= 19 (get-in ds [0 :pos :col]))))
  (is (= #{"TSP2554"} (codes (checked "(cond-> 0 true)")))))

(deftest conditional-thread-joins-shapes-and-uses-branch-narrowing
  (let [r (checked "\n(defn thread-value {:t/sig '[(t/or nil? string?) :-> (t/or nil? int?)]}
                          [s] (cond-> s (some? s) count))
                       (defn options {:t/sig '[boolean? :-> map?]}
                          [flag] (cond-> {} flag (assoc :added 1) true (assoc :required \"yes\")))
                       (defn join-types {:t/sig '[boolean? :-> (t/or string? int?)]}
                          [flag] (cond->> 1 flag str))
                       (cond-> :identity)")]
    (is (empty? (:diagnostics r)) (pr-str (:diagnostics r)))
    (is (= "(s/nilable int?)" (main/type-at r "app.clj" 6 31)))
    (is (= "(t/or (s/keys :req [:added :required]) (s/keys :req [:required]))"
           (main/type-at r "app.clj" 8 34)))
    (is (= "(t/or string? 1)" (main/type-at r "app.clj" 10 34)))
    (is (= ":identity" (main/type-at r "app.clj" 11 24)))))

(deftest qualified-aliased-and-referred-bodies
  (doseq [head '[system/defstart sys/defstart defstart]]
    (let [r (checked (str "(" head " [ctx config] (query ctx \"sql\"))"))]
      (is (empty? (:diagnostics r)) (pr-str (:diagnostics r)))))
  (is (empty? (:diagnostics (checked "(sys/defstop [ctx state] (query ctx \"sql\") 42)"))))
  (is (empty? (:diagnostics (checked "(sys/defstart [ctx config])")))))

(deftest body-errors-are-not-hidden
  (doseq [body ["(sys/defstart [ctx config] (query ctx 42))"
                "(system/defstart [ctx config] (query config \"sql\"))"
                "(sys/defstop [ctx state] (query ctx 42))"]]
    (is (seq (:diagnostics (checked body))) body))
  (is (contains? (codes (checked "(sys/defstart [ctx config] (query ctx 42))")) "TSP2345"))
  (is (contains? (codes (checked "(sys/defstart [ctx config] 42)")) "TSP2322"))
  (is (contains? (codes (checked "(sys/defstart [ctx config] (:typo ctx) nil)")) "TSP2339")))

(deftest malformed-shape-fails-closed
  (doseq [body ["(sys/defstart ctx nil)" "(sys/defstart [ctx] nil)"
                "(sys/defstart [ctx cfg extra] nil)" "(sys/defstart [ctx ctx] nil)"
                "(sys/defstart [{:keys [db]} cfg] nil)" "(sys/defstart [ctx &] nil)"
                "(sys/defstart [foo/ctx cfg] nil)" "(sys/defstart)"]]
    (is (contains? (codes (checked body)) "TSP7032") body)))

(deftest malformed-config-fails-closed
  (doseq [cs [nil [] {'defstart contract}
              {'system/defstart (assoc contract :expand 'evil/run)}
              {'system/defstart (dissoc contract :ret)}
              {'system/defstart (assoc contract :shape :anything)}
              {'system/defstart (assoc contract :bindings '(map?))}
              {'system/defstart (assoc contract :bindings ['bogus?])}
              {'system/defstart (assoc contract :ret :missing/type)}
              {'system/defstart (assoc contract :ret '(t/or map? :missing/type))}
              {'clojure.core/let contract}]]
    (is (contains? (codes (checked "" cs)) "TSP9004") (pr-str cs))))

(deftest lexical-shadowing-and-opt-in
  (is (empty? (:diagnostics
               (checked "(let [defstart (fn [n] n)] (defstart 42))"))))
  (let [r (check/check-files
           [(parse/parse-string "system.clj" "(ns system) (defmacro defstart [& xs] (throw (Exception. \"never run\")))")
            (parse/parse-string "app.clj" (str prelude "(sys/defstart [ctx cfg] 42)"))] {})]
    (is (contains? (codes r) "TSP7031"))
    (is (not (contains? (codes r) "TSP2322")))))

(deftest no-namespace-loading-or-macro-execution
  (let [r (checked "(missing.vendor/lifecycle [ctx cfg] (query ctx 42))"
                   {'missing.vendor/lifecycle contract})]
    (is (nil? (find-ns 'missing.vendor)))
    (is (contains? (codes r) "TSP2345")))
  (let [r (check/check-files
           [(parse/parse-string "system.clj"
              "(ns system) (defmacro defstart [& xs] (throw (Exception. \"must not expand\")))")
            (parse/parse-string "app.clj" (str prelude "(sys/defstart [ctx cfg] (query ctx 42))"))]
           {:macro-contracts contracts})]
    (is (contains? (codes r) "TSP2345"))
    (is (not (contains? (codes r) "TSP7031")))))
