(ns typespec.macro-contracts-test
  (:require [clojure.test :refer :all]
            [typespec.check :as check]
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
