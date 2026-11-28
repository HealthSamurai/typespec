(ns typespec.pg-boundary-contract-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [typespec.parse :as parse]
            [typespec.check :as check]
            [typespec.types :as t]))

(defn checked [source]
  (let [forms (edn/read-string (slurp "examples/context-pg-check/externs.edn"))
        vars (into {} (for [sym '[pg/truncate! pg/safe-execute! pg/fetch]]
                        [sym {:type (t/parse-sig {:fail #(throw (ex-info "Invalid contract" {:form %}))}
                                                (get forms sym))
                              :source :extern :complete? true}]))]
    (check/check-files [(parse/parse-string "pg-boundaries.clj" source)]
                      {:extern-contracts {:vars vars :specs {} :diagnostics []}})))

(deftest context-and-field-bounds
  (doseq [call ["(pg/truncate! {} :table)" "(pg/safe-execute! {} [\"select 1\"])"
                "(pg/fetch {} [] 10 :id (fn [row index] nil))" "(pg/fetch {} [] 10 \"id\" (fn [row index] nil))"
                "(pg/fetch {} [] 10 'id (fn [row index] nil))"]]
    (is (empty? (:diagnostics (checked (str "(ns app (:require [pg])) " call)))) call))
  (doseq [call ["(pg/fetch {} [] 10 :id nil)"
                "(pg/fetch {} [] \"ten\" :id (fn [row index] nil))"
                "(pg/truncate! 42 :table)" "(pg/safe-execute! nil [])"
                "(pg/fetch {} [] 10 42 (fn [row index] nil))"]]
    (is (some #(= "TSP2345" (:code %))
              (:diagnostics (checked (str "(ns app (:require [pg])) " call)))) call)))

(deftest bodies-remain-checked-and-results-stay-precise
  (testing "extern declaration does not bypass own function body"
    (doseq [source ["(ns pg) (defn truncate! [ctx table] 42)"
                    "(ns pg) (defn safe-execute! [ctx query] [42])"
                    "(ns pg) (defn fetch [ctx sql n field cb] \"wrong\")"]]
      (is (some #(= :error (:severity %)) (:diagnostics (checked source))) source)))
  (testing "vector-of-map and counter results remain usable without unknown warnings"
    (is (empty? (:diagnostics
                 (checked "(ns app (:require [pg]))
                   (defn rows {:t/sig '[:-> (s/coll-of map? :kind vector?)]} []
                     (pg/safe-execute! {} []))
                   (inc (pg/fetch {} [] 10 :id (fn [row index] nil)))")))))
  (testing "unknown context is still an explicit unchecked boundary"
    (is (some #(= "TSP210-01" (:code %))
              (:diagnostics (checked "(ns app (:require [pg])) (defn f [ctx] (pg/truncate! ctx :x))"))))))

(deftest fetch-context-comes-from-the-edn-declaration
  (let [prefix "(ns app (:require [pg] [clojure.string :as str])) (pg/fetch {} [] 10 :id "
        codes (fn [callback] (mapv :code (:diagnostics (checked (str prefix callback ")")))))]
    (is (empty? (codes "(fn [value index] (when (some? value) (str/upper-case value) (inc index)))")))
    (is (= ["TSP2531"] (codes "(fn [value index] (str/upper-case value))")))
    (is (= ["TSP2345"] (codes "(fn [value index] (str/trim index))")))))
