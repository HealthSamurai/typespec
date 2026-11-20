(ns typespec.context-web-pg-test
  (:require [clojure.test :refer [deftest is]]
            [typespec.main :as main]))

(def ^:private project "examples/context-web-pg")

(deftest application-sources-pass
  (let [result (main/run-check project nil {})]
    (is (= 4 (:files result)))
    (is (empty? (:diagnostics result)) (pr-str (:diagnostics result)))
    (is (= {:cast 0 :any 0 :ignore 0} (:trust result)))
    (is (= :checked (get-in result [:env :guards 'todo.handler/valid-title? :mode])))))

(deftest bad-calls-use-the-application-contracts
  (let [result (main/run-check project ["src" "negative"] {})
        diagnostics (:diagnostics result)]
    (is (= [["negative/bad-types.clj" 10 :error "TSP8001"]
            ["negative/bad-types.clj" 13 :error "TSP270-02"]
            ["negative/bad-types.clj" 20 :error "TSP2345"]
            ["negative/bad_lifecycle.clj" 7 :error "TSP2345"]
            ["negative/bad_title.clj" 4 :error "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :severity :code) diagnostics))
        (pr-str diagnostics))))
