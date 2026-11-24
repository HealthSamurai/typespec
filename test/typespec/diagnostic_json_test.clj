(ns typespec.diagnostic-json-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data.json :as json]
            [typespec.check :as check]
            [typespec.main :as main]
            [typespec.parse :as parse]))

(deftest incomplete-reasons-survive-json
  (let [d {:id "test" :code "TSP210-01" :severity :warning
           :file "src/demo.clj" :pos {:row 1 :col 1 :end-row 1 :end-col 2}
           :message "compatibility not checked" :reasons #{:unknown :depth}}
        result (json/read-str (json/write-str (main/diag->json d)) :key-fn keyword)]
    (is (= ["depth" "unknown"] (:reasons result)))
    (is (= "warning" (:severity result)))
    (is (not (contains? (main/diag->json (dissoc d :reasons)) :reasons)))))

(deftest diagnostics-only-check-keeps-the-same-report
  (let [api (parse/parse-string "api.clj" "(ns api)
(defn clean [^String s] (clojure.string/trim s))
(defn unused [x] (str x))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(a/clean \" x \")
(a/clean 42)
(inc #_(t/any \"fixture\") :value)")
        full (check/check-files [api client] {})
        report (check/check-files [api client] {::check/diagnostics-only true})]
    (is (seq (:types full)))
    (is (contains? (:sigs full) 'api/unused))
    (is (empty? (:types report)))
    (is (empty? (:sigs report)))
    (is (= (select-keys full [:diagnostics :trust]) (select-keys report [:diagnostics :trust])))))
