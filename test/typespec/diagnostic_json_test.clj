(ns typespec.diagnostic-json-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data.json :as json]
            [typespec.main :as main]))

(deftest incomplete-reasons-survive-json
  (let [d {:id "test" :code "TSP210-01" :severity :warning
           :file "src/demo.clj" :pos {:row 1 :col 1 :end-row 1 :end-col 2}
           :message "compatibility not checked" :reasons #{:unknown :depth}}
        result (json/read-str (json/write-str (main/diag->json d)) :key-fn keyword)]
    (is (= ["depth" "unknown"] (:reasons result)))
    (is (= "warning" (:severity result)))
    (is (not (contains? (main/diag->json (dissoc d :reasons)) :reasons)))))
