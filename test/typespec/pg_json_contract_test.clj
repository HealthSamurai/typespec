(ns typespec.pg-json-contract-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [typespec.parse :as parse]
            [typespec.check :as check]
            [typespec.types :as t]))

(deftest json-normal-return-and-options
  ;; Use the actual example declaration, not a copied second signature table.
  (let [form (get (edn/read-string (slurp "examples/context-pg-check/externs.edn"))
                  'cheshire.core/generate-string)
        ty (t/parse-sig {:fail #(throw (ex-info "Invalid JSON contract" {:form %}))} form)]
    (doseq [[call valid?] [["(json/generate-string {:x 1})" true]
                          ["(json/generate-string {:x 1} nil)" true]
                          ["(json/generate-string {:x 1} {:pretty true})" true]
                          ["(json/generate-string {:x 1} 42)" false]]]
      (let [file (parse/parse-string "json-contract.clj"
                                    (str "(ns app (:require [cheshire.core :as json]))
                                          (defn f {:t/sig '[:-> string?]} [] " call ")"))
            result (check/check-files [file]
                                     {:extern-contracts {:vars {'cheshire.core/generate-string
                                                                {:type ty :source :extern :complete? true}}
                                                         :specs {} :diagnostics []}})]
        (if valid?
          (is (empty? (:diagnostics result)) (pr-str (:diagnostics result)))
          (is (some #(= :error (:severity %)) (:diagnostics result))
              (pr-str (:diagnostics result))))))))
