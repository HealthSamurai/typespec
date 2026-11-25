(ns typespec.diagnostic-json-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
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

(deftest project-config-filters-warnings-without-changing-cross-namespace-checks
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "typespec-warning-filter-" (make-array java.nio.file.attribute.FileAttribute 0)))
        api (io/file root "src/api.clj")]
    (try
      (io/make-parents api)
      (spit api "(ns api (:require [clojure.spec.alpha :as s]))
(s/fdef accept :args (s/cat :x string?) :ret string?)
(defn accept [x] x)")
      (spit (io/file root "src/client.clj") "(ns client (:require [api :as api]))
(defn- unknown-input [x] (api/accept x))
(defn- unchecked-accumulator [f xs] (reduce f xs))
(defn- reflective [x] (.length x))
(api/accept 42)")
      (spit (io/file root "typespec.edn") "{:suppress-warnings {TSP210-01 #{:unknown}}}")
      (let [run #(main/run-check (str root) nil %)
            filtered (run {})
            full (run {:suppress-warnings {}})
            strict (run {:strict true})
            promoted (run {:levels {'TSP210-01 :error}})
            without-reflection (run {:suppress-warnings {'TSP7020 :all}})
            [unknown accumulator reflective error] (:diagnostics full)]
        (is (= ["TSP210-01" "TSP210-01" "TSP7020" "TSP2345"]
               (mapv :code (:diagnostics full))))
        (is (= [#{:unknown} #{:accumulator}] (mapv :reasons [unknown accumulator])))
        (is (= [accumulator reflective error] (:diagnostics filtered)))
        (is (= [unknown accumulator error] (:diagnostics without-reflection)))
        (is (= [1 1] (mapv :suppressed-warnings [filtered without-reflection])))
        (is (every? #(not (contains? % :suppressed-warnings)) [full strict promoted]))
        (doseq [res [strict promoted]]
          (is (= [:error :error :warning :error] (mapv :severity (:diagnostics res)))))
        (is (= (select-keys full [:types :sigs :trust]) (select-keys filtered [:types :sigs :trust]))))
      (finally (doseq [file (reverse (file-seq root))] (.delete file))))))

(deftest warning-rules-only-remove-fully-matched-reasons
  (let [base {:code "TSP210-01" :severity :warning}
        unknown (assoc base :reasons #{:unknown})
        mixed (assoc base :reasons #{:unknown :depth})
        strict (assoc unknown :severity :error)
        diagnostics [base unknown mixed strict]]
    (is (= [[base mixed strict] 1]
           (#'check/filter-warnings diagnostics {'TSP210-01 #{:unknown}})))
    (is (= [[base strict] 2]
           (#'check/filter-warnings diagnostics {'TSP210-01 #{:unknown :depth}})))
    (is (= [[strict] 3]
           (#'check/filter-warnings diagnostics {'TSP210-01 :all})))))
