(ns typespec.destructuring-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [typespec.check :as check]
            [typespec.parse :as parse]))

(defn- diagnostics [source]
  (:diagnostics
   (check/check-files [(parse/parse-string "client.clj"
                                         (str "(ns client (:require [clojure.spec.alpha :as s]))\n" source))]
                      {})))

(defn- locations [issues]
  (mapv (juxt (comp :row :pos) :code) issues))

(deftest map-of-supplies-types-for-string-and-symbol-bindings
  (let [api "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::counts (s/map-of string? int?))
(s/def ::symbols (s/map-of symbol? string?))
(s/fdef count-one :args (s/cat :m ::counts) :ret int?)
(defn count-one [{:strs [n] :or {n 0}}] (inc n))
(s/fdef print-one :args (s/cat :m ::symbols) :ret string?)
(defn print-one [{:syms [name] :or {name \"missing\"}}] name)
(s/fdef broken :args (s/cat :m ::counts) :ret string?)
(defn broken [{:strs [n] :or {n 0}}] n)"
        client "(ns client (:require [api :as api]))
(api/count-one {\"n\" 2})
(api/count-one {:n 2})
(api/count-one {\"n\" \"two\"})
(api/print-one {'name \"file\"})
(api/print-one {\"name\" \"file\"})"
        issues (:diagnostics
                (check/check-files [(parse/parse-string "client.clj" client)
                                    (parse/parse-string "api.clj" api)] {}))]
    (is (= [["api.clj" 9 "TSP2322"]
            ["client.clj" 3 "TSP2345"]
            ["client.clj" 4 "TSP2345"]
            ["client.clj" 6 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest destructuring-defaults-run-for-absent-keys-and-preserve-stored-nil
  (let [source "(let [{n :count :or {n 7}} {}] (inc n))
(let [{n \"count\" :or {n 7}} nil] (inc n))
(let [{n 'count :or {n 7}} {'count 3}] (inc n))
(let [{:strs [n] :or {n 7}} {:n nil}] (inc n))
(let [{:syms [n] :or {n 7}} {\"n\" nil}] (inc n))
(let [{:strs [n] :or {n 7}} {\"n\" nil}] (inc n))
(let [{n :count :or {n 7}} {:count nil}] (inc n))
(let [{:keys [n] :or {n (.substring 3 1)}} {:n 2}] (inc n))
(let [{:keys [n] :or {n (inc (:seed m))} :as m} {:seed 2}] (inc n))"
        issues (diagnostics source)]
    (is (= [[7 "TSP2531"] [8 "TSP2531"] [9 "TSP2339"]] (locations issues))
        (pr-str issues))
    (is (every? #(str/includes? (:message %) ":or applies only when the key is absent")
                (take 2 issues)))))

(deftest literal-map-types-survive-destructuring-and-narrowing
  (let [source "(let [{n \"fixture/n\"} {\"fixture/n\" 1}] (inc n))
(let [{:fixture/syms [n]} {'fixture/n 1}] (inc n))
(let [{:syms [fixture/n]} {'fixture/n 1}] (inc n))
(let [{{:strs [n]} 'outer} {'outer {\"n\" 1}}] (inc n))
(let [{:strs [n] :or {n \"missing\"}} {\"n\" 1}] (inc n))
(let [{:syms [n]} {'n 1}] (.substring n 1))
(defn branch [flag]
  (let [{:strs [tag value]} (if flag {\"tag\" :left \"value\" 1}
                                     {\"tag\" :right \"value\" \"text\"})]
    (case tag
      :left (inc value)
      :right (.substring value 1))))"
        issues (diagnostics source)]
    (is (= [[7 "TSP2339"] [13 "TSP7023"]] (locations issues)) (pr-str issues))
    (is (str/includes? (:message (second issues)) "value is known to be String"))))

(deftest type-hints-are-checked-against-the-selected-field
  (let [issues (diagnostics "(let [{:strs [^String label]} {\"label\" \"text\"}] (.substring label 1))
(let [{:strs [^String label]} {\"label\" 7}] (.substring label 1))")]
    (is (= [[3 "TSP2322"]] (locations issues)) (pr-str issues))))

(deftest clojure-1-12-destructuring-runtime-cases
  (is (= [7 nil 3 1 3 4]
         (let [{absent :count :or {absent 7}} nil
               {:strs [n] :or {n 7}} {"n" nil}
               {present 'count :or {present 7}} {'count 3}
               {a "fixture/a"} {"fixture/a" 1}
               {:fixture/syms [c]} {'fixture/c 3}
               {:syms [fixture/d]} {'fixture/d 4}]
           [absent n present a c d]))))
