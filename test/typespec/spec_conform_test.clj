(ns typespec.spec-conform-test
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [typespec.check :as check]
            [typespec.main :as main]
            [typespec.parse :as parse]
            [typespec.project-contracts :as contracts]
            [typespec.spec-conform :as conform]
            [typespec.types :as t]))

(defn- checked [source]
  (check/check-files [(parse/parse-string "conformed.clj"
                       (str "(ns conformed (:require [clojure.spec.alpha :as sp :refer [invalid?]]
                                                      [clojure.string :as str]))\n" source))]
                     {:strict-nil true}))

(deftest conform-returns-the-spec-tags-and-invalid-sentinel
  (let [runtime-spec (s/or :text string? :amount int?)]
    (is (= [[:text "raw"] [:amount 2] ::s/invalid]
           (mapv #(s/conform runtime-spec %) ["raw" 2 :wrong])))
    (is (vector? (s/conform runtime-spec "raw"))))
  (let [r (checked "(sp/def ::payload (sp/or :text string? :amount int?))
(sp/def ::alias ::payload)
(sp/conform ::alias 10)
(sp/conform (sp/or :text string? :amount int?) 10)
(invalid? ::sp/invalid)
(invalid? :other)
(invalid? [:text \"hello\"])
(sp/conform (identity ::alias) 10)")]
    (is (empty? (:diagnostics r)) (pr-str (:diagnostics r)))
    (let [[named inline] (mapv #(main/type-at r "conformed.clj" % 1) [5 6])]
      (is (= named inline))
      (is (str/includes? named "(s/tuple :text string?)"))
      (is (str/includes? named "(s/tuple :amount int?)"))
      (is (str/includes? named ":clojure.spec.alpha/invalid")))
    (is (= ["true" "false" "false" "t/unknown"]
           (mapv #(main/type-at r "conformed.clj" % 1) [7 8 9 10])))))

(deftest invalid-guard-and-case-narrow-destructured-positions
  (doseq [tag ["tag" "(first entry)" "(nth entry 0)" "(key entry)"]
          guard ["(invalid? entry)" "(= ::sp/invalid entry)"]]
    (let [r (checked (str "(sp/def ::payload (sp/or :text string? :amount int?))
(defn render [x]
  (let [entry (sp/conform ::payload x)
        invalid " guard "]
    (if invalid
      \"invalid\"
      (let [[tag value :as entry] entry]
        (case " tag "
          :text (str/upper-case value)
          :amount (str (inc (val entry))))))))"))]
      (is (empty? (:diagnostics r)) (pr-str (:diagnostics r))))))

(deftest final-spec-in-and-determines-the-conformed-value
  (let [domain (parse/parse-string "domain.clj" "(ns domain (:require [clojure.spec.alpha :as s]))
(s/def ::payload (s/and some? (s/or :word string? :count int?)))
(s/def ::changed (s/and (s/or :word string?) (s/conformer (constantly 3))))")
        client (parse/parse-string "client.clj" "(ns client (:require [clojure.spec.alpha :as s]
 [clojure.string :as str] [domain :as d]))
(defn label [x]
  (let [entry (s/conform ::d/payload x)]
    (when-not (s/invalid? entry)
      (let [[tag value] entry]
        (case tag :word (str/upper-case value) :count (str (inc value)))))))
(defn bad [x]
  (let [entry (s/conform (s/and some? (s/or :word string? :count int?)) x)]
    (when-not (s/invalid? entry)
      (let [[tag value] entry]
        (case tag :word (inc value) :count (inc value))))))")
        {:keys [env diagnostics]} (check/check-files [client domain] {})]
    (is (= [["client.clj" 12 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics))
    (is (nil? (conform/tagged-result env nil :domain/changed)))))

(deftest generator-wrappers-do-not-change-conformed-tags
  (let [domain (parse/parse-string "domain.clj" "(ns domain (:require [clojure.spec.alpha :as s]))
(s/def ::payload (s/with-gen (s/or :word string? :count int?) #(throw (ex-info \"generator not invoked\" {}))))")
        client (parse/parse-string "client.clj" "(ns client (:require [clojure.spec.alpha :as s]
 [clojure.string :as str] [domain :as d]))
(defn show [x]
  (let [entry (s/conform ::d/payload x)]
    (when-not (s/invalid? entry)
      (let [[tag value] entry]
        (case tag :word (str/upper-case value) :count (str (inc value)))))))
(defn bad [x]
  (let [entry (s/conform (s/with-gen (s/or :word string? :count int?) identity) x)]
    (when-not (s/invalid? entry)
      (let [[tag value] entry]
        (case tag :word (inc value) :count (inc value))))))")
        r (check/check-files [client domain] {})]
    (is (= [["client.clj" 12 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (str/includes? (main/type-at r "client.clj" 4 16) ":clojure.spec.alpha/invalid"))))

(deftest nonconforming-returns-original-values-for-known-specs
  (let [domain (parse/parse-string "domain.clj" "(ns domain (:require [clojure.spec.alpha :as s]))
(s/def ::mode (s/nonconforming (s/or :start #{:ready} :stop #{:halted})))")
        client (parse/parse-string "client.clj" "(ns client (:require [clojure.spec.alpha :as s] [domain :as d]))
(defn display [x]
  (let [mode (s/conform ::d/mode x)]
    (when-not (s/invalid? mode) (name mode))))
(defn broken [x]
  (let [mode (s/conform (s/nonconforming (s/or :start #{:ready} :stop #{:halted})) x)]
    (when-not (s/invalid? mode) (inc mode))))")
        r (check/check-files [client domain] {})]
    (is (= [["client.clj" 7 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= #{:ready :halted conform/invalid-key}
           (into #{} (map :val) (t/members (:env r) (conform/value-result (:env r) nil :domain/mode)))))
    (is (nil? (conform/tagged-result (:env r) nil :domain/mode)))))

(deftest checks-payloads-in-case-branches-and-keeps-scope
  (let [r (checked "(sp/def ::payload (sp/or :text string? :amount int?))
(defn render [x]
  (let [entry (sp/conform ::payload x)]
    (when-not (sp/invalid? entry)
      (let [[tag value] entry]
        (case tag
          :text (inc value)
          :amount (let [value \"independent\"] (inc value)))
        (str/upper-case value)))))")]
    (is (= [["TSP2345" 9] ["TSP2345" 10] ["TSP2345" 11]]
           (mapv (juxt :code (comp :row :pos)) (:diagnostics r)))
        (pr-str (:diagnostics r)))))

(deftest opaque-payloads-keep-known-tags-without-guessing-transformations
  (let [r (checked "(sp/def ::payload (sp/or :changed (sp/conformer str) :count int?))
(defn render [x]
  (let [entry (sp/conform ::payload x)]
    (when-not (invalid? entry)
      (let [[tag value] entry]
        (case tag
          :changed (str/upper-case value)
          :count (inc value))))))")]
    (is (= ["TSP9002" "TSP210-01"] (mapv :code (:diagnostics r)))
        (pr-str (:diagnostics r)))))

(deftest inline-specs-respect-local-predicate-bindings
  (let [r (checked "(let [string? (constantly true)
                         entry (sp/conform (sp/or :value string?) 1)]
                     (when-not (sp/invalid? entry)
                       (let [[_ value] entry] (str/upper-case value))))")]
    (is (= ["TSP210-01"] (mapv :code (:diagnostics r))) (pr-str (:diagnostics r)))))

(deftest conformed-maps-preserve-required-key-groups
  (let [r (checked "(sp/def ::id int?)
(sp/def ::label string?)
(sp/def ::payload (sp/or :entry (sp/keys :req-un [(or ::id ::label)])))")
        env (:env r)
        output (conform/tagged-result env nil :conformed/payload)
        payload (some #(get-in % [:items 1]) (t/members env output))]
    (is (empty? (:diagnostics r)))
    (is (= #{{:id (t/prim :int)} {:label (t/prim :string)}}
           (set (map :req (t/members env payload)))))))

(deftest tag-and-payload-changes-reach-other-namespaces
  (let [schema (fn [tag field-type]
                 (parse/parse-string "schema.clj"
                   (str "(ns schema (:require [clojure.spec.alpha :as sp :refer [or]]))
(sp/def ::label " field-type ")
(sp/def ::payload (or " tag " (sp/keys :req-un [::label]) :count int?))")))
        api (parse/parse-string "api.clj"
              "(ns api (:require [schema :as schema] [clojure.spec.alpha :as s]))
(s/fdef encode :args (s/cat :x ::schema/payload) :ret any?)
(defn encode [x] x)")
        client (parse/parse-string "client.clj"
                 "(ns client (:require [schema :as schema] [clojure.spec.alpha :as s] [clojure.string :as str]))
(defn show [x]
  (let [entry (s/conform ::schema/payload x)]
    (if (s/invalid? entry)
      \"invalid\"
      (let [[tag value] entry]
        (case tag
          :text (let [{:keys [label]} value] (str/upper-case label))
          :count (str (inc value)))))))")
        run #(check/check-files [client api (schema %1 %2)] {})
        signature (fn [r]
                    (->> (contracts/index r identity)
                         (some #(when (= "api/encode" (:var %)) %))
                         :specs
                         (some #(when (= ":schema/payload" (:name %)) (:conform %)))))
        before (run ":text" "string?")
        renamed (run ":message" "string?")
        changed (run ":text" "int?")]
    (is (empty? (:diagnostics before)) (pr-str (:diagnostics before)))
    (is (str/includes? (signature before) "(s/tuple :text "))
    (is (str/includes? (signature renamed) "(s/tuple :message "))
    (is (= [["client.clj" "TSP8001"] ["client.clj" "TSP2678"]]
           (mapv (juxt :file :code) (:diagnostics renamed))) (pr-str (:diagnostics renamed)))
    (is (= [["client.clj" "TSP2345"]]
           (mapv (juxt :file :code) (:diagnostics changed))) (pr-str (:diagnostics changed)))))

(deftest tag-only-duplicate-spec-is-a-project-conflict
  (let [first (parse/parse-string "schema.clj"
                "(ns schema (:require [clojure.spec.alpha :as s]))
(s/def ::value (s/or :first int?))")
        other (parse/parse-string "override.clj"
                "(ns override (:require [clojure.spec.alpha :as sp]))
(sp/def :schema/value (sp/or :other int?))")
        ds (:diagnostics (check/check-files [first other] {}))]
    (is (= ["TSP722-01"] (mapv :code ds)) (pr-str ds))))
