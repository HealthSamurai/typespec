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

(deftest conform-coll-of-uses-the-output-collection-kind
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::choice (s/or :text string? :count int?))
(s/def ::rows (s/coll-of ::choice :kind list? :into [] :min-count 2 :max-count 2))
(s/def ::unique (s/coll-of ::choice :kind vector? :into #{} :min-count 2))
(s/fdef accept :args (s/cat :n int?) :ret int?)
(defn accept [n] n)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a] [clojure.spec.alpha :as s]))
(s/fdef labels :args (s/cat :raw any?) :ret (s/coll-of string? :kind vector?))
(defn labels [raw]
  (let [rows (s/conform ::a/rows raw)]
    (if (s/invalid? rows) []
      (mapv (fn [[tag x]] (case tag :text (subs x 0) :count (str (a/accept x)))) rows))))
(s/fdef fixed-count :args (s/cat :raw any?) :ret #{2})
(defn fixed-count [raw]
  (let [rows (s/conform ::a/rows raw)]
    (if (s/invalid? rows) 2 (count rows))))
(s/fdef values :args (s/cat :raw any?) :ret set?)
(defn values [raw]
  (let [rows (s/conform ::a/unique raw)]
    (if (s/invalid? rows) #{} rows)))
(s/fdef wrong :args (s/cat :raw any?) :ret any?)
(defn wrong [raw]
  (let [rows (s/conform ::a/rows raw)]
    (when-not (s/invalid? rows)
      (mapv (fn [[tag x]] (case tag :text (a/accept x) :count x)) rows))))")
        issues (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 19 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues)))
  (let [context {:aliases {'s 'clojure.spec.alpha}}
        result (conform/value-result {} context '(s/coll-of int? :kind seq? :into [] :count 2))
        collection (first (filter #(= :coll (:t %)) (t/members {} result)))]
    (is (= [:vector 2 nil nil] ((juxt :kind :min-count :max-count :counted-max) collection)))
    (is (= [1 2 3] (s/conform (s/coll-of int? :kind seq? :into [] :count 2) (map identity [1 2 3]))))))

(deftest conform-map-of-into-vector-keeps-pair-data-and-entry-identity
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::val (s/or :text string? :count int?))
(s/def ::rows (s/map-of string? ::val :into [] :count 2))
(s/def ::raw-rows (s/map-of string? int? :into [] :count 1))
(s/fdef accept :args (s/cat :n int?) :ret int?)
(defn accept [n] n)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a] [clojure.spec.alpha :as s]))
(s/fdef render :args (s/cat :raw any?) :ret (s/coll-of string? :kind vector?))
(defn render [raw]
  (let [rows (s/conform ::a/rows raw)]
    (if (s/invalid? rows) []
      (mapv (fn [[label [tag x]]]
              (str (subs label 0)
                   (case tag :text (subs x 0) :count (a/accept x))))
            rows))))
(s/fdef size :args (s/cat :raw any?) :ret #{2})
(defn size [raw]
  (let [rows (s/conform ::a/rows raw)]
    (if (s/invalid? rows) 2 (count rows))))
(defn wrong [raw]
  (let [rows (s/conform ::a/rows raw)]
    (when-not (s/invalid? rows)
      (key (nth rows 0))
      (val (nth rows 0))
      (a/accept (first (nth rows 0))))))
(s/fdef raw-label :args (s/cat :raw any?) :ret string?)
(defn raw-label [raw]
  (let [rows (s/conform ::a/raw-rows raw)]
    (if (s/invalid? rows) \"invalid\" (subs (key (nth rows 0)) 0))))")
        issues (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 17 "TSP2345"] ["client.clj" 18 "TSP2345"] ["client.clj" 19 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues)))
  (let [same-key (s/map-of (s/conformer (constantly :same)) int?
                           :conform-keys true :into [] :count 2)]
    (is (= [[:same 1] [:same 2]] (s/conform same-key (array-map "a" 1 "b" 2))))))

(deftest conform-keeps-types-through-compatible-following-predicates
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::payload (s/or :text string? :amount int?))
(s/def ::union (s/and ::payload vector? any?))
(s/def ::n int?)
(s/def ::packet (s/and (s/keys :req-un [::n]) map?))
(s/fdef accept :args (s/cat :n int?) :ret int?)
(defn accept [n] n)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a] [clojure.spec.alpha :as s]))
(s/fdef render :args (s/cat :raw any?) :ret string?)
(defn render [raw]
  (let [x (s/conform ::a/union raw)]
    (if (s/invalid? x) \"invalid\"
      (let [[tag value] x]
        (case tag :text (subs value 0) :amount (str (a/accept value)))))))
(s/fdef unwrap :args (s/cat :raw any?) :ret int?)
(defn unwrap [raw]
  (let [x (s/conform ::a/packet raw)]
    (if (s/invalid? x) 0 (a/accept (:n x)))))
(s/fdef wrong :args (s/cat :raw any?) :ret int?)
(defn wrong [raw]
  (let [x (s/conform ::a/union raw)]
    (if (s/invalid? x) 0
      (let [[tag value] x]
        (case tag :text (a/accept value) :amount value)))))")
        issues (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 17 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest conform-preserves-nilable-results-across-files
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::n int?)
(s/def ::packet (s/nilable (s/keys :req-un [::n])))
(s/def ::event (s/nilable (s/or :text string? :count int?)))
(s/fdef accept :args (s/cat :n int?) :ret int?)
(defn accept [n] n)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a] [clojure.spec.alpha :as s]))
(s/fdef unpack :args (s/cat :raw any?) :ret int?)
(defn unpack [raw]
  (let [value (s/conform ::a/packet raw)]
    (if (s/invalid? value) 0
      (if-some [{:keys [n]} value] (a/accept n) 0))))
(s/fdef missing-guard :args (s/cat :raw any?) :ret int?)
(defn missing-guard [raw]
  (let [value (s/conform ::a/packet raw)]
    (if (s/invalid? value) 0 (inc (:n value)))))
(s/fdef render :args (s/cat :raw any?) :ret string?)
(defn render [raw]
  (let [value (s/conform ::a/event raw)]
    (if (s/invalid? value) \"invalid\"
      (if-some [[tag x] value]
        (case tag :text (subs x 0) :count (str (a/accept x)))
        \"nil\"))))
(unpack nil)
(render nil)")
        issues (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 10 "TSP2531"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues)))
  (let [result (conform/value-result {} {:aliases {'s 'clojure.spec.alpha}} '(s/nilable string?))]
    (is (= #{t/nil-t (t/prim :string) (t/lit conform/invalid-key)}
           (set (t/members {} result))))))

(deftest conform-preserves-element-and-collection-types-across-files
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::item (s/or :text string? :count int?))
(s/def ::items (s/coll-of ::item :kind vector? :min-count 1))
(s/def ::labels (s/coll-of ::item :kind list?))
(s/fdef format-n :args (s/cat :n int?) :ret string?)
(defn format-n [n] (str n))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a] [clojure.spec.alpha :as s]))
(s/fdef print-items :args (s/cat :raw any?) :ret (s/coll-of string? :kind vector?))
(defn print-items [raw]
  (let [items (s/conform ::a/items raw)]
    (if (s/invalid? items)
      []
      (mapv (fn [[tag x]] (case tag :text (subs x 0) :count (a/format-n x))) items))))
(s/fdef print-labels :args (s/cat :raw any?) :ret seq?)
(defn print-labels [raw]
  (let [items (s/conform ::a/labels raw)]
    (if (s/invalid? items) ()
      (for [[tag x] items] (case tag :text (subs x 0) :count (a/format-n x))))))
(s/fdef wrong :args (s/cat :raw any?) :ret any?)
(defn wrong [raw]
  (let [items (s/conform ::a/items raw)]
    (when-not (s/invalid? items)
      (mapv (fn [[tag x]] (case tag :text (a/format-n x) :count (a/format-n x))) items))))
(print-items [\"ok\" 42])")
        issues (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 17 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues)))
  (let [output (fn [spec]
                 (first (filter #(= :coll (:t %))
                                (t/members {}
                                           (conform/value-result {} {:aliases {'s 'clojure.spec.alpha}} spec)))))
        set-output (output '(s/coll-of (s/conformer (constantly :one)) :kind set? :count 2))
        distinct-input (output '(s/coll-of (s/conformer (constantly :one))
                                         :kind vector? :count 2 :distinct true))]
    (is (= [nil 2] ((juxt :min-count :max-count) set-output)))
    (is (= [2 2 nil] ((juxt :min-count :max-count :distinct?) distinct-input)))))

(deftest conform-preserves-dictionary-value-and-key-types-across-files
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::payload (s/or :text string? :amount int?))
(s/def ::index (s/map-of string? ::payload))
(s/def ::key (s/or :text string? :amount int?))
(s/def ::tagged-keys (s/map-of ::key int? :conform-keys true))
(s/def ::raw-keys (s/map-of ::key int?))
(s/fdef accept :args (s/cat :n int?) :ret int?)
(defn accept [n] n)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a] [clojure.spec.alpha :as s]))
(s/fdef render :args (s/cat :raw any?) :ret string?)
(defn render [raw]
  (let [result (s/conform ::a/index raw)]
    (if (s/invalid? result) \"invalid\"
      (if-let [[tag x] (get result \"main\")]
        (case tag :text (subs x 0) :amount (str (a/accept x)))
        \"missing\"))))
(s/fdef tags :args (s/cat :raw any?) :ret (s/coll-of string? :kind vector?))
(defn tags [raw]
  (let [result (s/conform ::a/tagged-keys raw)]
    (if (s/invalid? result) []
      (mapv (fn [[tag x]] (case tag :text (subs x 0) :amount (str (inc x)))) (keys result)))))
(s/fdef read-raw :args (s/cat :raw any?) :ret int?)
(defn read-raw [raw]
  (let [result (s/conform ::a/raw-keys raw)]
    (if (s/invalid? result) 0 (inc (get result \"main\" 0)))))
(s/fdef wrong :args (s/cat :raw any?) :ret int?)
(defn wrong [raw]
  (let [result (s/conform ::a/index raw)]
    (if (s/invalid? result) 0
      (if-let [[tag x] (get result \"main\")]
        (case tag :text (a/accept x) :amount x)
        0))))")
        issues (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 23 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues)))
  (let [combined (s/map-of (s/conformer (constantly :same)) int? :conform-keys true :count 2)
        result (conform/value-result {} {:aliases {'s 'clojure.spec.alpha}}
                                     '(s/map-of (s/conformer (constantly :same)) int?
                                                :conform-keys true :count 2))
        dictionary (first (filter #(= :map-of (:t %)) (t/members {} result)))]
    (is (= {:same 2} (s/conform combined (array-map "a" 1 "b" 2))))
    (is (= [nil 2] ((juxt :min-count :max-count) dictionary)))))

(deftest conform-preserves-outer-map-and-tuple-shapes-across-files
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::payload (s/or :text string? :amount int?))
(s/def ::packet (s/keys :req-un [::payload]))
(s/def ::row (s/tuple ::payload int?))
(s/fdef accept :args (s/cat :n int?) :ret int?)
(defn accept [n] n)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a] [clojure.spec.alpha :as s]))
(s/fdef render :args (s/cat :raw any?) :ret string?)
(defn render [raw]
  (let [packet (s/conform ::a/packet raw)]
    (if (s/invalid? packet) \"invalid\"
      (let [[tag value] (:payload packet)]
        (case tag :text (subs value 0) :amount (str (a/accept value)))))))
(s/fdef wrong :args (s/cat :raw any?) :ret int?)
(defn wrong [raw]
  (let [packet (s/conform ::a/packet raw)]
    (if (s/invalid? packet) 0
      (let [[tag value] (:payload packet)]
        (case tag :text (a/accept value) :amount value)))))
(s/fdef from-row :args (s/cat :raw any?) :ret string?)
(defn from-row [raw]
  (let [row (s/conform ::a/row raw)]
    (if (s/invalid? row) \"invalid\"
      (let [[[tag value] n] row]
        (case tag :text (str (subs value 0) (inc n)) :amount (str (+ value n)))))))
(render {:payload \"ok\"})
(from-row [\"ok\" 42])")
        issues (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 13 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

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
        (str/trim value)))))")]
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
          :text (let [{:keys [label]} value] (str/trim label))
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
