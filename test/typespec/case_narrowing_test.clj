(ns typespec.case-narrowing-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [typespec.check :as check]
            [typespec.main :as main]
            [typespec.parse :as parse]))

(def ^:private specs
  "(ns example.events (:require [clojure.spec.alpha :as s] [clojure.string :as str]))
   (s/def :text/kind #{:text :markdown})
   (s/def :text/payload string?)
   (s/def :count/kind #{:count})
   (s/def :count/payload int?)
   (s/def ::text (s/keys :req-un [:text/kind :text/payload]))
   (s/def ::count (s/keys :req-un [:count/kind :count/payload]))
   (s/def ::event (s/or :string ::text :number ::count))
   (s/fdef render :args (s/cat :event ::event) :ret string?)\n")

(deftest equality-with-class-refines-the-value-across-files
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s] [clojure.string :as str]))
(s/fdef text :args (s/cat :value string?) :ret string?)
(defn text [value] (str/trim value))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a] [clojure.spec.alpha :as s]))
(defn render [value] (when (= (class value) String) (a/text value)))
(s/fdef choose :args (s/cat :value (s/or :text string? :number int?)) :ret any?)
(defn choose [value] (if (= String (class value)) (a/text value) (inc value)))
(defn wrong [value] (when (= (class value) String) (inc value)))
(s/fdef inherited :args (s/cat :value java.util.Date) :ret number?)
(defn inherited [value] (if (= (class value) java.util.Date) 0 (inc value)))
(defn metadata-type [value] (when (= (type value) String) (a/text value)))
(defn covariant [^\"[Ljava.lang.String;\" values]
  (if (not= (class values) Object/1) (inc values) 0))")
        issues (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 5 "TSP2345"] ["client.clj" 7 "TSP2345"]
            ["client.clj" 8 "TSP210-01"] ["client.clj" 10 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues)))
  (let [date (java.sql.Date. 0)
        strings (into-array String ["x"])
        tagged (with-meta 'metadata {:type String})]
    (is (instance? java.util.Date date))
    (is (not= java.util.Date (class date)))
    (is (instance? (class (object-array 0)) strings))
    (is (not= (class (object-array 0)) (class strings)))
    (is (= String (type tagged)))
    (is (not= String (class tagged)))))

(defn- checked [source]
  (check/check-files [(parse/parse-string "events.clj" (str specs source))] {:strict true :strict-nil true}))

(defn- diagnostics [source]
  (:diagnostics (checked source)))

(deftest case-reads-variant-types-from-spec
  (doseq [source ["(defn render [event]
                    (case (:kind event)
                      (:text :markdown) (str/upper-case (:payload event))
                      :count (str (inc (:payload event)))))"
                  "(defn render [{:keys [kind payload]}]
                    (case kind
                      (:text :markdown) (str/upper-case payload)
                      (str (inc payload))))"
                  "(defn render [event]
                    (let [{tag :kind :keys [payload]} event]
                      (case tag
                        (:text :markdown) (str/upper-case (:payload event))
                        (str (inc payload)))))"
                  "(defn render [{:keys [payload] :as event}]
                    (case (:kind event)
                      :count (str (inc payload))
                      (str/upper-case payload)))"
                  "(defn render [event]
                    (case (:kind event)
                      :count (str (inc (:payload event)))
                      (let [{:keys [payload]} event] (str/upper-case payload))))"
                  "(defn- render-if-valid ^String [event]
                    (if (s/valid? ::event event)
                      (let [{:keys [kind payload]} event]
                        (case kind
                          (:text :markdown) (str/upper-case payload)
                          :count (str (inc payload))))
                      \"invalid\"))"]]
    (is (empty? (diagnostics source)) source)))

(deftest case-diagnoses-wrong-payload-and-missing-tags
  (let [ds (diagnostics "(defn render [event]
                            (case (:kind event)
                              :text (str (inc (:payload event)))
                              :wrong (str/trim 1)
                              :count (str (inc (:payload event)))))")]
    (is (= ["TSP8001" "TSP2345" "TSP2678"] (mapv :code ds)) (pr-str ds))
    (is (str/includes? (:message (first ds)) ":markdown"))
    (is (str/includes? (:message (last ds)) ":wrong"))))

(deftest case-keeps-all-variants-sharing-a-tag
  (let [ds (diagnostics
            "(s/def :number/kind #{:text})
             (s/def ::number-text (s/keys :req-un [:number/kind :count/payload]))
             (s/def ::event (s/or :string ::text :number ::number-text))
             (defn render [event]
               (case (:kind event)
                 :text (str/trim (:payload event))
                 (str/trim (:payload event))))")]
    (is (= ["TSP2345"] (mapv :code ds)) (pr-str ds))))

(deftest case-narrows-tag-values-and-leaves-other-scopes-alone
  (let [source "(defn render [{:keys [kind payload] :as event}]
                  (case kind
                    :text (name (:kind event))
                    :markdown (str/upper-case payload)
                    :count (str (inc payload))))"
        res (checked source)
        line (nth (str/split-lines source) 2)
        row (+ (count (str/split-lines specs)) 3)
        col (inc (str/index-of line "(:kind event)"))]
    (is (empty? (:diagnostics res)))
    (is (= ":text" (main/type-at res "events.clj" row col))))
  (testing "a new binding with the same name keeps its own type"
    (let [ds (diagnostics "(defn render [{:keys [kind payload] :as event}]
                              (let [payload 1 event {:payload 1}]
                                (case kind
                                  (:text :markdown) (str/trim payload)
                                  (str/trim (:payload event)))))")]
      (is (= ["TSP2345" "TSP2345"] (mapv :code ds)) (pr-str ds))))
  (testing "narrowing is local to the case branch"
    (let [ds (diagnostics "(defn render [{:keys [kind payload]}]
                              (case kind (:text :markdown) (str/trim payload) (str (inc payload)))
                              (str/trim payload))")]
      (is (= ["TSP2345"] (mapv :code ds)) (pr-str ds)))))

(deftest case-only-correlates-required-finite-tags
  (doseq [event ["(s/or :string ::text :number (s/keys :req-un [:count/payload] :opt-un [:count/kind]))"
                 "(s/or :string ::text :number (s/keys :req-un [:open/kind :count/payload]))"
                 "(s/nilable ::text)"]]
    (let [ds (diagnostics (str "(s/def :open/kind keyword?) (s/def ::event " event ")
                               (defn render [event] (case (:kind event) :text (str/trim (:payload event)) \"other\"))"))]
      (is (= [(if (= "(s/nilable ::text)" event) "TSP2531" "TSP2345")] (mapv :code ds)) (pr-str ds)))))

(deftest case-supports-nil-and-integer-tag-domains
  (is (empty? (diagnostics "(s/def :text/kind (s/nilable #{:text}))
                            (defn render [event]
                              (case (:kind event)
                                (nil :text) (str/upper-case (:payload event))
                                :count (str (inc (:payload event)))))")))
  (is (empty? (diagnostics "(s/def :text/kind #{-1}) (s/def :count/kind #{1})
                            (defn render [event]
                              (case (:kind event)
                                -1 (str/upper-case (:payload event))
                                1 (str (inc (:payload event)))))")))
  (testing "hash case can miss a numerically equal value, so the default can still run"
    (let [ds (diagnostics "(s/def :text/kind #{1.0M}) (s/def ::event ::text)
                            (defn render [event]
                              (case (:kind event) 1.0M \"matched\" (str/trim 42)))")]
      (is (= ["TSP2345"] (mapv :code ds)) (pr-str ds)))))

(deftest case-reports-impossible-tags-including-non-text-domains
  (testing "nil is a possible tag and the typo suggestion uses the textual alternative"
    (let [ds (diagnostics "(s/def :text/kind (s/nilable #{:text})) (s/def ::event ::text)
                            (defn render [event]
                              (case (:kind event) nil \"none\" :tex \"text\" \"other\"))")]
      (is (= ["TSP2678"] (mapv :code ds)) (pr-str ds))
      (is (str/ends-with? (:message (first ds)) "Did you mean :text?"))))
  (testing "invalid tags still get diagnostics for boolean and integer alternatives"
    (doseq [[domain body] [["boolean?" "true \"yes\" false \"no\" :unknown \"other\""]
                           ["#{0 1}" "0 \"off\" 1 \"on\" 2 \"other\""]]]
      (let [ds (diagnostics (str "(s/def ::event " domain ") (defn render [event] (case event " body "))"))]
        (is (= ["TSP2678"] (mapv :code ds)) (pr-str ds))
        (is (not (str/includes? (:message (first ds)) "Did you mean"))))))
  (testing "a valid nil tag still participates in exhaustiveness"
    (is (empty? (diagnostics "(s/def :text/kind (s/nilable #{:text})) (s/def ::event ::text)
                               (defn render [event] (case (:kind event) nil \"none\" :text \"text\"))")))))

(deftest equality-refines-both-branches-and-cond-residue
  (doseq [body ["(if (= :count (:kind event))
                    (str (inc (:payload event)))
                    (str/upper-case (:payload event)))"
                "(let [{:keys [kind payload]} event]
                    (cond (= kind :text) (str/upper-case payload)
                          (= :markdown kind) (str/upper-case payload)
                          (= kind :count) (str (inc payload))))"
                "(if (or (= (:kind event) :text) (= (:kind event) :markdown))
                    (str/upper-case (:payload event))
                    (str (inc (:payload event))))"
                "(if (and (not (= (:kind event) :text)) (not= :other :count)
                           (not (= (:kind event) :markdown)))
                    (str (inc (:payload event)))
                    (str/upper-case (:payload event)))"
                "(let [numeric (= (:kind event) :count)]
                    (if numeric (str (inc (:payload event))) (str/upper-case (:payload event))))"]]
    (let [ds (diagnostics (str "(defn render [event] " body ")"))]
      (is (empty? ds) (pr-str {:body body :diagnostics ds}))))
  (testing "a reachable cond fallthrough returns nil"
    (let [ds (diagnostics "(defn render [{:keys [kind payload]}]
                             (cond (= kind :text) (str/upper-case payload)
                                   (= kind :count) (str (inc payload))))")]
      (is (= ["TSP2322"] (mapv :code ds)) (pr-str ds)))))

(deftest nested-tag-paths-and-aliases
  (doseq [body ["(if (= :count (get-in event [:meta :kind]))
                    (str (inc (get-in event [:meta :payload])))
                    (str/upper-case (:payload (:meta event))))"
                "(let [meta (get event :meta) tag (:kind meta)]
                    (case tag
                      :count (str (inc (:payload meta)))
                      (str/upper-case (get-in event [:meta :payload]))))"
                "(let [{{:keys [kind payload]} :meta} event]
                    (if (= kind :count) (str (inc payload)) (str/upper-case payload)))"
                "(case (:kind (:meta event))
                    (:text :markdown) (str/upper-case (:payload (:meta event)))
                    :count (str (inc (:payload (:meta event)))))"]]
    (let [ds (diagnostics (str "(s/def :envelope/meta ::event)
                                (s/def ::envelope (s/keys :req-un [:envelope/meta]))
                                (s/fdef render :args (s/cat :event ::envelope) :ret string?)
                                (defn render [event] " body ")"))]
      (is (empty? ds) (pr-str {:body body :diagnostics ds})))))

(deftest condp-refines-and-checks-coverage
  (is (empty? (diagnostics "(defn render [event]
                            (condp = (:kind event)
                              :text (str/upper-case (:payload event))
                              :markdown (str/upper-case (:payload event))
                              :count :>> (fn [matched] (if matched (str (inc (:payload event))) \"\"))))")))
  (let [ds (diagnostics "(defn render [event]
                            (condp = (:kind event) :count (str (inc (:payload event)))))")]
    (is (= ["TSP8001"] (mapv :code ds)) (pr-str ds))
    (is (str/includes? (:message (first ds)) ":markdown")))
  (is (empty? (diagnostics "(defn render [event]
                            (condp = (:kind event)
                              :count (str (inc (:payload event)))
                              (str/upper-case (:payload event))))"))))

(deftest contains-preserves-present-nil
  (doseq [[field sources] [["string?" ["(if (contains? event :note) (str/upper-case (:note event)) \"missing\")"
                                      "(let [{:keys [note]} event]
                                         (if (contains? event :note) (str/upper-case note) \"missing\"))"]]
                           ["(s/nilable string?)" ["(if (and (contains? event :note) (some? (:note event)))
                                                       (str/upper-case (:note event)) \"missing\")"]]]
          source sources]
    (let [ds (diagnostics (str "(s/def :event/note " field ")
                                (s/def ::event (s/keys :opt-un [:event/note]))
                                (defn render [event] " source ")"))]
      (is (empty? ds) (pr-str ds))))
  (let [ds (diagnostics "(s/def :event/note (s/nilable string?))
                        (s/def ::event (s/keys :opt-un [:event/note]))
                        (defn render [event]
                          (if (contains? event :note) (str/upper-case (:note event)) \"missing\"))")]
    (is (= ["TSP2531"] (mapv :code ds)) (pr-str ds))))

(deftest conditional-facts-respect-bindings-and-predicate-identity
  (doseq [source ["(defn render [event]
                    (let [= (fn [_ _] true)]
                      (if (= (:kind event) :count) (str (inc (:payload event))) \"other\")))"
                  "(s/fdef current :args (s/cat :event ::event) :ret ::event)
                   (defn current [event] event)
                   (defn render [event]
                     (if (= (:kind (current event)) :count)
                       (str (inc (:payload (current event)))) \"other\"))"
                  "(defn render [event]
                     (let [tag (:kind event) numeric (= tag :count) tag :count]
                       (if numeric (str (inc (:payload event))) \"other\")))"]]
    (let [ds (diagnostics source)]
      (is (= ["TSP2345"] (mapv :code ds)) (pr-str ds))))
  (is (empty? (diagnostics "(s/def ::event (s/nilable string?))
                            (defn render [event]
                              (let [present (some? event)]
                                (if present (str/upper-case event) \"missing\")))"))))

(deftest new-spec-variant-updates-coverage
  (let [source "(s/def :reset/kind #{:reset})
                (s/def ::reset (s/keys :req-un [:reset/kind]))
                (s/def ::event (s/or :string ::text :number ::count :reset ::reset))
                (defn render [event]
                  (case (:kind event) (:text :markdown) \"text\" :count \"count\""
        ds (diagnostics (str source "))"))]
    (is (= ["TSP8001"] (mapv :code ds)) (pr-str ds))
    (is (str/includes? (:message (first ds)) ":reset"))
    (is (empty? (diagnostics (str source " :reset \"reset\"))"))))))

(deftest runtime-predicates-retain-safe-residue
  (is (empty? (diagnostics "(s/def ::event (s/or :text string? :count int?))
                            (defn render [event]
                              (if (string? event) (str/upper-case event) (str (inc event))))")))
  (let [ds (diagnostics "(s/def ::event #{1})
                        (defn render [event]
                          (if (instance? Long event) \"long\" (str/trim 1)))")]
    (is (= ["TSP2345"] (mapv :code ds)) (pr-str ds)))
  (let [ds (diagnostics "(s/def ::event t/unknown)
                        (defn render [event]
                          (if (instance? Object event) (str/upper-case event) \"nil\"))")]
    (is (= ["TSP210-01"] (mapv :code ds)) (pr-str ds)))
  (let [ds (diagnostics "(defn render [event]
                          (if (= (:kind event) :count)
                            (let [tag (:kind event)
                                  event (assoc event :kind :other :payload \"changed\")]
                              (if (= tag :count) (str (inc (:payload event))) \"\"))
                            \"\"))")]
    (is (= ["TSP2345"] (mapv :code ds)) (pr-str ds))))
