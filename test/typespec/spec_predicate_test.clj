(ns typespec.spec-predicate-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is]]
            [typespec.check :as check]
            [typespec.parse :as parse]
            [typespec.project-contracts :as contracts]
            [typespec.spec-conform :as conform]
            [typespec.types :as t]))

(s/def ::http-method #(contains? #{:get :post :put :delete :patch :head} %))
(s/def ::middleware #(or (list? %) (vector? %)))

(def ^:private api
  (parse/parse-string "api.clj"
                      "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::method #(contains? #{:get :post :put :delete :patch :head} %))
(s/def ::middleware #(or (list? %) (vector? %)))
(s/def ::maybe #(clojure.core/contains? #{nil false :ok} %1))
(s/def ::property (s/or :method ::method :middleware ::middleware))
(s/fdef method :args (s/cat :x ::method) :ret any?)
(defn method [x] x)
(s/fdef middleware :args (s/cat :x ::middleware) :ret any?)
(defn middleware [x] x)"))

(deftest spec-predicate-types-agree-with-runtime-on-known-values
  (let [cases (concat
               (for [value [:get :post :put :delete :patch :head :trace "get" nil false]]
                 ["method" (pr-str value) (s/valid? ::http-method value)])
               (for [[form value] [["()" '()]
                                   ["'(:auth)" '(:auth)]
                                   ["(list :auth)" (list :auth)]
                                   ["[]" []]
                                   ["[:auth]" [:auth]]
                                   ["{:a 1}" {:a 1}]
                                   ["#{:auth}" #{:auth}]
                                   ["nil" nil]
                                   ["(clojure.lang.Cons. :auth '(:more))" (cons :auth '(:more))]]]
                 ["middleware" form (s/valid? ::middleware value)]))
        client (parse/parse-string "client.clj"
                                   (str "(ns client (:require [api :as api]))\n"
                                        (apply str (for [[method form] cases]
                                                     (str "(api/" method " " form ")\n")))))
        issues (:diagnostics (check/check-files [client api] {}))]
    (is (= (keep-indexed (fn [i [_ _ valid?]] (when-not valid? [(+ i 2) "TSP2345"])) cases)
           (mapv (juxt (comp :row :pos) :code) issues)) (pr-str issues))
    (is (every? #(= "client.clj" (:file %)) issues))))

(deftest preserves-literal-sets-and-conformed-payload-types
  (let [env (:env (check/check-files [api] {}))
        maybe (t/ref-t :api/maybe)
        conform-type (conform/tagged-result env nil :api/property)
        methods (some (fn [branch]
                        (when (= :method (get-in branch [:items 0 :val]))
                          (get-in branch [:items 1])))
                      (t/members env conform-type))]
    (is (every? #(t/sub? env % maybe) [t/nil-t (t/lit false) (t/lit :ok)]))
    (is (= :mismatch (:status (t/sub-result env (t/lit true) maybe))))
    (is (= :proved (:status (t/sub-result env (t/lit :get) methods))))
    (is (= :mismatch (:status (t/sub-result env (t/lit :trace) methods))))))

(deftest leaves-unresolved-and-shadowed-predicates-opaque
  (let [source (parse/parse-string "schema.clj"
                                   "(ns schema (:require [clojure.spec.alpha :as s]))
(s/def ::local #(contains? #{:ok} %))
(s/def ::dynamic-set #(clojure.core/contains? #{status} %))
(s/def ::other-argument #(clojure.core/contains? #{:ok} %2))
(s/def ::parameter-call (fn [vector?] (vector? vector?)))
(s/def ::mixed #(or (list? %) (custom-predicate %)))
(s/def ::not-the-argument (fn [x] (instance? String value)))
(s/def ::protocol #(vector? %))
(s/def ::hinted (fn [^long x] (clojure.core/contains? #{:ok} x)))
(s/def ::qualified (clojure.core/fn [x] (clojure.core/contains? #{:ok} x)))")
        defs (parse/parse-string "predicates.clj"
                                 "(ns schema)
(defn contains? [_ _] true)
(defprotocol Custom (vector? [x]))")
        excluded (parse/parse-string "excluded.clj"
                                     "(ns excluded (:refer-clojure :exclude [contains?])
                                         (:require [clojure.spec.alpha :as s]))
(s/def ::value #(contains? #{:ok} %))")
        result (check/check-files [source excluded defs] {})
        issues (:diagnostics result)]
    (is (= (into [["excluded.clj" 3 "TSP9002"]]
                 (map #(vector "schema.clj" % "TSP9002") (range 2 10)))
           (mapv (juxt :file (comp :row :pos) :code) issues)) (pr-str issues))
    (is (= #{:ok} (into #{} (map :val) (t/members (:env result) (t/ref-t :schema/qualified)))))))

(deftest spec-predicates-resolve-through-namespace-mappings
  (let [custom (parse/parse-string "custom.clj"
                                  "(ns custom (:refer-clojure :exclude [string?]))
(defn string? [x] (keyword? x))")
        api (parse/parse-string "api.clj"
                               "(ns api (:refer-clojure :rename {string? text?})
  (:require [clojure.spec.alpha :as sp] [custom :refer [string?]]))
(sp/def ::category string?)
(sp/def ::title text?)
(sp/def ::summary #(text? %))
(sp/def ::entry (sp/keys :req-un [::title]))
(sp/fdef category :args (sp/cat :x ::category) :ret any?)
(sp/fdef entry :args (sp/cat :x ::entry :summary ::summary) :ret any?)
(defn category [x] x)
(defn entry [x summary] x)
(defn explicit {:t/sig '[string? :-> string?]} [x] x)
(sp/def ::tagged (sp/or :text text? :other string?))")
        client (parse/parse-string "client.clj"
                                  "(ns client (:require [api :as a]))
(a/category :valid)
(a/entry {:title \"valid\"} \"summary\")
(a/entry {:title 42} \"summary\")
(a/entry {:title \"valid\"} 42)
(a/explicit \"valid\")
(a/explicit :wrong)")
        {:keys [diagnostics env]} (check/check-files [client api custom] {})
        output (conform/tagged-result env nil :api/tagged)
        payloads (into {} (keep (fn [part]
                                 (when-let [[tag payload] (:items part)]
                                   [(:val tag) payload]))) (t/members env output))]
    (is (= [["api.clj" 3 "TSP9002"] ["api.clj" 12 "TSP9002"]
            ["client.clj" 4 "TSP2322"] ["client.clj" 5 "TSP2345"] ["client.clj" 7 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics))
    (is (= {:text (t/prim :string) :other t/unknown} payloads))))

(deftest boolean-literal-predicates-check-spec-fields-and-returns
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::enabled true?)
(s/def ::preview #(clojure.core/false? %))
(s/def ::config (s/keys :req-un [::enabled ::preview]))
(s/fdef start :args (s/cat :config ::config) :ret true?)
(defn start [{:keys [enabled]}] enabled)
(s/fdef stop :args (s/cat) :ret false?)
(defn stop [] true)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api]))
(api/start {:enabled true :preview false})
(api/start {:enabled false :preview false})
(api/start {:enabled true :preview true})
(api/start {:enabled :yes :preview false})")
        result (check/check-files [client api] {})]
    (is (= [["api.clj" 8 "TSP2322"] ["client.clj" 3 "TSP2322"] ["client.clj" 4 "TSP2322"]
            ["client.clj" 5 "TSP2322"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))))

(deftest boolean-coercion-supplies-a-boolean-to-spec-contracts
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/fdef enabled? :args (s/cat :value any?) :ret boolean?)
(defn enabled? [value] (boolean value))
(s/fdef label :args (s/cat :enabled boolean?) :ret string?)
(defn label [enabled] (if enabled \"yes\" \"no\"))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(a/label (boolean :enabled))
(a/label (apply boolean [nil]))
(boolean)
(inc (boolean :enabled))")
        issues (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 4 "TSP2554"] ["client.clj" 5 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))
    (is (= [false false true true] (mapv boolean [nil false 0 :enabled])))))

(deftest literal-spec-sets-use-the-members-truthiness
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::mode #{nil false :auto})
(s/def ::none #{})
(s/def ::tagged (s/or :mode ::mode :count int?))
(s/fdef select-mode :args (s/cat :mode ::mode) :ret ::mode)
(s/fdef reject :args (s/cat :value ::none) :ret any?)
(defn select-mode [mode] mode)
(defn reject [value] value)
(defn literal-type-only {:t/sig '[#{false} :-> #{false}]} [x] x)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(a/select-mode nil)
(a/select-mode false)
(a/select-mode :auto)
(a/select-mode true)
(a/reject :auto)
(a/literal-type-only false)
(a/literal-type-only true)")
        {:keys [diagnostics env]} (check/check-files [client api] {})
        conformed (t/members env (conform/tagged-result env nil :api/tagged))]
    (is (= [["client.clj" 2 "TSP2345"] ["client.clj" 3 "TSP2345"] ["client.clj" 5 "TSP2345"]
            ["client.clj" 6 "TSP2345"] ["client.clj" 8 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics))
    (is (= (t/lit :auto)
           (some #(when (= :mode (get-in % [:items 0 :val])) (get-in % [:items 1])) conformed)))
    (is (= [false false true false] (mapv #(s/valid? #{nil false :auto} %) [nil false :auto true])))
    (is (not (s/valid? #{} :auto)))))

(deftest spec-and-narrows-through-value-preserving-predicates
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::count (s/and number? int? pos?))
(s/def ::label (s/and some? string?))
(s/def ::converted (s/and number? (s/conformer str) string?))
(s/def ::tagged (s/and (s/or :number int? :text string?) vector?))
(s/fdef show :args (s/cat :n ::count :label ::label) :ret string?)
(s/fdef convert :args (s/cat :x ::converted) :ret any?)
(s/fdef tag :args (s/cat :x ::tagged) :ret any?)
(defn show [n label] (subs label n))
(defn convert [x] x)
(defn tag [x] x)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(a/show 1 \"text\")
(a/show 1.5 \"text\")
(a/show 1 :wrong)
(a/show 1 nil)
(a/convert 1.5)
(a/convert \"text\")
(a/tag 1)
(a/tag \"text\")
(a/tag :wrong)")
        diagnostics (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 3 "TSP2345"] ["client.clj" 4 "TSP2345"] ["client.clj" 5 "TSP2345"]
            ["client.clj" 7 "TSP2345"] ["client.clj" 10 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics))
    (is (s/valid? (s/and number? (s/conformer str) string?) 1.5))
    (is (s/valid? (s/and (s/or :number int? :text string?) vector?) 1))))

(deftest jvm-value-predicates-preserve-tuple-contracts
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::delimiter #(char? %))
(s/def ::payload bytes?)
(s/def ::kind class?)
(s/fdef describe :args (s/cat :value (s/tuple ::delimiter ::payload ::kind)) :ret string?)
(defn describe [[^Character delimiter ^bytes payload ^Class kind]]
  (str (.charValue delimiter) (alength payload) (.getName kind)))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]) (:import [java.net URI]))
(a/describe [\\: (byte-array [1 2]) String])
(a/describe [\\: (byte-array [1 2]) URI])
(a/describe [\":\" (byte-array [1 2]) String])
(a/describe [\\: (int-array [1 2]) String])
(a/describe [\\: (byte-array [1 2]) \"String\"])")
        diagnostics (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 4 "TSP2345"] ["client.clj" 5 "TSP2345"] ["client.clj" 6 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics)))
  (is (s/valid? (s/tuple char? bytes? class?) [\: (byte-array [1]) java.net.URI]))
  (is (not (s/valid? (s/tuple char? bytes? class?) [\: (int-array [1]) java.net.URI]))))

(deftest instance-predicates-resolve-core-vars-and-respect-shadowing
  (let [api (parse/parse-string "api.clj" "(ns api (:refer-clojure :rename {instance? is-instance? partial bind-first})
  (:require [clojure.spec.alpha :as s]))
(defn instance? [_ value] (string? value))
(s/def ::file (bind-first is-instance? java.io.File))
(s/def ::custom (bind-first instance? java.io.File))
(s/def ::object (clojure.core/partial clojure.core/instance? Object))
(s/def ::shadow (fn [String] (is-instance? String String)))
(s/def ::wrong-arity (bind-first is-instance? java.io.File :extra))
(s/fdef file :args (s/cat :value ::file) :ret any?)
(s/fdef custom :args (s/cat :value ::custom) :ret any?)
(s/fdef present :args (s/cat :value ::object) :ret any?)
(defn file [x] x)
(defn custom [x] x)
(defn present [x] x)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(a/file (java.io.File. \"test\"))
(a/file \"not a file\")
(a/custom \"text\")
(a/present false)
(a/present nil)")
        diagnostics (:diagnostics (check/check-files [client api] {}))]
    (is (= [["api.clj" 5 "TSP9002"] ["api.clj" 7 "TSP9002"] ["api.clj" 8 "TSP9002"]
            ["client.clj" 3 "TSP2345"] ["client.clj" 6 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics))))

(deftest predicate-conjunctions-retain-known-input-types
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::label #(and (string? %) (< (count %) 8)))
(s/def ::count (fn [n] (and (number? n) (int? n) (pos? n))))
(s/fdef render :args (s/cat :label ::label :count ::count) :ret string?)
(defn render [label count] (str (subs label 0) count))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(a/render \"valid\" 2)
(a/render :wrong 2)
(a/render \"valid\" 2.5)
(a/render \"valid\" 0)")
        issues (:diagnostics (check/check-files [client api] {}))]
    (is (= [["api.clj" 2 "TSP9002"]
            ["client.clj" 3 "TSP2345"] ["client.clj" 4 "TSP2345"] ["client.clj" 5 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))
    (is (re-find #"\(< \(count %\) 8\)" (:message (first issues))))
    (is (re-find #"0 is not assignable to :api/count \(\(s/and int\? pos\?\)\)" (:message (last issues)))))
  (let [short-text #(and (string? %) (< (count %) 8))]
    (is (= [true false false]
           (mapv #(s/valid? short-text %) ["valid" :wrong "longer-than-limit"])))))

(deftest spec-and-narrows-named-value-preserving-contracts
  (let [schema (parse/parse-string "schema.clj" "(ns schema (:require [clojure.spec.alpha :as spec]))
(spec/def ::maybe-title (spec/nilable string?))
(spec/def ::alias ::maybe-title)
(spec/def ::tagged (spec/or :number int? :text string?))")
        api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s] [schema :as schema]))
(s/def ::title (s/and ::schema/alias some?))
(s/def ::entry (s/and ::schema/tagged vector?))
(s/fdef title :args (s/cat :x ::title) :ret string?)
(s/fdef entry :args (s/cat :x ::entry) :ret any?)
(defn title [x] (subs x 0))
(defn entry [x] x)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(a/title \"hello\")
(a/title nil)
(a/title 42)
(a/entry 1)
(a/entry \"text\")")
        {:keys [env diagnostics]} (check/check-files [client api schema] {})
        signature (get-in env [:vars 'api/title :type])
        dependencies (contracts/dependency-paths (:specs env) signature)]
    (is (= [["client.clj" 3 "TSP2345"] ["client.clj" 4 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics))
    (is (= [:api/title :schema/alias :schema/maybe-title]
           (get dependencies :schema/maybe-title)))))

(deftest numeric-spec-predicates-check-known-values
  (let [domain (parse/parse-string "domain.clj" "(ns domain (:require [clojure.spec.alpha :as s]))
(s/def ::id pos-int?)
(s/def ::offset nat-int?)
(s/def ::adjustment neg-int?)
(s/def ::retries (s/and int? pos?))
(s/def ::response (s/keys :req-un [::id ::offset ::adjustment ::retries]))")
        api (parse/parse-string "api.clj" "(ns api (:require [domain :as d] [clojure.spec.alpha :as s]))
(s/fdef process-response :args (s/cat :response ::d/response) :ret int?)
(defn process-response [{:keys [id offset adjustment retries]}] (+ id offset adjustment retries))
(s/fdef wrong-result :args (s/cat) :ret ::d/id)
(defn wrong-result [] 0)
(s/fdef consume :args (s/cat :value (s/and number? neg?)) :ret number?)
(defn consume [value] value)
(s/fdef tolerance :args (s/cat :value (s/and number? zero?)) :ret number?)
(defn tolerance [value] value)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(a/process-response {:id 1 :offset 0 :adjustment -1 :retries 2})
(a/process-response {:id 0 :offset 0 :adjustment -1 :retries 2})
(a/process-response {:id 1 :offset -1 :adjustment -1 :retries 2})
(a/process-response {:id 1 :offset 0 :adjustment 1 :retries 2})
(a/process-response {:id 1 :offset 0 :adjustment -1 :retries -2})
(a/process-response {:id 1N :offset 0 :adjustment -1 :retries 2})
(a/consume -0.5)
(a/consume -0.0)
(a/tolerance -0.0)
(a/tolerance 0.1)")
        issues (:diagnostics (check/check-files [client api domain] {}))]
    (is (= (concat [["api.clj" 5 "TSP2322"]]
                   (map #(vector "client.clj" % "TSP2322") (range 3 8))
                   [["client.clj" 9 "TSP2345"] ["client.clj" 11 "TSP2345"]])
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))
    (is (re-find #"\(s/and int\? pos\?\)" (:message (nth issues 4)))))
  (is (= [true false false]
         (mapv #(s/valid? pos-int? %) [1 0 1N])))
  (is (= [true true false]
         (mapv #(s/valid? zero? %) [0 -0.0 0.1]))))

(deftest spec-and-applies-shape-contracts-after-value-preserving-predicates
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::id int?)
(s/def ::record (s/and map? (s/keys :req-un [::id])))
(s/def ::pair (s/and vector? (s/tuple string? int?)))
(s/fdef sum-id :args (s/cat :row ::record :pair ::pair) :ret int?)
(defn sum-id [{:keys [id]} [label suffix]] (+ id suffix))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(a/sum-id {:id 1} [\"good\" 2])
(a/sum-id {} [\"good\" 2])
(a/sum-id {:id \"1\"} [\"good\" 2])
(a/sum-id {:id 1} [\"good\" \"2\"])
(a/sum-id {:id 1} [\"short\"])")
        diagnostics (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 3 "TSP2741"] ["client.clj" 4 "TSP2322"]
            ["client.clj" 5 "TSP2345"] ["client.clj" 6 "TSP330-01"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics)))
  (is (= [true false false]
         (mapv #(s/valid? (s/and vector? (s/tuple string? int?)) %)
               [["good" 2] ["good" "2"] ["short"]]))))

(deftest spec-and-imports-named-map-and-tuple-input-shapes
  (let [schema (parse/parse-string "schema.clj" "(ns schema (:require [clojure.spec.alpha :as spec]))
(spec/def ::id int?)
(spec/def ::row (spec/keys :req-un [::id]))
(spec/def ::row-alias ::row)
(spec/def ::pair (spec/tuple string? int?))")
        api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s] [schema :as schema]))
(s/def ::record (s/and map? ::schema/row-alias))
(s/def ::entry (s/and vector? ::schema/pair))
(s/fdef total :args (s/cat :row ::record :entry ::entry) :ret int?)
(defn total [{:keys [id]} [_ count]] (+ id count))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(a/total {:id 1} [\"valid\" 2])
(a/total {} [\"valid\" 2])
(a/total {:id \"1\"} [\"valid\" 2])
(a/total {:id 1} [\"valid\" \"2\"])")
        {:keys [env diagnostics]} (check/check-files [client api schema] {})
        signature (get-in env [:vars 'api/total :type])
        dependencies (contracts/dependency-paths (:specs env) signature)]
    (is (= [["client.clj" 3 "TSP2741"] ["client.clj" 4 "TSP2322"] ["client.clj" 5 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics))
    (is (= [:api/record :schema/row-alias :schema/row] (get dependencies :schema/row)))
    (is (= [:api/entry :schema/pair] (get dependencies :schema/pair)))))

(deftest spec-and-preserves-homogeneous-collection-inputs
  (let [domain (parse/parse-string "domain.clj" "(ns domain
  (:refer-clojure :exclude [vector?])
  (:require [clojure.spec.alpha :as spec]))
(defn vector? [x] (map? x))
(spec/def ::by-name (spec/map-of string? int?))
(spec/def ::counts (spec/coll-of int? :kind clojure.core/vector? :min-count 1))
(spec/def ::custom (spec/coll-of any? :kind vector?))")
        api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s] [domain :as d]))
(s/fdef read-index :args (s/cat :m (s/and map? ::d/by-name)) :ret int?)
(defn read-index [m] (count m))
(s/fdef read-counts :args (s/cat :v (s/and vector? ::d/counts)) :ret int?)
(defn read-counts [v] (count v))
(s/fdef read-custom :args (s/cat :m (s/and coll? ::d/custom)) :ret int?)
(defn read-custom [m] (count m))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(a/read-index {\"n\" 1})
(a/read-index {\"n\" \"one\"})
(a/read-counts [2])
(a/read-counts [\"two\"])
(a/read-counts [])
(a/read-custom {:n 1})")
        diagnostics (:diagnostics (check/check-files [client api domain] {}))]
    (is (= [["client.clj" 3 "TSP2345"] ["client.clj" 5 "TSP2345"] ["client.clj" 6 "TSP330-01"]
            ["domain.clj" 7 "TSP9002"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics))))

(deftest spec-and-combines-map-checks-when-earlier-specs-preserve-values
  (let [base (parse/parse-string "base.clj" "(ns base (:require [clojure.spec.alpha :as s]))
(s/def ::id int?)
(s/def ::identity (s/keys :req-un [::id]))
(s/def ::unchanged (s/nonconforming ::identity))")
        details (parse/parse-string "details.clj" "(ns details (:require [clojure.spec.alpha :as s]))
(s/def ::name string?)
(s/def ::named (s/keys :req-un [::name]))")
        api (parse/parse-string "api.clj" "(ns api (:require [base :as b] [details :as d] [clojure.spec.alpha :as s] [clojure.string :as str]))
(s/def ::row (s/and ::b/unchanged ::d/named))
(s/def ::prefixed (s/and map? (s/nonconforming ::b/identity) ::d/named))
(s/fdef render :args (s/cat :m ::row) :ret string?)
(defn render [{:keys [id name]}] (str (inc id) (str/trim name)))
(s/fdef prefixed :args (s/cat :m ::prefixed) :ret string?)
(defn prefixed [{:keys [id name]}] (str (inc id) (str/trim name)))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(a/render {:id 1 :name \"Ada\"})
(a/render {:id 1})
(a/render {:name \"Ada\"})
(a/render {:id 1 :name 42})
(a/prefixed {:id 1 :name \"Ada\"})
(a/prefixed {:id 1})")
        {:keys [env diagnostics]} (check/check-files [client api details base] {})
        signature (get-in env [:vars 'api/render :type])
        dependencies (contracts/dependency-paths (:specs env) signature)]
    (is (= [["client.clj" 3 "TSP2741"] ["client.clj" 4 "TSP2741"]
            ["client.clj" 5 "TSP2322"] ["client.clj" 7 "TSP2741"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics))
    (is (= [:api/row :base/unchanged :base/identity] (get dependencies :base/identity)))
    (is (= [:api/row :details/named] (get dependencies :details/named)))))

(deftest spec-and-combines-map-checks-for-untouched-fields
  (let [base (parse/parse-string "base.clj" "(ns base (:require [clojure.spec.alpha :as s]))
(s/def ::id int?)
(s/def ::identity (s/keys :req-un [::id]))
(s/def ::status keyword?)
(s/def ::marked (s/keys :req [::status]))
(s/def ::tagged-id (s/or :number int? :name string?))
(s/def ::transforms-id (s/keys :req-un [::tagged-id]))")
        details (parse/parse-string "details.clj" "(ns details (:require [clojure.spec.alpha :as s]))
(s/def ::name string?)
(s/def ::named (s/keys :req-un [::name]))
(s/def ::tagged-id vector?)
(s/def ::expects-tag (s/keys :req-un [::tagged-id]))")
        api (parse/parse-string "api.clj" "(ns api (:require [base :as b] [details :as d] [clojure.spec.alpha :as s] [clojure.string :as str]))
(s/def ::person (s/and ::b/identity ::d/named))
(s/def ::marked-person (s/and map? ::b/marked ::b/identity ::d/named))
(s/def ::tagged (s/and ::b/transforms-id ::d/expects-tag))
(s/fdef render :args (s/cat :person ::person) :ret string?)
(defn render [{:keys [id name]}] (str (inc id) (str/trim name)))
(s/fdef render-marked :args (s/cat :person ::marked-person) :ret string?)
(defn render-marked [{:keys [id name] ::b/keys [status]}] (str status (inc id) (str/trim name)))
(s/fdef transformable :args (s/cat :x ::tagged) :ret any?)
(defn transformable [x] x)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(a/render {:id 1 :name \"Ada\"})
(a/render {:id 1})
(a/render {:name \"Ada\"})
(a/render {:id 1 :name 42})
(a/render-marked {:base/status :ready :id 1 :name \"Ada\"})
(a/render-marked {:base/status :ready :name \"Ada\"})
(a/render-marked {:base/status :ready :id 1})
(a/transformable {:tagged-id 1})
(a/transformable {:tagged-id [1]})")
        {:keys [diagnostics env]} (check/check-files [client api details base] {})
        signature (get-in env [:vars 'api/render-marked :type])
        dependencies (contracts/dependency-paths (:specs env) signature)]
    (is (= [[3 "TSP2741"] [4 "TSP2741"] [5 "TSP2322"]
            [7 "TSP2741"] [8 "TSP2741"] [10 "TSP2322"]]
           (mapv (juxt (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics))
    (is (every? #(= "client.clj" (:file %)) diagnostics))
    (is (= [:api/marked-person :details/named] (get dependencies :details/named)))))

(deftest collection-kind-resolves-in-the-spec-namespace
  (let [api (parse/parse-string "api.clj" "(ns api
  (:refer-clojure :exclude [vector?] :rename {set? allowed-set?})
  (:require [clojure.spec.alpha :as s] [clojure.core :as core]))
(defn vector? [x] (map? x))
(s/def ::custom (s/coll-of any? :kind vector?))
(s/def ::counts (s/coll-of int? :kind allowed-set?))
(s/def ::indices (s/every int? :kind core/vector?))
(s/def ::letters (s/coll-of char? :kind string? :into []))
(s/fdef emit :args (s/cat :custom ::custom :counts ::counts :indices ::indices :letters ::letters) :ret any?)
(defn emit [custom counts indices letters] [custom counts indices letters])")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(a/emit {:n 1} #{1} [1] \"abc\")
(a/emit {:n 1} [1] [1] \"abc\")
(a/emit {:n 1} #{1} #{1} \"abc\")")
        diagnostics (:diagnostics (check/check-files [client api] {}))]
    (is (= [["api.clj" 5 "TSP9002"] ["api.clj" 8 "TSP9002"]
            ["client.clj" 3 "TSP2345"] ["client.clj" 4 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics))))

(deftest collection-kind-distinguishes-list-and-seq
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::integers (s/coll-of int? :kind list?))
(s/def ::sequence (s/every int? :kind seq?))
(s/fdef list-total :args (s/cat :values ::integers) :ret int?)
(defn list-total [values] (count values))
(s/fdef seq-total :args (s/cat :values ::sequence) :ret int?)
(defn seq-total [values] (count values))
(s/fdef next-list :args (s/cat :values ::integers) :ret ::integers)
(defn next-list [values] values)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a] [clojure.spec.alpha :as s]))
(a/list-total (list 1 2))
(a/list-total '(1 2))
(a/list-total '())
(a/list-total (map identity [1 2]))
(a/seq-total (map identity [1 2]))
(a/list-total '(1 \"2\"))
(a/list-total [1 2])
(a/list-total (a/next-list '(1 2)))
(s/fdef has-list :args (s/cat :items list?) :ret int?)
(defn has-list [items] (a/list-total items))
(defn unknown-list [^clojure.lang.ISeq items] (a/list-total items))
(s/fdef grow :args (s/cat :items ::a/integers) :ret int?)
(defn grow [items] (a/list-total (conj items 3)))")
        issues (:diagnostics (check/check-files [client api] {}))]
    (is (= [[5 "TSP2345"] [7 "TSP2345"] [8 "TSP2345"]
            [11 "TSP210-01"] [12 "TSP210-01"] [14 "TSP210-01"]]
           (mapv (juxt (comp :row :pos) :code) issues))
        (pr-str issues))
    (is (every? #(= "client.clj" (:file %)) issues))
    (is (= [#{:collection-elements} #{:sequence-kind} #{:sequence-kind}]
           (mapv :reasons (drop 3 issues)))))
  (let [custom (reify clojure.lang.IPersistentList
                 (seq [_] (seq [1]))
                 (count [_] 1)
                 (peek [_] 1)
                 (pop [_] ())
                 (cons [_ value] [value 1])
                 (empty [_] ())
                 (equiv [this other] (identical? this other)))]
    (is (s/valid? (s/coll-of int? :kind list?) custom))
    (is (not (s/valid? (s/coll-of int? :kind seq?) custom)))
    (is (= [2 1] (conj custom 2))))
  (is (not (s/valid? (s/coll-of int? :kind list?) (map identity [1 2])))))

(deftest collection-distinctness-rejects-proven-repeats
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::tags (s/coll-of keyword? :kind vector? :distinct true))
(s/def ::numbers (s/every number? :kind vector? :distinct true))
(s/def ::list (s/coll-of int? :kind list? :distinct true))
(s/def ::loose (s/coll-of int? :kind vector? :distinct false))
(s/fdef tags :args (s/cat :values ::tags) :ret ::tags)
(defn tags [values] values)
(s/fdef numbers :args (s/cat :values ::numbers) :ret int?)
(defn numbers [values] (count values))
(s/fdef list-length :args (s/cat :values ::list) :ret int?)
(defn list-length [values] (count values))
(s/fdef loose :args (s/cat :values ::loose) :ret int?)
(defn loose [values] (count values))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(a/tags [:one :two])
(a/tags [])
(a/tags [:one :one])
(a/tags (a/tags [:one :two]))
(defn repeated [x] (a/tags [:one x :one]))
(a/numbers [1 1.0])
(a/numbers [1 1])
(a/list-length '(1 2))
(a/list-length '(1 2 1))
(a/loose [1 1])
(defn unproven [x] (a/tags [x :one]))")
        issues (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 4 "TSP2345"] ["client.clj" 6 "TSP2345"]
            ["client.clj" 8 "TSP2345"] ["client.clj" 10 "TSP2345"] ["client.clj" 12 "TSP210-01"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))
    (is (every? #(= "collection must contain distinct elements" (:message %)) (take 4 issues))))
  (let [spec (s/every number? :kind vector? :distinct true)]
    (is (= [true true false] (mapv #(s/valid? spec %) [[] [1 1.0] [1 1]])))))

(deftest spec-wrappers-keep-cross-namespace-input-constraints
  (let [domain (parse/parse-string "domain.clj" "(ns domain (:require [clojure.spec.alpha :as s]))
(s/def ::id int?)
(s/def ::row (s/nonconforming (s/keys :req-un [::id])))
(s/def ::pair (s/with-gen (s/tuple string? int?) #(throw (ex-info \"generator not invoked\" {}))))
(s/def ::broken (s/with-gen string?))")
        api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s] [domain :as d]))
(s/fdef serve :args (s/cat :row ::d/row :pair ::d/pair) :ret int?)
(defn serve [{:keys [id]} [_ n]] (+ id n))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(a/serve {:id 2} [\"ok\" 3])
(a/serve {:id \"2\"} [\"ok\" 3])
(a/serve {} [\"ok\" 3])
(a/serve {:id 2} [\"ok\" \"3\"])")
        diagnostics (:diagnostics (check/check-files [client api domain] {}))]
    (is (= [["client.clj" 3 "TSP2322"] ["client.clj" 4 "TSP2741"] ["client.clj" 5 "TSP2345"]
            ["domain.clj" 5 "TSP9002"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics))))

(deftest map-of-kind-checks-entry-collections
  (let [api (parse/parse-string "api.clj" "(ns api
  (:refer-clojure :rename {vector? entries?})
  (:require [clojure.spec.alpha :as s]))
(s/def ::pairs (s/map-of string? int? :kind entries?))
(s/def ::index (s/map-of string? int?))
(s/fdef entries :args (s/cat :pairs ::pairs) :ret int?)
(defn entries [pairs] (count pairs))
(s/fdef index :args (s/cat :pairs ::index) :ret int?)
(defn index [pairs] (count pairs))
(s/def ::pairs-only (s/and entries? ::pairs))
(s/fdef checked-entries :args (s/cat :pairs ::pairs-only) :ret int?)
(defn checked-entries [pairs] (count pairs))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(a/entries [[\"good\" 1]])
(a/entries {\"bad\" 1})
(a/entries [[\"wrong\" \"2\"]])
(a/index {\"good\" 1})
(a/index [[\"wrong\" 1]])
(a/checked-entries [[\"valid\" 1]])
(a/checked-entries [[\"wrong\"]])")
        diagnostics (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 3 "TSP2345"] ["client.clj" 4 "TSP2345"] ["client.clj" 6 "TSP2345"]
            ["client.clj" 8 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics))))

(deftest seqable-spec-checks-arrays-and-jvm-collection-interfaces
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::input clojure.core/seqable?)
(s/fdef length :args (s/cat :values ::input) :ret int?)
(defn length [values] (count (seq values)))
(s/fdef wrapped :args (s/cat :values #(seqable? %)) :ret int?)
(defn wrapped [values] (count (seq values)))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(a/length nil)
(a/length {:a 1})
(a/length (int-array [1 2]))
(a/length (into-array String [\"a\"]))
(a/length (StringBuilder. \"ab\"))
(a/length (java.util.ArrayList.))
(a/length 42)
(a/length (java.util.Date.))
(a/length (.iterator (java.util.ArrayList.)))
(a/wrapped (boolean-array [true]))
(a/wrapped true)")
        diagnostics (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 8 "TSP2345"] ["client.clj" 9 "TSP2345"] ["client.clj" 10 "TSP2345"]
            ["client.clj" 12 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics))
    (is (= "42 is not assignable to :api/input (seqable?)" (:message (first diagnostics))))
    (is (= "true is not assignable to seqable?" (:message (peek diagnostics))))))

(deftest counted-spec-distinguishes-constant-time-counting-from-count
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::fast counted?)
(s/def ::wrapped #(clojure.core/counted? %))
(s/fdef length :args (s/cat :value ::fast) :ret int?)
(defn length [value] (count value))
(s/fdef wrapped :args (s/cat :value ::wrapped) :ret int?)
(defn wrapped [value] (count value))
(s/fdef has-count :args (s/cat :value any?) :ret boolean?)
(defn has-count [value] (counted? value))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(inc (a/length {}))
(inc (a/length [1]))
(inc (a/length '(1)))
(inc (a/wrapped #{1}))
(a/length (map inc [1]))
(a/length (java.util.ArrayList.))
(a/length nil)
(a/wrapped (int-array 2))
(defn by-kind [^clojure.lang.ISeq x] (a/length x))
(a/has-count :value)")
        issues (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 6 "TSP2345"] ["client.clj" 7 "TSP2345"]
            ["client.clj" 8 "TSP2345"] ["client.clj" 9 "TSP2345"]
            ["client.clj" 10 "TSP210-01"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues)))
  (is (counted? '(1)))
  (is (not (counted? (map inc [1]))))
  (is (not (counted? (int-array 2)))))

(deftest sequential-spec-checks-the-jvm-marker-across-namespaces
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::ordered sequential?)
(s/def ::wrapped #(clojure.core/sequential? %))
(s/fdef preserve :args (s/cat :value ::ordered) :ret ::ordered)
(defn preserve [value] value)
(s/fdef render :args (s/cat :value ::wrapped) :ret string?)
(defn render [value] (str value))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(a/preserve [1 2])
(a/preserve '(1 2))
(a/preserve (map inc [1 2]))
(a/preserve {:a 1})
(a/preserve #{1})
(a/render [1])
(a/render #{1})
(a/preserve (java.util.ArrayList.))
(defn by-kind [^clojure.lang.ISeq x] (a/preserve x))")
        diagnostics (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 5 "TSP2345"] ["client.clj" 6 "TSP2345"]
            ["client.clj" 8 "TSP2345"] ["client.clj" 9 "TSP2345"]
            ["client.clj" 10 "TSP210-01"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics))
    (is (= #{:sequence-kind} (:reasons (peek diagnostics)))))
  (is (sequential? (map inc [1 2])))
  (is (not (sequential? {:a 1})))
  (is (not (sequential? (java.util.ArrayList.)))))

(deftest associative-and-indexed-specs-check-collection-capabilities
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::entries #(associative? %))
(s/fdef copy-value :args (s/cat :x ::entries) :ret ::entries)
(defn copy-value [x] x)
(s/fdef read-first :args (s/cat :x indexed?) :ret any?)
(defn read-first [x] (nth x 0 nil))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(a/copy-value {:id 1})
(a/copy-value [1])
(a/copy-value #{1})
(a/copy-value (java.util.HashMap.))
(a/read-first [1])
(a/read-first {:id 1})
(a/read-first \"one\")")
        diagnostics (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 4 "TSP2345"] ["client.clj" 5 "TSP2345"]
            ["client.clj" 7 "TSP2345"] ["client.clj" 8 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics))))

(deftest an-unknown-qualified-key-write-remains-inconclusive
  (let [client (parse/parse-string "client.clj"
                                   "(ns client (:require [api]))
(defn- store [method] {:api/method method})
(defn- bad [] {:api/method :trace})")
        issues (fn [opts] (:diagnostics (check/check-files [client api] opts)))
        default (issues {})]
    (is (= [[2 "TSP210-01" :warning] [3 "TSP2322" :error]]
           (mapv (juxt (comp :row :pos) :code :severity) default)) (pr-str default))
    (is (= #{:unknown} (:reasons (first default))))
    (is (= ["TSP210-01" :error] ((juxt :code :severity) (first (issues {:strict true})))))))
