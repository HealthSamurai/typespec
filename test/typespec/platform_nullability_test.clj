(ns typespec.platform-nullability-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [typespec.check :as check]
            [typespec.parse :as parse]
            [typespec.types :as t]))

(def ^:private api
  "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::parent #(instance? java.io.File %))
(s/def ::settings (s/keys :req-un [::parent]))
(s/def ::settings-list (s/coll-of ::settings :kind vector?))
(s/def ::pair (s/tuple int? ::parent))
(s/fdef consume :args (s/cat :m ::settings) :ret any?)
(defn consume [m] m)
(s/fdef consume-list :args (s/cat :xs ::settings-list) :ret any?)
(defn consume-list [xs] xs)
(s/fdef consume-pair :args (s/cat :p ::pair) :ret any?)
(defn consume-pair [p] p)")

(defn- run-project [consumer opts]
  (check/check-files [(parse/parse-string "client.clj"
                                          (str "(ns client (:require [api :as api] [clojure.spec.alpha :as s]))\n"
                                               consumer))
                      (parse/parse-string "api.clj" api)]
                     opts))

(defn- issue [diagnostic]
  ((juxt :file (comp :row :pos) :code :type-path) diagnostic))

(deftest sequence-elements-of-platform-arrays-respect-strict-nil
  (let [source "(let [dir (java.io.File. \".\")]
  (mapv #(.getName ^java.io.File %) (.listFiles dir)))"
        strict (:diagnostics (run-project source {:strict-nil true}))]
    (is (empty? (:diagnostics (run-project source {}))))
    (is (= [["client.clj" 3 "TSP2531" nil]] (mapv issue strict))
        (pr-str strict))))

(deftest java-methods-report-receivers-proved-to-be-nil
  (let [source "(.length \"ok\")
(.length nil)
(.charValue (second \"x\"))
(.charValue (first (next \"x\")))
(let [target nil] (.toString target))"]
    (doseq [opts [{} {:strict-nil true}]]
      (let [ds (:diagnostics (run-project source opts))]
        (is (= [[3 "TSP2531" "Receiver is nil"]
                [4 "TSP2531" "(second \"x\") is nil"]
                [5 "TSP2531" "(first (next \"x\")) is nil"]
                [6 "TSP2531" "target is nil"]]
               (mapv (juxt (comp :row :pos) :code :message) ds))
            (pr-str ds))))))

(deftest expression-hints-preserve-nil-from-project-contracts
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/fdef label :args (s/cat :flag boolean?) :ret (s/nilable string?))
(defn label [flag] (when flag \"hello\"))")
        client (parse/parse-string "client.clj"
                 "(ns client (:require [api :as a] [clojure.spec.alpha :as s]))
(s/fdef bad :args (s/cat :flag boolean?))
(defn bad [flag] (.length ^String (a/label flag)))
(s/fdef good :args (s/cat :flag boolean?))
(defn good [flag]
  (when-let [label (a/label flag)] (.length ^String (identity label))))
(.length ^String (identity nil))")
        diagnostics (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 3 "TSP2531" "(a/label flag) may be nil"]
            ["client.clj" 7 "TSP2531" "(identity nil) is nil"]]
           (mapv (juxt :file (comp :row :pos) :code :message) diagnostics))
        (pr-str diagnostics))))

(deftest local-and-receiver-hints-preserve-known-nil
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/fdef label :args (s/cat :flag boolean?) :ret (s/nilable java.lang.CharSequence))
(defn label [flag] (when flag \"hello\"))")
        client (parse/parse-string "client.clj"
                 "(ns client (:require [api :as a] [clojure.spec.alpha :as s]))
(s/fdef local :args (s/cat :flag boolean?) :ret int?)
(defn local [flag] (let [^String text (a/label flag)] (.length text)))
(s/fdef receiver :args (s/cat :flag boolean?) :ret int?)
(defn receiver [flag] (let [text (a/label flag)] (.length ^String text)))
(s/fdef guarded :args (s/cat :flag boolean?) :ret int?)
(defn guarded [flag]
  (if-some [^String text (a/label flag)] (.length text) 0))")
        diagnostics (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 3 "TSP2531"] ["client.clj" 5 "TSP2531"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics))
    (is (every? #(str/includes? (:message %) "a/label returns (s/nilable java.lang.CharSequence)")
                diagnostics))))

(deftest platform-values-in-maps-and-tuples-are-checked-across-namespaces
  (let [client "(api/consume {:parent (.getParentFile (java.io.File. \"file\"))})
(api/consume-pair [1 (.getParentFile (java.io.File. \"file\"))])
(api/consume-list [{:parent (.getParentFile (java.io.File. \"file\"))}])
(when-let [parent (.getParentFile (java.io.File. \"file\"))]
  (api/consume {:parent parent})
  (api/consume-pair [1 parent])
  (api/consume-list [{:parent parent}]))
(when-let [parent (.getParentFile (java.io.File. \"file\"))]
  (api/consume {:parent (.getParentFile (java.io.File. \"file\"))}))"
        strict (:diagnostics (run-project client {:strict-nil true}))
        source-lines (str/split-lines client)]
    (is (nil? (.getParentFile (java.io.File. "file"))))
    (is (empty? (:diagnostics (run-project client {}))))
    (is (= [["client.clj" 2 "TSP2531" [:parent]]
            ["client.clj" 3 "TSP2531" [1]]
            ["client.clj" 4 "TSP2531" [0 :parent]]
            ["client.clj" 10 "TSP2531" [:parent]]]
           (mapv issue strict)) (pr-str strict))
    (is (= (mapv #(inc (str/index-of (nth source-lines %) "(.getParentFile")) [0 1 2 8])
           (mapv #(get-in % [:pos :col]) strict)))
    (is (every? #(str/includes? (:message %) "may be nil") strict))))

(deftest return-contracts-walk-nested-fields-and-positions
  (let [source "(s/fdef settings :args (s/cat) :ret ::api/settings)
(defn settings [] {:parent (.getParentFile (java.io.File. \"file\"))})
(s/fdef pair :args (s/cat) :ret ::api/pair)
(defn pair [] [1 (.getParentFile (java.io.File. \"file\"))])
(defn stored [] {:parent (.getParentFile (java.io.File. \"file\"))})
(api/consume (stored))
(s/fdef guarded :args (s/cat) :ret (s/nilable ::api/settings))
(defn guarded []
  (when-some [parent (.getParentFile (java.io.File. \"file\"))]
    {:parent parent}))"
        strict (:diagnostics (run-project source {:strict-nil true}))]
    (is (empty? (:diagnostics (run-project source {}))))
    (is (= [["client.clj" 3 "TSP2322" [:parent]]
            ["client.clj" 5 "TSP2322" [1]]
            ["client.clj" 7 "TSP2531" [:parent]]]
           (mapv issue strict)) (pr-str strict))))

(deftest merging-branches-preserves-the-platform-alternative-in-both-orders
  (let [source "(defn checked [flag]
  (api/consume {:parent (if flag (java.io.File. \"file\")
                             (.getParentFile (java.io.File. \"file\")))}))
(defn reversed [flag]
  (api/consume {:parent (if flag (.getParentFile (java.io.File. \"file\"))
                             (java.io.File. \"file\"))}))"
        strict (:diagnostics (run-project source {:strict-nil true}))]
    (is (= [["client.clj" 3 "TSP2531" [:parent]]
            ["client.clj" 6 "TSP2531" [:parent]]]
           (mapv issue strict)) (pr-str strict))
    (is (empty? (:diagnostics (run-project source {})))))
  (let [file (t/class-type "java.io.File")
        platform (t/platform file "File")
        platform-union (assoc (t/union file (t/prim :string)) :platform true)]
    (is (every? #(t/nilable? {:strict-nil true} %)
                [(t/union file platform) (t/union platform file)
                 (t/union (t/prim :int) platform-union)]))
    (is (not (t/nilable? {} (t/union file platform))))))

(deftest inference-through-a-generic-tuple-does-not-erase-nil
  (let [source "(defn unbox {:t/sig '(t/all [a] [(s/tuple a) :-> a])} [[value]] value)
(clojure.string/upper-case (unbox [(.getProperty (java.util.Properties.) \"key\")]))
(when-let [value (unbox [(.getProperty (java.util.Properties.) \"key\")])]
  (clojure.string/upper-case value))"
        strict (:diagnostics (run-project source {:strict-nil true}))]
    (is (= [["client.clj" 3 "TSP2531" nil]] (mapv issue strict)) (pr-str strict))
    (is (empty? (:diagnostics (run-project source {}))))))

(deftest recursive-maps-retain-the-path-to-a-platform-value
  (let [source "(s/def ::children (s/coll-of ::node :kind vector?))
(s/def ::node (s/keys :opt-un [::api/parent ::children]))
(s/fdef tree :args (s/cat :node ::node) :ret any?)
(defn tree [node] node)
(tree {:children [{:parent (.getParentFile (java.io.File. \"file\"))}]})"
        strict (:diagnostics (run-project source {:strict-nil true}))]
    (is (= [["client.clj" 6 "TSP2531" [:children 0 :parent]]]
           (mapv issue strict)) (pr-str strict))
    (is (empty? (:diagnostics (run-project source {}))))))

(deftest nullable-targets-and-stable-local-guards-accept-platform-values
  (let [source "(s/def ::nullable (s/nilable ::api/parent))
(s/def ::settings (s/keys :req-un [::nullable]))
(s/fdef settings :args (s/cat) :ret ::settings)
(defn settings [] {:nullable (.getParentFile (java.io.File. \"file\"))})
(defn hint-guard [^java.io.File parent]
  (if (some? parent)
    (api/consume {:parent parent})
    nil))
(let [settings {:parent (.getParentFile (java.io.File. \"file\"))}]
  (when (some? (:parent settings))
    (api/consume settings)))"
        strict (:diagnostics (run-project source {:strict-nil true}))]
    (is (empty? strict) (pr-str strict))))
