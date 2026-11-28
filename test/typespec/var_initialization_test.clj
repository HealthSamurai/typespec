(ns typespec.var-initialization-test
  (:require [clojure.test :refer [deftest is]]
            [typespec.check :as check]
            [typespec.main :as main]
            [typespec.parse :as parse]))

(defn- checked
  ([provider consumer] (checked provider consumer {}))
  ([provider consumer opts]
   (check/check-files [(parse/parse-string "consumer.clj"
                                         (str "(ns consumer (:require [provider :as p]))\n" consumer))
                       (parse/parse-string "provider.clj" (str "(ns provider)\n" provider))]
                      opts)))

(defn- codes [result] (mapv :code (:diagnostics result)))

(deftest distinguishes-a-var-from-its-root-value
  (let [result (checked "(defn init [] (def context :ready))"
                        "#'p/context\n(var p/context)\np/context\np/contex")]
    (is (= ["TSP721-01" "TSP2305"] (codes result)) (pr-str (:diagnostics result)))
    (is (= [4 5] (mapv #(get-in % [:pos :row]) (:diagnostics result))))
    (is (= #{:possibly-unbound-var} (get-in result [:diagnostics 0 :reasons])))
    (is (= "clojure.lang.Var" (main/type-at result "consumer.clj" 2 1)))
    (is (= "clojure.lang.Var" (main/type-at result "consumer.clj" 3 1)))
    (is (= "t/unknown" (main/type-at result "consumer.clj" 4 1))))
  (let [result (checked "(declare pending)" "p/pending" {:strict true})]
    (is (= [["TSP721-01" :error]] (mapv (juxt :code :severity) (:diagnostics result))))))

(deftest distinguishes-nil-from-an-omitted-initializer
  (let [result (checked "(def no-init)
                         (def nil-root nil)
                         (do (defonce once nil))
                         (defn maybe-replace [] (def nil-root :later))
                         (declare later)
                         (defn read-later [] later)
                         (defmulti dispatch identity)
                         (defmethod dispatch :read [_] (def method-var later))
                         (def later nil)
                         (declare initialized)
                         (defn initialized [] nil)"
                        "p/no-init\np/nil-root\np/once\np/later\n(p/initialized)\np/method-var")]
    (is (= ["TSP721-01" "TSP721-01"] (codes result)) (pr-str (:diagnostics result)))
    (is (= [["consumer.clj" 2] ["consumer.clj" 7]]
           (mapv (juxt :file (comp :row :pos)) (:diagnostics result))))))

(deftest def-and-defonce-check-their-initializer-hints
  (let [result (checked "(def ^String good \"ready\")
(def ^String wrong (identity 42))
(defonce ^{:tag String} bad-once {:label \"ready\"})
(defonce ^String maybe nil)
(def ^String pending)"
                        "p/good\np/maybe\np/pending")
        diagnostics (:diagnostics result)]
    (is (= [["consumer.clj" 4 "TSP721-01"] ["provider.clj" 3 "TSP2322"] ["provider.clj" 4 "TSP2322"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics))
    (is (= "hint String contradicts the type of the initializer: 42"
           (:message (second diagnostics))))))

(deftest initialized-var-hints-reach-other-namespaces
  (let [result (checked "(def ^String label \"ready\")
(defonce ^String fallback \"default\")
(def ^java.io.File location (java.io.File. \"settings\"))
(def ^String pending)
(def ^String redefined \"old\")
(def redefined 42)
(defn size [^String label] (.length label))"
                        "(.length p/label)\n(p/size p/fallback)\n(p/size p/location)\np/redefined\np/pending")
        diagnostics (:diagnostics result)]
    (is (= [["consumer.clj" 4 "TSP2345"] ["consumer.clj" 6 "TSP721-01"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics))
    (is (= "t/unknown" (main/type-at result "consumer.clj" 5 1)))))

(deftest scalar-const-vars-preserve-literal-types-at-consumers
  (let [result (checked "(def ^:const mode :auto)
(def ^{:const true} wrong \"123\")
(def ^:const absent nil)
(def ^:const label \"ready\")
(def ^{:const false} ^:const ordinary :manual)
(def ^:const reassigned :auto)
(def reassigned :manual)"
                        "(clojure.spec.alpha/fdef choose :args (clojure.spec.alpha/cat :mode #{:auto :manual}))
(defn choose [mode] mode)
(choose p/mode)
(choose p/wrong)
(choose p/absent)
(.length p/label)
p/ordinary
p/reassigned")
        diagnostics (:diagnostics result)]
    (is (= [["consumer.clj" 5 "TSP2345"] ["consumer.clj" 6 "TSP2531"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics))
    (is (= [":auto" "nil?" "t/unknown" "t/unknown"]
           (mapv (fn [[row col]] (main/type-at result "consumer.clj" row col))
                 [[4 9] [6 9] [8 1] [9 1]])))))

(deftest literal-const-collections-preserve-map-fields-and-tuple-positions
  (let [result (checked "(def ^:const deployment {:mode :auto :port 443})
(def ^:const invalid {:mode :auto :port \"443\"})
(def ^:const cert [\"root\" 2])
(def ^:const bad-cert [42 2])
(def ^:const calculated {:port (+ 400 43)})"
                        "(clojure.spec.alpha/def ::mode #{:auto :manual})
(clojure.spec.alpha/def ::port int?)
(clojure.spec.alpha/fdef deploy :args
  (clojure.spec.alpha/cat :opts (clojure.spec.alpha/keys :req-un [::mode ::port])
                         :cert (clojure.spec.alpha/tuple string? int?)) :ret int?)
(defn deploy [{:keys [port]} [_ generation]] (+ port generation))
(deploy p/deployment p/cert)
(deploy p/invalid p/cert)
(deploy p/deployment p/bad-cert)
(.size p/deployment)
p/calculated")
        diagnostics (:diagnostics result)]
    (is (= [["consumer.clj" 9 "TSP2322"] ["consumer.clj" 10 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics))
    (is (= "value of :port: \"443\" is not assignable to ::port (int?)" (:message (first diagnostics))))
    (is (= "int" (main/type-at result "consumer.clj" 11 1)))
    (is (= "t/unknown" (main/type-at result "consumer.clj" 12 1)))))

(deftest regex-roots-have-a-type-across-namespaces-when-they-are-stable
  (let [result (checked "(def pattern #\"(x)\")
(defonce other #\"y\")
(def ^:dynamic *pattern* #\"x\")
(def changeable #\"x\")
(defn change! [] (def changeable :bad))
(def altered #\"x\")
(defn alter! [] (alter-var-root #'altered (constantly :bad)))"
                        "(re-find p/pattern \"x\")
(.find (re-matcher p/other \"y\"))
(inc p/pattern)
(re-find p/*pattern* \"x\")
(re-find p/changeable \"x\")
(with-redefs [p/pattern :wrong]
  (re-find p/pattern \"x\"))
(re-find p/altered \"x\")")
        diagnostics (:diagnostics result)]
    (is (= [["consumer.clj" 4 "TSP2345"] ["consumer.clj" 5 "TSP210-01"]
            ["consumer.clj" 6 "TSP210-01"] ["consumer.clj" 8 "TSP210-01"]
            ["consumer.clj" 9 "TSP210-01"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics))
    (is (= "java.util.regex.Pattern" (main/type-at result "consumer.clj" 2 10)))))

(deftest follows-local-sequences-without-assuming-calls-to-initializers
  (let [result (checked "(declare root)
                         (def snapshot root)
                         (do (def root nil) (def later root))
                         (defn initialize [] (do (def local nil)) local)
                         (defn conditional [flag] (when flag (def maybe-root nil)) maybe-root)
                         (declare handler)
                         (defn install [] (def handler identity))"
                        "(p/initialize)\np/local\n(p/handler 1)\np/root")]
    (is (= (repeat 4 "TSP721-01") (codes result)) (pr-str (:diagnostics result)))
    (is (= [["consumer.clj" 3] ["consumer.clj" 4] ["provider.clj" 3] ["provider.clj" 6]]
           (mapv (juxt :file (comp :row :pos)) (:diagnostics result))))))

(deftest finds-defs-in-code-and-skips-data-and-opaque-macros
  (let [result (checked "(defmacro discard [& _] nil)
                         (defn init [flag] (if flag (def branch nil) nil))
                         (defn lazy-init [] #(do (def deferred nil)))
                         (comment (def only-comment nil))
                         '(def quoted nil)
                         (discard (def macro-data nil))
                         (let [x 1] (case x (def case-label) nil))
                         (let [declare identity] (declare shadowed))
                         (defn explicit [] (clojure.core/declare nested-declaration))
                         (def f (fn ([x] (def from-fn nil)) ([] nil)))"
                        "p/branch\np/deferred\np/from-fn\np/nested-declaration\np/only-comment\np/quoted\np/macro-data\np/case-label\np/shadowed")
        consumer-diags (filter #(= "consumer.clj" (:file %)) (:diagnostics result))]
    (is (= (concat (repeat 4 "TSP721-01") (repeat 5 "TSP2305"))
           (mapv :code consumer-diags)) (pr-str (:diagnostics result)))))

(deftest declared-signatures-do-not-establish-root-initialization
  (let [result (checked "(declare make)
                         (defn install [] (def make identity))
                         (clojure.spec.alpha/fdef make :args (clojure.spec.alpha/cat :x string?) :ret string?)"
                        "(p/make 1)")]
    (is (= ["TSP721-01"] (codes result)) (pr-str (:diagnostics result)))
    (is (= "t/unknown" (main/type-at result "consumer.clj" 2 1)))))

(deftest only-the-eager-wrapper-proves-initialization
  (let [result (checked "(time (def measured nil))
                         (def task (future (def async-root nil)))
                         (def lazy-value (lazy-seq (def deferred-root nil) nil))
                         (defn install [] (time (def local-root nil)) local-root)"
                        "p/measured\np/async-root\np/deferred-root\np/local-root")]
    (is (= (repeat 3 "TSP721-01") (codes result)) (pr-str (:diagnostics result)))
    (is (= [["consumer.clj" 3] ["consumer.clj" 4] ["consumer.clj" 5]]
           (mapv (juxt :file (comp :row :pos)) (:diagnostics result))))))

(deftest a-foreign-binding-vector-does-not-prove-a-global-read
  (let [result (checked "(defn install [] (def state nil))
                         (lifecycle/start [state config]
                           (count [state])
                           provider/state)
                         (count [state])"
                        "p/state")]
    (is (= (repeat 3 "TSP721-01") (codes result)) (pr-str (:diagnostics result)))
    (is (= [["consumer.clj" 2] ["provider.clj" 5] ["provider.clj" 6]]
           (mapv (juxt :file (comp :row :pos)) (:diagnostics result))))))
