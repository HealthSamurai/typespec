(ns typespec.java-overloads-test
  (:require [clojure.test :refer [deftest is]]
            [typespec.check :as check]
            [typespec.main :as main]
            [typespec.parse :as parse]))

(deftest metadata-map-tags-participate-in-interop-and-project-contracts
  (let [api (parse/parse-string "api.clj" "(ns api)
(defn upper ^{:tag String} [^{:tag String} ^Long text]
  (.toUpperCase text))
(defn wrong-return ^{:tag String} [] 42)
(defn local []
  (let [^{:tag String} text (identity 42)]
    (.toUpperCase text)))
(defn cleared [^{:tag nil} ^String x] x)
(defn label-count [^{:tag \"[Ljava.lang.String;\"} labels] (alength labels))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api]))
(api/upper \"good\")
(api/upper 42)
(api/cleared 42)
(api/label-count (into-array String [\"good\"]))
(api/label-count (int-array [42]))")
        result (check/check-files [client api] {})]
    (is (= [["api.clj" 4 "TSP2322"] ["api.clj" 6 "TSP2322"]
            ["client.clj" 3 "TSP2345"] ["client.clj" 6 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))
    (is (= "String" (main/type-at result "client.clj" 2 1)))))

(deftest clojure-functions-adapt-to-java-interfaces-across-namespaces
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/fdef make-future :args (s/cat) :ret java.util.concurrent.CompletableFuture)
(defn make-future [] (java.util.concurrent.CompletableFuture/completedFuture 3))
(defn stringify [x] (str x))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api])
  (:import [java.util.concurrent CompletableFuture] [java.util.concurrent.atomic AtomicInteger]))
(.thenApply ^CompletableFuture (api/make-future) api/stringify)
(.thenApply ^CompletableFuture (api/make-future) (fn [x & more] (str x)))
(.thenApply ^CompletableFuture (api/make-future) (fn [] 1))
(.thenApply ^CompletableFuture (api/make-future) 42)
(.updateAndGet (AtomicInteger. 3) (fn [x] 4))
(.updateAndGet (AtomicInteger. 3) (fn [x] \\A))
(.updateAndGet (AtomicInteger. 3) (fn [x] \"wrong\"))
(.filter (java.util.stream.IntStream/range 0 3) (fn [x] true))
(java.util.stream.IntStream/.filter (java.util.stream.IntStream/range 0 3) (fn [x] \"wrong\"))
(.setURIResolver (javax.xml.transform.TransformerFactory/newInstance) (fn [href base] nil))")
        ds (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 5 "TSP2554"] ["client.clj" 6 "TSP2345"] ["client.clj" 9 "TSP2345"]
            ["client.clj" 11 "TSP2345"] ["client.clj" 12 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) ds))
        (pr-str ds))
    (is (re-find #"callback accepting 1 argument" (:message (first ds))))
    (is (re-find #"callback returning .* for applyAsInt" (:message (nth ds 2))))))

(deftest java-functional-interface-adaptation-on-the-jvm
  (let [^java.util.concurrent.CompletableFuture f (java.util.concurrent.CompletableFuture/completedFuture 3)]
    (is (= "3" (.join (.thenApply f (fn [x] (str x))))))
    (is (instance? clojure.lang.ArityException
                   (try (.join (.thenApply f (fn [] 1)))
                        (catch java.util.concurrent.CompletionException ex (ex-cause ex))))))
  (let [^java.util.concurrent.atomic.AtomicInteger cell (java.util.concurrent.atomic.AtomicInteger. 3)]
    (is (= 65 (.updateAndGet cell (fn [_] \A))))
    (is (thrown? ClassCastException (.updateAndGet cell (fn [_] "wrong")))))
  (let [^java.util.stream.IntStream stream (java.util.stream.IntStream/range 0 3)]
    (is (thrown? ClassCastException (.count (.filter stream (fn [_] "truthy")))))))

(deftest java-functional-interface-inputs-contextualize-and-check-callbacks
  (let [api (parse/parse-string "api.clj" "(ns api)
(defn add-one [^long x] (inc x))
(defn string-size [^String x] (count x))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api])
  (:import [java.util.concurrent.atomic AtomicInteger]))
(.updateAndGet (AtomicInteger. 3) #(inc %))
(.updateAndGet (AtomicInteger. 3) api/add-one)
(.updateAndGet (AtomicInteger. 3) api/string-size)
(java.util.Arrays/setAll (int-array 2) (fn [i] (inc i)))")
        ds (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 5 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) ds))
        (pr-str ds))
    (is (= "java.util.concurrent.atomic.AtomicInteger.updateAndGet passes int? to callback parameter 1 for applyAsInt, which expects String"
           (:message (first ds))))))

(deftest native-java-interfaces-check-clojure-function-contracts
  (let [api (parse/parse-string "api.clj" "(ns api)
(defn ordered [a b] false)
(defn task [] :finished)
(defn wrong-task [x] x)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api]))
(java.util.TreeSet. api/ordered)
(java.util.TreeSet. (fn [x y] 0))
(java.util.TreeSet. (fn [x] 0))
(java.util.TreeSet. (fn [x y] nil))
(java.util.TreeSet. (java.util.Comparator/naturalOrder))
(Thread. api/task)
(Thread. api/wrong-task)
(java.util.concurrent.FutureTask. api/task)
(java.util.concurrent.FutureTask. api/wrong-task)")
        ds (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 4 "TSP2554"] ["client.clj" 5 "TSP2345"]
            ["client.clj" 8 "TSP2554"] ["client.clj" 10 "TSP2554"]]
           (mapv (juxt :file (comp :row :pos) :code) ds))
        (pr-str ds)))
  (let [ordered (java.util.TreeSet. (fn [x y] (< x y)))]
    (.add ordered 2)
    (.add ordered 1)
    (is (= [1 2] (vec ordered))))
  (is (thrown? clojure.lang.ArityException (.run (Thread. (fn [x] x))))))

(deftest java-callbacks-distinguish-primitive-hints-from-spec-contracts
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/fdef only-doubles :args (s/cat :x double?) :ret int?)
(defn only-doubles [x] (if (double? x) 1 (throw (IllegalArgumentException. \"expected Double\"))))
(defn hinted-double [^double x] (long x))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api]))
(.updateAndGet (java.util.concurrent.atomic.AtomicInteger. 7) api/hinted-double)
(.updateAndGet (java.util.concurrent.atomic.AtomicInteger. 7) api/only-doubles)")
        ds (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 3 "TSP2345"]] (mapv (juxt :file (comp :row :pos) :code) ds))
        (pr-str ds))
    (is (re-find #"passes int\? to callback parameter 1 .* expects double\?" (:message (first ds))))))

(deftest other-java-arguments-select-the-callback-context-across-namespaces
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/fdef integers :args (s/cat) :ret #(instance? int/1 %))
(defn integers [] (int-array [1 2 3]))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api]))
(java.util.Arrays/parallelPrefix (api/integers) (fn [a x] (+ a x)))
(java.util.Arrays/parallelPrefix (double-array 2) (fn [a x] (+ a x)))
(java.util.Arrays/parallelPrefix (api/integers) (fn [a x] (str a x)))")
        ds (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 4 "TSP2345"]] (mapv (juxt :file (comp :row :pos) :code) ds))
        (pr-str ds))))

(deftest direct-java-overloads-need-a-unique-functional-target
  (let [source "(ns sample.choices (:import [java.io File FileFilter FilenameFilter]))
(.listFiles (File. \".\") (fn [_] false))
(defn two [^File f] (.listFiles f (fn [_ _] false)))
(^[FileFilter] File/.listFiles (File. \".\") (fn [_] false))
(^[FilenameFilter] File/.listFiles (File. \".\") (fn [_ _] false))
(defn reflective [f] (.listFiles f (fn [_] false)))"
        ds (:diagnostics (check/check-files [(parse/parse-string "choices.clj" source)] {}))]
    (is (= [[2 "TSP2345"] [3 "TSP2345"] [6 "TSP7020"]]
           (mapv (juxt (comp :row :pos) :code) ds))
        (pr-str ds))
    (is (every? #(re-find #"java.io.FileFilter, java.io.FilenameFilter" (:message %)) (take 2 ds))))
  (is (try (eval '(.listFiles (java.io.File. ".") (fn [_] false)))
           false
           (catch clojure.lang.Compiler$CompilerException ex
             (re-find #"More than one matching method" (ex-message (ex-cause ex)))))))

(deftest java-overloads-check-the-whole-argument-list
  (let [source "(ns example.overloads)
(java.util.Arrays/equals (byte-array [1]) (byte-array [1]))
(java.util.Arrays/equals (int-array [1]) (int-array [1]))
(java.util.Arrays/equals (byte-array [1]) (int-array [1]))
(java.util.Arrays/equals (byte-array [1]) :bad)
(defn same-array [flag]
  (let [values (if flag (byte-array [1]) (int-array [1]))]
    (java.util.Arrays/equals values values)))
(defn format-or-fail [flag]
  (String/format \"%s\" (if flag (object-array [\"ok\"]) [\"bad\"])))"
        result (check/check-files [(parse/parse-string "overloads.clj" source)] {})
        diagnostics (:diagnostics result)]
    (is (= [[4 1 "TSP2345"] [5 43 "TSP2345"] [10 23 "TSP2345"]]
           (mapv (juxt (comp :row :pos) (comp :col :pos) :code) diagnostics))
        (pr-str diagnostics))
    (is (= (str "No overload of java.util.Arrays.equals accepts these arguments. "
                "(byte[], byte[]) rejects argument 2. (int[], int[]) rejects argument 1")
           (:message (first diagnostics))))))

(deftest java-overloads-check-destructured-array-specs-across-namespaces
  (let [files [(parse/parse-string
                "provider.clj"
                "(ns provider (:require [clojure.spec.alpha :as s]))
(s/fdef arrays :args (s/cat) :ret (s/tuple #(instance? byte/1 %) #(instance? int/1 %)))
(defn arrays [] [(byte-array [1]) (int-array [1])])")
               (parse/parse-string
                "consumer.clj"
                "(ns consumer (:require [provider :as p]))
(let [[bytes ints] (p/arrays)]
  (java.util.Arrays/equals bytes bytes)
  (java.util.Arrays/equals ints ints)
  (java.util.Arrays/equals bytes ints))")]
        result (check/check-files files {})]
    (is (= [["consumer.clj" 5 3 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) (comp :col :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))))

(deftest qualified-instance-calls-use-the-class-in-the-method-name
  (let [source "(ns example.qualified (:import [java.io InputStream ByteArrayInputStream]))
(defn stream [] (ByteArrayInputStream. (byte-array [7])))
(InputStream/.read (stream))
(java.io.InputStream/.read (stream) (byte-array 2) 0 1)
(CharSequence/.length \"text\")
(Class/.getName String)
(InputStream/.read (stream) \"bad-buffer\")
(InputStream/.read (stream) 1 2)
(InputStream/.read \"bad-receiver\")
(InputStream/.read nil)
(InputStream/.read)
(Object/.length \"text\")
(InputStream/.raed (stream))
(Integer/.intValue 1)
(Integer/.intValue (Integer/valueOf \"1\"))
(CharSequence/.getClass \"text\")"
        result (check/check-files [(parse/parse-string "qualified.clj" source)] {})
        ds (:diagnostics result)]
    (is (= [[7 "TSP2345"] [8 "TSP2554"] [9 "TSP2345"] [10 "TSP2531"]
            [11 "TSP2554"] [12 "TSP2339"] [13 "TSP2339"] [14 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) ds))
        (pr-str ds))
    (is (= ["int" "int" "int" "String" "int" "Class"]
           (mapv #(main/type-at result "qualified.clj" % 1) [3 4 5 6 15 16])))))

(deftest qualified-instance-parameter-tags-select-a-single-jvm-overload
  (let [source "(ns example.param-tags (:import [java.io InputStream ByteArrayInputStream]))
(defn stream [] (ByteArrayInputStream. (byte-array [7])))
(^[] InputStream/.read (stream))
(^[bytes] InputStream/.read (stream) (byte-array 2))
(^[_ int int] InputStream/.read (stream) (byte-array 2) 0 1)
(^{:param-tags [byte/1]} InputStream/.read (stream) (byte-array 2))
(^[\"[B\"] InputStream/.read (stream) (byte-array 2))
(^{:param-tags nil} InputStream/.read (stream))
(^[String] String/.indexOf \"abc\" \"b\")
(^[String] String/.indexOf \"abc\" 98)
(^[bytes] InputStream/.read (stream))
(^[_] String/.indexOf \"abc\" \"b\")
(^[String] InputStream/.read (stream) \"bad\")
(^{:param-tags :wrong} InputStream/.read (stream))"
        result (check/check-files [(parse/parse-string "params.clj" source)] {})
        ds (:diagnostics result)]
    (is (= [[10 "TSP2345"] [11 "TSP2554"] [12 "TSP2345"] [13 "TSP2345"] [14 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) ds))
        (pr-str ds))
    (is (= (repeat 7 "int") (map #(main/type-at result "params.clj" % 1) (range 3 10))))))

(deftest qualified-instance-methods-use-receiver-specs-across-namespaces
  (let [provider (parse/parse-string
                   "streams.clj"
                   "(ns sample.streams (:require [clojure.spec.alpha :as s])
                     (:import [java.io InputStream ByteArrayInputStream]))
(s/fdef open :args (s/cat) :ret InputStream)
(defn open [] (ByteArrayInputStream. (byte-array [7])))
(s/fdef text :args (s/cat) :ret string?)
(defn text [] \"no stream\")")
        client (parse/parse-string
                 "client.clj"
                 "(ns sample.client (:require [sample.streams :as src]) (:import [java.io InputStream]))
(InputStream/.read (src/open))
(InputStream/.read (src/text))")
        ds (:diagnostics (check/check-files [provider client] {}))]
    (is (= [["client.clj" 3 20 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) (comp :col :pos) :code) ds))
        (pr-str ds))))

(deftest receiver-hints-remain-available-after-map-narrowing
  (let [source "(ns example.hints)
(defn items [^Iterable value]
  (when (map? value)
    (.iterator value)))
(defn alias-items [^Iterable value]
  (let [alias value]
    (when (map? alias) (. alias iterator))))
(defn from-expression [value]
  (let [alias ^Iterable value]
    (when (map? alias) (.iterator alias))))
(defn entries [^Iterable value]
  (when (map? value) (.entrySet value)))
(defn wrong-arity [^Iterable value]
  (when (map? value) (.iterator value 42)))
(defn missing [^Iterable value]
  (when (map? value) (.missingMethod value)))
(defn shadowed [^Iterable value]
  (let [value \"text\"] (.iterator value)))
(defn use-site [value]
  (when (map? value) (.iterator ^Iterable value)))"
        result (check/check-files [(parse/parse-string "hints.clj" source)] {})
        ds (:diagnostics result)]
    (is (= [[14 "TSP2554"] [16 "TSP2339"] [18 "TSP2339"]]
           (mapv (juxt (comp :row :pos) :code) ds))
        (pr-str ds))
    (is (= "java.util.Iterator" (main/type-at result "hints.clj" 4 5)))
    (is (= "map?" (main/type-at result "hints.clj" 4 16)))))

(deftest a-narrowed-receiver-can-satisfy-an-exported-spec
  (let [provider (parse/parse-string
                   "maps.clj"
                   "(ns sample.maps (:require [clojure.spec.alpha :as s]) (:import [java.util Iterator]))
(s/fdef entries :args (s/cat :value Iterable) :ret (s/nilable Iterator))
(defn entries [^Iterable value]
  (when (map? value) (.iterator value)))")
        client (parse/parse-string
                 "client.clj"
                 "(ns sample.client (:require [sample.maps :as m]))
(m/entries {:item 1})
(m/entries \"not iterable\")")
        ds (:diagnostics (check/check-files [provider client] {}))]
    (is (= [["client.clj" 3 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) ds))
        (pr-str ds))))

(deftest java-constructors-check-arity-and-arguments
  (let [source "(ns constructors (:import [java.io File InputStream]))
(StringBuilder. \"text\")
(new StringBuilder \"text\")
(StringBuilder. :bad)
(new StringBuilder :bad)
(StringBuilder. \"text\" \"extra\")
(new StringBuilder 8)
(File. (File. \"parent\") \"child\")
(new File \"parent\" false)
(ProcessBuilder. (into-array String [\"echo\"]))
(new ProcessBuilder [\"echo\"])
(ProcessBuilder. \"echo\")
(new ProcessBuilder \"echo\" \"hi\")
(new InputStream)
(new Runnable)
(Object.)
(clojure.lang.Cons. :auth '(:more))
(new clojure.lang.Cons :auth '())"
        result (check/check-files [(parse/parse-string "constructors.clj" source)] {})]
    (is (= [[4 17 "TSP2345"] [5 20 "TSP2345"] [6 1 "TSP2554"] [9 20 "TSP2345"]
            [12 18 "TSP2345"] [13 1 "TSP2554"] [14 1 "TSP2345"] [15 1 "TSP2339"]]
           (mapv (juxt (comp :row :pos) (comp :col :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))
    (is (= ["java.lang.StringBuilder" "java.lang.StringBuilder" "java.lang.StringBuilder"
            "java.io.File" "java.lang.ProcessBuilder" "java.lang.ProcessBuilder" "java.lang.Object"
            "clojure.lang.Cons" "clojure.lang.Cons"]
           (mapv #(main/type-at result "constructors.clj" % 1) [2 3 7 8 10 11 16 17 18])))))

(deftest java-constructor-arguments-cross-namespace-contracts
  (let [files [(parse/parse-string
                "provider.clj"
                "(ns provider (:require [clojure.spec.alpha :as s]))
(s/fdef command :args (s/cat) :ret (s/tuple #(instance? String/1 %) #(instance? byte/1 %)))
(defn command [] [(into-array String [\"echo\"]) (byte-array [1])])")
               (parse/parse-string
                "consumer.clj"
                "(ns consumer (:require [provider :as p]))
(let [[words bytes] (p/command)]
  (ProcessBuilder. words)
  (new ProcessBuilder bytes))")]
        result (check/check-files files {})]
    (is (= [["consumer.clj" 4 23 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) (comp :col :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))))
