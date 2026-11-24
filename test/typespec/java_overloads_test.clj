(ns typespec.java-overloads-test
  (:require [clojure.test :refer [deftest is]]
            [typespec.check :as check]
            [typespec.main :as main]
            [typespec.parse :as parse]))

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
