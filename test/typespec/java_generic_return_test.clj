(ns typespec.java-generic-return-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is use-fixtures]]
            [typespec.check :as check]
            [typespec.java :as java]
            [typespec.main :as main]
            [typespec.parse :as parse]
            [typespec.types :as t])
  (:import [java.net URL URLClassLoader]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [javax.tools ToolProvider]))

(def ^:private owner "typespec.fixture.GenericReturns")
(def ^:private builder (str owner "$Builder"))
(def ^:private product (str owner "$Product"))

(deftest java-map-get-preserves-its-nullable-value-type-across-files
  (let [api (parse/parse-string "api.clj" "(ns api)
(defn environment [] (System/getenv))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a] [clojure.spec.alpha :as s])
  (:import [java.util Map]))
(s/fdef prefix :args (s/cat :key string?) :ret (s/nilable string?))
(defn prefix [key]
  (when-let [value (.get ^Map (a/environment) key)]
    (subs value 0)))
(s/fdef wrong :args (s/cat :key string?) :ret (s/nilable number?))
(defn wrong [key]
  (when-let [value (.get ^Map (a/environment) key)]
    (inc value)))
(s/fdef unguarded :args (s/cat :key string?) :ret string?)
(defn unguarded [key] (.get ^Map (a/environment) key))
(prefix \"PATH\")
(prefix 42)")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 10 "TSP2345"] ["client.clj" 12 "TSP2322"] ["client.clj" 14 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))
    (is (re-find #"may return nil" (:message (second issues))))
    (let [raw (t/class-type "java.util.Map")
          typed (assoc raw :java-map-classes ["java.lang.String" "java.lang.String"])]
      (is (= raw (t/union typed raw))))))

(deftest java-map-values-preserve-value-types-across-files
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(defn traces [] (Thread/getAllStackTraces))
(s/fdef stack-size :args (s/cat :trace #(instance? StackTraceElement/1 %)) :ret int?)
(defn stack-size [^StackTraceElement/1 trace] (alength trace))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a] [clojure.spec.alpha :as s])
  (:import [java.util Map Collection Iterator]))
(s/fdef sizes :args (s/cat) :ret (s/coll-of int? :kind vector?))
(defn sizes [] (mapv #(a/stack-size %) (.values ^Map (a/traces))))
(defn wrong [] (mapv #(subs % 0) (.values ^Map (a/traces))))
(defn next-size []
  (a/stack-size (.next ^Iterator (.iterator ^Collection (.values ^Map (a/traces))))))
(defn next-wrong []
  (subs (.next ^Iterator (.iterator ^Collection (.values ^Map (a/traces)))) 0))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 5 "TSP2345"] ["client.clj" 9 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest java-collection-streams-retain-elements-across-files
  (let [api (parse/parse-string "api.clj" "(ns api (:import [java.nio.file Files Path] [java.util List]))
(defn lines [^Path path] (Files/readAllLines path))
(defn line-stream [^Path path] (.stream ^List (lines path)))
(defn parallel-lines [^Path path] (.parallelStream ^List (lines path)))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a] [clojure.string :as str])
  (:import [java.nio.file Path] [java.util.stream Stream]))
(defn good [^Path path]
  (with-open [source (a/line-stream path)]
    (let [filtered (.filter ^Stream source #(str/starts-with? % \"typed\"))]
      (mapv #(subs % 0) (iterator-seq (.iterator ^Stream filtered))))))
(defn wrong [^Path path]
  (with-open [source (a/parallel-lines path)]
    (mapv inc (iterator-seq (.iterator ^Stream source)))))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 9 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest java-stream-filter-checks-the-predicate-and-keeps-result-elements
  (let [api (parse/parse-string "api.clj" "(ns api (:import [java.nio.file Files Path]))
(defn lines [^Path path] (Files/lines path))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a] [clojure.string :as str])
  (:import [java.nio.file Path] [java.util.stream Stream]))
(defn good [^Path path]
  (with-open [lines (a/lines path)]
    (let [filtered (.filter ^Stream lines #(str/starts-with? % \"typed\"))]
      (mapv #(subs % 0) (iterator-seq (.iterator ^Stream filtered))))))
(defn wrong-predicate [^Path path]
  (with-open [lines (a/lines path)]
    (let [filtered (.filter ^Stream lines #(pos? %))]
      (.count ^Stream filtered))))
(defn wrong-result [^Path path]
  (with-open [lines (a/lines path)]
    (let [filtered (.filter ^Stream lines #(str/starts-with? % \"typed\"))]
      (mapv inc (iterator-seq (.iterator ^Stream filtered))))))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 9 "TSP2345"] ["client.clj" 14 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest java-iterable-foreach-contextualizes-inline-callbacks
  (let [api (parse/parse-string "api.clj" "(ns api (:import [java.nio.file FileSystems Path]))
(defn roots [] (.getRootDirectories (FileSystems/getDefault)))
(defn views [] (.supportedFileAttributeViews (FileSystems/getDefault)))
(defn name-count [^Path root] (.getNameCount root))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(.forEach ^Iterable (a/roots) (fn [p] (a/name-count p)))
(.forEach ^Iterable (a/roots) (fn [p] (subs p 0)))
(.forEach ^Iterable (a/views) #(subs % 0))
(.forEach ^Iterable (a/views) #(inc %))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 3 "TSP2345"] ["client.clj" 5 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest java-stream-iterators-propagate-elements-through-with-open
  (let [api (parse/parse-string "api.clj" "(ns api (:import [java.nio.file Files Path]))
(defn lines [^Path path] (Files/lines path))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a] [clojure.spec.alpha :as s])
  (:import [java.nio.file Path] [java.util.stream Stream] [java.util Iterator]))
(s/fdef render :args (s/cat :path #(instance? Path %)) :ret (s/coll-of string? :kind vector?))
(defn render [^Path path]
  (with-open [lines (a/lines path)]
    (mapv #(subs % 0) (iterator-seq (.iterator ^Stream lines)))))
(defn wrong [^Path path]
  (with-open [lines (a/lines path)]
    (mapv inc (iterator-seq (.iterator ^Stream lines)))))
(defn first-line [^Path path]
  (with-open [lines (a/lines path)]
    (subs (.next ^Iterator (.iterator ^Stream lines)) 0)))
(defn wrong-next [^Path path]
  (with-open [lines (a/lines path)]
    (inc (.next ^Iterator (.iterator ^Stream lines)))))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 9 "TSP2345"] ["client.clj" 15 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest java-list-iterators-preserve-both-direction-element-types
  (let [api (parse/parse-string "api.clj" "(ns api (:import [java.nio.file Files Path] [java.util List]))
(defn lines [^Path path] (Files/readAllLines path))
(defn forward [^Path path] (.listIterator ^List (lines path)))
(defn reverse-one [^Path path] (.listIterator ^List (lines path) 1))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]) (:import [java.nio.file Path] [java.util ListIterator]))
(defn start [^Path path] (subs (.next ^ListIterator (a/forward path)) 0))
(defn back [^Path path] (subs (.previous ^ListIterator (a/reverse-one path)) 0))
(defn wrong-next [^Path path] (inc (.next ^ListIterator (a/forward path))))
(defn wrong-previous [^Path path] (inc (.previous ^ListIterator (a/reverse-one path))))
(defn render [^Path path] (mapv #(subs % 0) (iterator-seq (a/forward path))))
(defn wrong-seq [^Path path] (mapv inc (iterator-seq (a/forward path))))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= (mapv #(vector "client.clj" % "TSP2345") [4 5 7])
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest java-list-get-preserves-the-element-result-across-files
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s])
  (:import [java.nio.file Files Path] [java.util List]))
(defn lines [^Path path] (Files/readAllLines path))
(s/fdef first-line :args (s/cat :path #(instance? Path %)) :ret string?)
(defn first-line [^Path path] (.get ^List (lines path) 0))
(defn inferred-line [^Path path] (.get ^List (lines path) 0))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]) (:import [java.nio.file Path] [java.util List]))
(defn render [^Path path] (subs (.get ^List (a/lines path) 0) 0))
(defn wrong [^Path path] (inc (.get ^List (a/lines path) 0)))
(defn wrong-inferred [^Path path] (inc (a/inferred-line path)))
(defn bad-index [^Path path] (.get ^List (a/lines path) \"bad\"))
(a/first-line \"bad\")
(defn wrong-qualified [^Path path] (inc (List/.get (a/lines path) 0)))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= (mapv #(vector "client.clj" % "TSP2345") [3 4 5 6 7])
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest iterators-from-generic-collections-preserve-elements-across-files
  (let [api (parse/parse-string "api.clj" "(ns api (:import [java.nio.file FileSystems Path]))
(defn roots [] (.getRootDirectories (FileSystems/getDefault)))
(defn views [] (.supportedFileAttributeViews (FileSystems/getDefault)))
(defn root-iterator [] (.iterator ^Iterable (roots)))
(defn view-iterator [] (.iterator ^Iterable (views)))
(defn name-count [^Path root] (.getNameCount root))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]) (:import [java.util Iterator]))
(a/name-count (.next ^Iterator (a/root-iterator)))
(subs (.next ^Iterator (a/view-iterator)) 0)
(inc (.next ^Iterator (a/view-iterator)))
(mapv #(subs % 0) (iterator-seq (a/view-iterator)))
(mapv #(subs % 0) (iterator-seq (a/root-iterator)))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 4 "TSP2345"] ["client.clj" 6 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest compatible-expression-hints-preserve-java-element-types
  (let [api (parse/parse-string "api.clj" "(ns api (:import [java.nio.file FileSystems Path]))
(defn roots [] (.getRootDirectories (FileSystems/getDefault)))
(defn views [] (.supportedFileAttributeViews (FileSystems/getDefault)))
(defn name-count [^Path root] (.getNameCount root))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(for [p ^Iterable (a/roots)] (a/name-count p))
(for [p ^Iterable (a/roots)] (subs p 0))
(mapv #(subs % 0) ^Iterable (a/views))
(mapv #(inc %) ^Iterable (a/views))
(let [^Iterable roots (a/roots)] (for [p roots] (a/name-count p)))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 3 "TSP2345"] ["client.clj" 5 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest java-iterable-returns-preserve-element-types-across-files
  (let [api (parse/parse-string "api.clj" "(ns api (:import [java.nio.file FileSystems Path]))
(defn roots [] (.getRootDirectories (FileSystems/getDefault)))
(defn views [] (.supportedFileAttributeViews (FileSystems/getDefault)))
(defn name-count [^Path root] (.getNameCount root))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(for [p (a/roots)] (a/name-count p))
(for [p (a/roots)] (subs p 0))
(doseq [view (a/views)] (subs view 0))
(doseq [view (a/views)] (inc view))
(mapv #(subs % 0) (a/views))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 3 "TSP2345"] ["client.clj" 5 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(use-fixtures :once
  (fn [test]
    (let [root (.toFile (Files/createTempDirectory "typespec-java-return-" (make-array FileAttribute 0)))
          compiler (ToolProvider/getSystemJavaCompiler)
          source "test/fixtures/java/typespec/fixture/GenericReturns.java"
          thread (Thread/currentThread)
          original (.getContextClassLoader thread)]
      (try
        (when-not (and compiler
                       (zero? (.run compiler nil nil nil (into-array String ["-d" (str root) source]))))
          (throw (ex-info "Could not compile the generic return fixture" {:source source})))
        (with-open [loader (URLClassLoader. (into-array URL [(.toURL (.toURI root))]) original)]
          (.setContextClassLoader thread loader)
          (try (test)
               (finally (.setContextClassLoader thread original))))
        (finally
          (doseq [file (reverse (file-seq root))]
            (io/delete-file file true)))))))

(defn- checked
  ([source] (checked source {}))
  ([source opts]
   (check/check-files [(parse/parse-string "interop.clj"
                                          (str "(ns interop (:import " owner "))\n" source))]
                      opts)))

(deftest inherits-class-variables-through-classes-and-interfaces
  (let [result (checked "(-> (GenericReturns/builder) (.settings nil) .fromInterface .build)")]
    (is (not-any? #(= :error (:severity %)) (:diagnostics result)) (pr-str (:diagnostics result)))
    (is (= product (main/type-at result "interop.clj" 2 1))))
  (is (= builder (:ret (first (java/methods-named builder "settings" false)))))
  (is (= builder (:ret (first (java/methods-named builder "fromInterface" false))))))

(deftest preserves-erasure-when-the-contract-does-not-prove-more
  (is (= (str owner "$Base") (:ret (first (java/methods-named (str owner "$Raw") "settings" false)))))
  (is (= (str owner "$Base") (:ret (first (java/methods-named builder "baseView" false)))))
  (is (= "java.lang.Object" (:ret (first (java/methods-named builder "methodVariable" false)))))
  (is (= "java.util.List"
         (:ret (first (java/methods-named (str owner "$NamedList") "fromInterface" false)))))
  (is (= "java.lang.Object"
         (:ret (first (java/methods-named (str owner "$RawList") "fromInterface" false)))))
  (let [result (checked "(-> (GenericReturns/builder) (.settings nil) .missingMethod)
                          (-> (GenericReturns/builder) .baseView .build)")]
    (is (= ["TSP2339" "TSP2339"] (mapv :code (:diagnostics result))) (pr-str (:diagnostics result)))))

(deftest prefers-the-covariant-method-over-its-bridge
  (let [methods (.getMethods ^Class (t/load-class builder))]
    (is (some #(and (= "build" (.getName ^java.lang.reflect.Method %))
                    (.isBridge ^java.lang.reflect.Method %)) methods))
    (is (= [product] (mapv :ret (java/methods-named builder "build" false))))))

(deftest generic-return-keeps-its-nullability-policy
  (let [lookup (checked "(.get (GenericReturns/products) \"missing\")")]
    (is (empty? (:diagnostics lookup)))
    (is (= (str "(s/nilable " product ")") (main/type-at lookup "interop.clj" 2 1))))
  (let [result (checked "(let [builder (GenericReturns/builder)]
                            (when (some? builder) (.build (.settings builder nil))))"
                        {:strict-nil true})]
    (is (= ["TSP2531"] (mapv :code (:diagnostics result))) (pr-str (:diagnostics result)))))

(deftest java-sequence-adapters-read-generic-element-classes
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s])
  (:import [java.util.jar JarFile JarEntry] [java.util.zip ZipFile ZipEntry]
           [java.net URL] [java.nio.file Path]))
(defn jar-entries [^JarFile jar] (.entries jar))
(defn zip-entries [^ZipFile zip] (.entries zip))
(defn resources [^ClassLoader loader] (.getResources loader \"clojure/core.clj\"))
(defn path-parts [^Path path] (.iterator path))
(s/fdef jar-name :args (s/cat :entry (partial instance? JarEntry)) :ret string?)
(defn jar-name [^JarEntry entry] (.getName entry))
(s/fdef zip-name :args (s/cat :entry (partial instance? ZipEntry)) :ret string?)
(defn zip-name [^ZipEntry entry] (.getName entry))
(s/fdef resource-name :args (s/cat :url (partial instance? URL)) :ret string?)
(defn resource-name [^URL url] (.getPath url))
(s/fdef path-name :args (s/cat :path (partial instance? Path)) :ret string?)
(defn path-name [path] (str path))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a])
  (:import [java.util.jar JarFile] [java.util.zip ZipFile] [java.util Enumeration] [java.nio.file Path]))
(defn jar-names [^JarFile jar] (mapv a/jar-name (enumeration-seq (a/jar-entries jar))))
(defn zip-names [^ZipFile zip] (mapv a/zip-name (enumeration-seq (a/zip-entries zip))))
(defn resource-names [^ClassLoader loader] (mapv a/resource-name (enumeration-seq (a/resources loader))))
(defn path-names [^Path path] (mapv a/path-name (iterator-seq (a/path-parts path))))
(defn bad-jar [^JarFile jar] (mapv inc (enumeration-seq (a/jar-entries jar))))
(defn bad-zip [^ZipFile zip] (mapv a/jar-name (enumeration-seq (a/zip-entries zip))))
(defn opaque [^Enumeration values] (mapv a/resource-name (enumeration-seq values)))
(defn choices [choose? ^JarFile jar ^ClassLoader loader]
  (mapv a/jar-name (enumeration-seq (if choose? (a/jar-entries jar) (a/resources loader)))))
(defn changing [^JarFile jar ^ClassLoader loader]
  (let [values (atom (a/jar-entries jar))]
    (reset! values (a/resources loader))
    (mapv a/jar-name (enumeration-seq @values))))")
        issues (:diagnostics (check/check-files [client api] {}))]
    (is (= [[7 "TSP2345"] [8 "TSP2345"] [9 "TSP210-01"] [11 "TSP210-01"] [15 "TSP210-01"]]
           (mapv (juxt (comp :row :pos) :code) issues))
        (pr-str issues))
    (is (every? #(= "client.clj" (:file %)) issues))
    (is (re-find #"java\.util\.zip\.ZipEntry" (:message (second issues)))))
  (is (= builder (:element-class (first (java/methods-named builder "values" false)))))
  (is (= "java.util.List"
         (:element-class (first (java/methods-named (str owner "$NamedList") "values" false)))))
  (is (nil? (:element-class (first (java/methods-named (str owner "$RawList") "values" false))))))

(deftest direct-java-iteration-keeps-the-reflected-element-class
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s])
  (:import [java.util.jar JarFile JarEntry] [java.util.zip ZipFile] [java.nio.file Path]))
(defn entries [^JarFile jar] (.entries jar))
(defn zip-entries [^ZipFile zip] (.entries zip))
(defn parts [^Path path] (.iterator path))
(s/fdef jar-name :args (s/cat :entry (partial instance? JarEntry)) :ret string?)
(defn jar-name [^JarEntry entry] (.getName entry))
(s/fdef path-name :args (s/cat :entry (partial instance? Path)) :ret string?)
(defn path-name [path] (str path))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a])
  (:import [java.util.jar JarFile] [java.util.zip ZipFile]
           [java.util Enumeration Iterator] [java.nio.file Path]))
(defn jar-good [^JarFile jar] (a/jar-name (.nextElement (a/entries jar))))
(defn zip-bad [^ZipFile zip] (a/jar-name (.nextElement (a/zip-entries zip))))
(defn path-good [^Path path] (a/path-name (Iterator/.next (a/parts path))))
(defn path-bad [^Path path] (inc (.next (a/parts path))))
(defn raw [^Enumeration entries] (.nextElement entries))
(defn arity [^JarFile jar] (.nextElement (a/entries jar) 12))
(defn qualified-bad [^Path path] (inc (Iterator/.next (a/parts path))))")
        result (check/check-files [client api] {})
        issues (:diagnostics result)
        errors (filter #(= :error (:severity %)) issues)]
    (is (= [[5 "TSP2345"] [7 "TSP2345"] [9 "TSP2554"] [10 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) errors))
        (pr-str issues))
    (is (every? #(= "client.clj" (:file %)) issues))
    (is (re-find #"java\.util\.zip\.ZipEntry" (:message (first errors))))
    (is (= "any?" (main/type-at result "client.clj" 8 34)))))
