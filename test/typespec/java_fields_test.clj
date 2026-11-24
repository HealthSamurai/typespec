(ns typespec.java-fields-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [typespec.check :as check]
            [typespec.main :as main]
            [typespec.parse :as parse])
  (:import [java.net URL URLClassLoader]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [javax.tools ToolProvider]))

(deftest static-field-types-are-used-at-call-sites
  (let [source "(ns fields (:require [clojure.string :as str]) (:import [java.nio.charset StandardCharsets] [java.util Locale]))
Integer/MAX_VALUE
Math/PI
(Math/PI)
(StandardCharsets/UTF_8)
(.getDisplayName Locale/ROOT)
(.missingMember Locale/ROOT)
(inc StandardCharsets/UTF_8)
(String. (byte-array [65]) StandardCharsets/UTF_8)
(into-array Integer [Integer/MAX_VALUE])
(into-array Long [Integer/MAX_VALUE])
(java.awt.Point/x)
(defn method-value [] Math/abs)
Integer/TYPE
(str/replace \"dir/file\" \\/ java.io.File/separatorChar)"
        result (check/check-files [(parse/parse-string "fields.clj" source)] {})]
    (is (= [[7 2 "TSP2339"] [8 6 "TSP2345"] [11 19 "TSP2345"] [12 2 "TSP2339"]]
           (mapv (juxt (comp :row :pos) (comp :col :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))
    (is (= ["int" "double" "double" "java.nio.charset.Charset" "String" "string?"
            "[Ljava.lang.Integer;" "Class" "string?"]
           (mapv #(main/type-at result "fields.clj" % 1) [2 3 4 5 6 9 10 14 15])))))

(deftest instance-fields-check-the-receiver-and-return-the-declared-type
  (let [source "(ns fields (:import [java.awt Point] [java.io File]))
(.-x (Point. 1 2))
(.x (Point. 1 2))
(inc (.-x (Point. 1 2)))
(alength (.-x (Point. 1 2)))
(.-missing (Point.))
(.-separator (File. \"x\"))
(.-separator File)
(.-x Point)
(.-x nil)
(.-x (Point.) 0)
(.-x)
(.-x (java.awt.Rectangle. 1 2 3 4))
(let [Point (Point.)] (.-x Point))
(defn dynamic-field [target] (.-x target))"
        result (check/check-files [(parse/parse-string "fields.clj" source)] {})]
    (is (= [[5 10 "TSP2345"] [6 2 "TSP2339"] [7 2 "TSP2339"] [8 2 "TSP2339"]
            [9 2 "TSP2339"] [10 6 "TSP2531"] [11 1 "TSP2554"] [12 1 "TSP2554"]]
           (mapv (juxt (comp :row :pos) (comp :col :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))
    (is (= ["int" "int" "int" "int"]
           (mapv #(main/type-at result "fields.clj" % 1) [2 3 13 14])))))

(deftest instance-field-receivers-respect-strict-nil-and-guards
  (let [source "(ns fields (:import java.awt.Point))
(defn unchecked [^Point p] (.-x p))
(defn guarded [^Point p] (when (some? p) (.-x p)))"
        result (check/check-files [(parse/parse-string "fields.clj" source)] {:strict-nil true})]
    (is (= [[2 "TSP2531"]] (mapv (juxt (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))))

(deftest instance-field-types-cross-namespace-contracts
  (let [files [(parse/parse-string
                "provider.clj"
                "(ns provider (:require [clojure.spec.alpha :as s]) (:import java.awt.Point))
(s/fdef point :args (s/cat) :ret #(instance? Point %))
(defn point [] (Point. 1 2))")
               (parse/parse-string
                "consumer.clj"
                "(ns consumer (:require [provider :as p]))
(inc (.-x (p/point)))
(alength (.-x (p/point)))")]
        result (check/check-files files {})]
    (is (= [["consumer.clj" 3 10 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) (comp :col :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))))

(deftest reference-fields-are-platform-types
  (let [source "(ns fields (:import java.nio.charset.StandardCharsets))
(.name StandardCharsets/UTF_8)"
        result (check/check-files [(parse/parse-string "fields.clj" source)] {:strict-nil true})]
    (is (= ["TSP2531"] (mapv :code (:diagnostics result)))
        (pr-str (:diagnostics result)))))

(deftest static-field-types-cross-namespace-contracts
  (let [files [(parse/parse-string
                "provider.clj"
                "(ns provider (:require [clojure.spec.alpha :as s])
                  (:import [java.nio.charset Charset StandardCharsets]))
(s/fdef charset :args (s/cat) :ret #(instance? Charset %))
(defn charset [] StandardCharsets/UTF_8)")
               (parse/parse-string
                "consumer.clj"
                "(ns consumer (:require [provider :as p]))
(String. (byte-array [65]) (p/charset))
(inc (p/charset))")]
        result (check/check-files files {})]
    (is (= [["consumer.clj" 3 6 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) (comp :col :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))))

(deftest field-metadata-keeps-method-lookup-and-does-not-initialize-the-class
  (let [root (.toFile (Files/createTempDirectory "typespec-static-fields-" (make-array FileAttribute 0)))
        compiler (ToolProvider/getSystemJavaCompiler)
        source "test/fixtures/java/typespec/fixture/StaticFields.java"
        thread (Thread/currentThread)
        original (.getContextClassLoader thread)]
    (try
      (when-not (zero? (.run compiler nil nil nil (into-array String ["-d" (str root) source])))
        (throw (ex-info "Could not compile static field fixture" {:source source})))
      (with-open [loader (URLClassLoader. (into-array URL [(.toURL (.toURI root))]) original)]
        (.setContextClassLoader thread loader)
        (try
          (let [file (parse/parse-string
                      "fixture.clj"
                      "(ns example (:import [typespec.fixture StaticFields StaticFields$Members]))
StaticFields/number
(StaticFields/number)
StaticFields/text
(StaticFields/text)
(StaticFields/text 1)
StaticFields/labels
(aset StaticFields/labels 0 42)
(.-size (StaticFields$Members.))
(.size (StaticFields$Members.))
(.alt (StaticFields$Members.))
(.alt (StaticFields$Members.) 1)
(.-labels (StaticFields$Members.))
(aset (.-labels (StaticFields$Members.)) 0 42)")
                result (check/check-files [file] {})]
            (is (= ["int" "String" "String" "int" "[Ljava.lang.String;"
                    "int" "String" "int" "String" "[Ljava.lang.String;"]
                   (mapv #(main/type-at result "fixture.clj" % 1) [2 3 4 6 7 9 10 11 12 13])))
            (is (= [[5 "TSP2554"] [8 "TSP2345"] [14 "TSP2345"]]
                   (mapv (juxt (comp :row :pos) :code) (:diagnostics result)))
                (pr-str (:diagnostics result))))
          (finally (.setContextClassLoader thread original))))
      (finally
        (doseq [file (reverse (file-seq root))]
          (io/delete-file file true))))))
