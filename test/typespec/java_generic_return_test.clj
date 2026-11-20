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
