(ns typespec.extern-resources-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [typespec.externs :as externs]
            [typespec.main :as main])
  (:import (java.net URL URLClassLoader URLStreamHandler)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util Collections)
           (java.util.jar JarEntry JarOutputStream)))

(def resource "vendor/types.edn")

(defn with-project [texts f]
  (let [root (.toFile (Files/createTempDirectory "extern-resources-" (make-array FileAttribute 0)))
        jars (mapv (fn [i text]
                     (let [file (io/file root (str i ".jar"))]
                       (with-open [out (JarOutputStream. (io/output-stream file))]
                         (.putNextEntry out (JarEntry. resource))
                         (.write out (.getBytes ^String text "UTF-8"))
                         (.closeEntry out))
                       file)) (range) texts)]
    (try
      ;; All data lives outside cwd; only these real JARs are visible to the loader.
      (with-open [loader (URLClassLoader. (into-array URL (map #(.toURL (.toURI %)) jars)) nil)]
        (with-bindings {clojure.lang.Compiler/LOADER loader}
          (f root)))
      (finally (doseq [file (reverse (file-seq root))] (.delete file))))))

(defn load-resource [root & [cfg]]
  (externs/load-contracts root (merge {:extern-resources [resource]} cfg)))

(defn errors? [res]
  (and (seq (:diagnostics res))
       (every? #(= "TSP9003" (:code %)) (:diagnostics res))))

(deftest jar-loading-provenance-and-no-require
  (with-project ["{never.loaded/f [string? :-> :vendor/result] :vendor/result int?}"]
    (fn [root]
      (let [loaded (load-resource root)
            [env ds] (externs/merge-contracts {} loaded)
            v (get-in env [:vars 'never.loaded/f])]
        (is (empty? ds))
        (is (= :extern (:source v)))
        (is (= resource (:extern/resource v)))
        (is (= (str "classpath:" resource) (:extern/path v)))
        (is (nil? (find-ns 'never.loaded)))
        (is (re-find #"extern classpath:vendor/types.edn"
                     (first (main/sig-lines {:env env} 'never.loaded))))
        (let [[merged ds] (externs/merge-contracts {:vars {'never.loaded/f {:source :hints :body :kept}}} loaded)]
          (is (empty? ds))
          (is (= :kept (get-in merged [:vars 'never.loaded/f :body])))
          (is (= resource (get-in merged [:vars 'never.loaded/f :extern/resource]))))))))

(deftest duplicates-and-missing
  (with-project [] #(is (errors? (load-resource %))))
  (with-project ["{}" "{}"] #(is (errors? (load-resource %))))
  (with-project ["{vendor/f [int? :-> int?]}"]
    #(is (empty? (:diagnostics (load-resource % {:extern-resources [resource resource]}))))))

(deftest path-and-config-validation
  (is (empty? (:diagnostics (externs/load-contracts "." {}))))
  (is (empty? (:diagnostics (externs/load-contracts "." {:extern-resources []}))))
  (with-project ["{}"]
    (fn [root]
      (doseq [names [nil "vendor/types.edn" [1] [""] ["/vendor/types.edn"]
                     ["../types.edn"] ["a/../types.edn"] ["./types.edn"]
                     ["a//types.edn"] ["a\\types.edn"] ["http://host/a"]
                     ["a/%2e%2e/types.edn"] ["a?x"] ["a#x"]]]
        (is (errors? (load-resource root {:extern-resources names})) (pr-str names))))))

(deftest mixed-conflicts-equivalence-and-core-guard
  (with-project ["{vendor/f [int? :-> int?]}"]
    (fn [root]
      (spit (io/file root "api.edn") "{vendor/f [string? :-> string?]}")
      (let [res (load-resource root {:externs ["api.edn"]})]
        (is (errors? res))
        (is (= "api.edn" (get-in res [:vars 'vendor/f :extern/path]))))
      (spit (io/file root "api.edn") "{vendor/f [int? :-> int?]}")
      (is (empty? (:diagnostics (load-resource root {:externs ["api.edn"]}))))))
  (with-project ["{clojure.core/inc [string? :-> string?]}"]
    (fn [root]
      (is (errors? (load-resource root)))
      (is (empty? (:vars (load-resource root))))
      (is (empty? (:diagnostics (load-resource root {:externs-core :override})))))))

(deftest inert-malformed-and-missing-references
  (let [called (atom false)]
    (binding [*data-readers* {'evil/tag (fn [_] (reset! called true) {})}]
      (doseq [text ["" "#evil/tag {}" "#=(System/exit 1)" "[]" "{} {}" "{"
                    "{f [int? :-> int?]}" "{vendor/f int?}" "{vendor/f [bogus? :-> int?]}"]]
        (with-project [text] #(is (errors? (load-resource %)) text))))
    (is (false? @called)))
  (with-project ["{vendor/f [:missing/type :-> int?]}"]
    (fn [root]
      (let [[env ds] (externs/merge-contracts {} (load-resource root))]
        (is (= ["TSP9003"] (mapv :code ds)))
        (is (false? (get-in env [:vars 'vendor/f :complete?])))))))

(deftest unsafe-url-handlers-are-never-opened
  (doseq [address ["https://example.invalid/types.edn" "jar:https://example.invalid/a.jar!/vendor/types.edn"
                   "file://example.invalid/types.edn" "custom:/types.edn"]]
    (let [opened (atom false)
          handler (proxy [URLStreamHandler] []
                    (openConnection [_] (reset! opened true) (throw (Exception. "must not open"))))
          url (URL. nil address handler)
          loader (proxy [ClassLoader] [nil]
                   (getResources [_] (Collections/enumeration [url])))]
      (with-bindings {clojure.lang.Compiler/LOADER loader}
        (is (errors? (load-resource ".")) address))
      (is (false? @opened) address))))

(deftest local-directory-resource
  (with-project []
    (fn [root]
      (let [f (io/file root resource)]
        (io/make-parents f)
        (spit f "{vendor/f [int? :-> int?]}")
        (with-open [loader (URLClassLoader. (into-array URL [(.toURL (.toURI root))]) nil)]
          (with-bindings {clojure.lang.Compiler/LOADER loader}
            (is (empty? (:diagnostics (load-resource root))))))))))

(deftest duplicate-entries-within-one-jar
  (with-project []
    (fn [root]
      (let [jar (io/file root "duplicates.jar")
            other "vendor/typez.edn"]
        (with-open [out (JarOutputStream. (io/output-stream jar))]
          (doseq [name [resource other]]
            (.putNextEntry out (JarEntry. name))
            (.write out (.getBytes "{}" "UTF-8"))
            (.closeEntry out)))
        ;; JarOutputStream refuses duplicates. Rewrite equal-length entry names in
        ;; both ZIP headers, leaving compressed contents and their CRC untouched.
        (let [bytes (Files/readAllBytes (.toPath jar))
              text (String. bytes "ISO-8859-1")]
          (with-open [out (io/output-stream jar)]
            (.write out (.getBytes (.replace text other resource) "ISO-8859-1"))))
        (with-open [loader (URLClassLoader. (into-array URL [(.toURL (.toURI jar))]) nil)]
          (with-bindings {clojure.lang.Compiler/LOADER loader}
            (let [res (load-resource root)]
              (is (errors? res))
              (is (re-find #"duplicate entry" (-> res :diagnostics first :message))))))))))
