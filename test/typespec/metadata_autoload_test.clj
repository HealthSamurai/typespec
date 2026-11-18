(ns typespec.metadata-autoload-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [typespec.core-types :as core])
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util.concurrent TimeUnit)))

(deftest metadata-lookups-never-autoload
  ;; Also guards less common branches, irrespective of what the test runner loaded.
  (with-redefs [clojure.core/require (fn [& _] (throw (AssertionError. "require")))
                clojure.core/requiring-resolve (fn [& _] (throw (AssertionError. "requiring-resolve")))]
    (is (core/stdlib-macro? 'clojure.core/when))
    (is (core/stdlib-tagged? 'clojure.core/str))
    (is (contains? (core/stdlib-publics 'clojure.core) 'map))
    (is (false? (core/stdlib-macro? 'clojure.core.async/nonexistent)))
    (is (false? (core/stdlib-tagged? 'clojure.set/nonexistent)))
    (core/stdlib-publics 'clojure.walk)))

(deftest check-does-not-execute-core-async-dependency
  ;; clojure.main may run user.clj at JVM startup. That is outside the checker;
  ;; this fixture deliberately has no user.clj, and attacks checker metadata only.
  (let [root (.toFile (Files/createTempDirectory "typespec-metadata-" (make-array FileAttribute 0)))
        project (doto (io/file root "project") .mkdirs)
        dependency (doto (io/file root "dependency") .mkdirs)
        marker (io/file root "executed")
        log (io/file root "output")
        cp (str/join File/pathSeparator
                     (cons (.getAbsolutePath dependency)
                           (map #(.getAbsolutePath (io/file %))
                                (str/split (System/getProperty "java.class.path")
                                           (re-pattern File/pathSeparator)))))
        java (str (io/file (System/getProperty "java.home") "bin" "java"))]
    (try
      (io/make-parents (io/file dependency "clojure/core/async.clj"))
      (spit (io/file dependency "clojure/core/async.clj")
            (str "(ns clojure.core.async)\n(spit " (pr-str (str marker)) " \"executed\")\n"
                 "(defmacro go [& body] `(do ~@body))\n"))
      (io/make-parents (io/file project "src/app.clj"))
      (spit (io/file project "src/app.clj")
            "(ns app (:require [clojure.core.async :as async]))\n(defn work [] (async/go 1))\n(inc \"bad\")\n")
      (let [proc (-> (ProcessBuilder. ^java.util.List
                                     [java "-cp" cp "clojure.main" "-m" "typespec.main" "check"])
                     (.directory project)
                     (.redirectErrorStream true)
                     (.redirectOutput log)
                     .start)]
        (try
          (let [finished (.waitFor proc 60 TimeUnit/SECONDS)]
            (is finished "checker subprocess timed out")
            (when finished
              (let [out (slurp log)]
                (is (= 1 (.exitValue proc)) out)
                ;; Prove check actually reached analysis, not merely failed to start.
                (is (re-find #"src/app\.clj:3:\d+\s+TSP\d+" out) out)
                (is (str/includes? out "error(s)") out)))
            (is (not (.exists marker)) "checker executed a dependency during metadata lookup"))
          (finally
            (when (.isAlive proc)
              (.destroyForcibly proc)
              (.waitFor proc)))))
      (finally
        (doseq [file (reverse (file-seq root))] (.delete file))))))
