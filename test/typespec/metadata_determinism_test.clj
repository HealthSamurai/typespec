(ns typespec.metadata-determinism-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [typespec.core-types :as core])
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util.concurrent TimeUnit)))

(defn- subprocess [root extra-cp expression]
  (let [cp (str/join File/pathSeparator
                     (concat extra-cp
                             (map #(.getAbsolutePath (io/file %))
                                  (str/split (System/getProperty "java.class.path")
                                             (re-pattern File/pathSeparator)))))
        log (File/createTempFile "output" ".log" root)
        proc (-> (ProcessBuilder. ^java.util.List
                                  [(str (io/file (System/getProperty "java.home") "bin/java"))
                                   "-cp" cp "clojure.main" "-e" expression])
                 (.directory root) (.redirectErrorStream true) (.redirectOutput log) .start)]
    (try
      (when-not (.waitFor proc 60 TimeUnit/SECONDS)
        (throw (ex-info "metadata subprocess timed out" {})))
      {:exit (.exitValue proc) :out (slurp log)}
      (finally (when (.isAlive proc) (.destroyForcibly proc) (.waitFor proc))))))

(defn- temp-root []
  (.toFile (Files/createTempDirectory "typespec-determinism-" (make-array FileAttribute 0))))

(defn- clean [root]
  (doseq [file (reverse (file-seq root))] (.delete file)))

(deftest fresh-and-preloaded-checks-match
  (let [root (temp-root)]
    (try
      (io/make-parents (io/file root "src/app.clj"))
      (spit (io/file root "src/app.clj")
            (str "(ns app (:require [clojure.set :as s] [clojure.test :refer [deftest is are]]))\n"
                 "(defn typo [a b] (s/unionn a b))\n"
                 "(deftest example (is (= 1 (inc \"bad\"))))\n"
                 "(are [x] (inc x) \"bad\")\n"))
      (let [check "(require 'typespec.main) (typespec.main/-main \"check\")"
            fresh (subprocess root [] check)
            loaded (subprocess root [] (str "(require 'clojure.set 'clojure.test) " check))]
        (is (= fresh loaded) (pr-str {:fresh fresh :loaded loaded}))
        (is (= 1 (:exit fresh)) (:out fresh))
        (is (str/includes? (:out fresh) "TSP2305") (:out fresh))
        (is (not (str/includes? (:out fresh) "TSP2345")) (:out fresh))
        (is (not (str/includes? (:out fresh) "TSP210-01")) (:out fresh)))
      (finally (clean root)))))

(deftest metadata-does-not-use-live-namespace-state
  (let [root (temp-root)
        query (str "(require '[typespec.core-types :as c]) "
                   "(prn [(c/stdlib-tagged? 'clojure.core/str) "
                   "(c/stdlib-macro? 'clojure.test/deftest) "
                   "(c/stdlib-macro? 'clojure.test/are) "
                   "(c/stdlib-macro? 'clojure.core.async/go) "
                   "(c/stdlib-publics 'clojure.core.async) "
                   "(contains? (c/stdlib-publics 'clojure.set) 'union)])")]
    (try
      (let [fresh (subprocess root [] query)
            mutated (subprocess root []
                       (str "(require 'clojure.set 'clojure.test) "
                            "(alter-meta! #'clojure.core/str dissoc :tag :arglists) "
                            "(alter-meta! #'clojure.test/deftest dissoc :macro) "
                            "(ns-unmap 'clojure.set 'union) "
                            "(create-ns 'clojure.core.async) "
                            "(intern 'clojure.core.async (with-meta 'go {:macro true}) identity) "
                            query))]
        (is (= 0 (:exit fresh)) (:out fresh))
        (is (= "[true true true false nil true]\n" (:out fresh)))
        (is (= 0 (:exit mutated)) (:out mutated))
        ;; clojure.main prints intermediate expression values as well.
        (is (str/ends-with? (:out mutated) (:out fresh)) (:out mutated)))
      (finally (clean root)))))

(deftest catalogue-fails-loudly
  (let [load-catalogue (deref (ns-resolve 'typespec.core-types 'load-stdlib-metadata))
        root (temp-root)]
    (try
      (let [resource (io/file root "catalogue.edn")
            loader (fn [urls]
                     (proxy [ClassLoader] [nil]
                       (getResources [_] (java.util.Collections/enumeration urls))))]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not found"
                              (load-catalogue (loader []))))
        (spit resource "{}")
        (let [url (.toURL (.toURI resource))]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"versioned public-var"
                                (load-catalogue (loader [url]))))
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"2 copies"
                                (load-catalogue (loader [url (java.net.URL. "file:/different.edn")])))))
        (doseq [value ["{:format" "{} {}" "#evil/tag {}"]]
          (spit resource value)
          (is (thrown? clojure.lang.ExceptionInfo
                       (load-catalogue (loader [(.toURL (.toURI resource))]))))))
      (finally (clean root)))))
