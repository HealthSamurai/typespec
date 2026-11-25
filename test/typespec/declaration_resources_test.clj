(ns typespec.declaration-resources-test
  "Built-in declarations live in inert EDN resources. These tests pin how they are found,
  read and shipped; the declarations themselves have one home, the resource files."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [typespec.core-types :as core]
            [typespec.externs :as externs]
            [typespec.types :as t])
  (:import (java.io File)
           (java.net URL URLClassLoader)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util.jar JarEntry JarOutputStream)))

(def repo-root
  ;; Found from the loaded source, not from the working directory.
  (let [url (io/resource "typespec/core_types.clj")]
    (assert (= "file" (.getProtocol url)) "tests run from a source checkout")
    (-> (io/file (.toURI url)) .getParentFile .getParentFile .getParentFile)))

(defn- temp-dir ^File []
  (.toFile (Files/createTempDirectory "typespec-decl-" (make-array FileAttribute 0))))

(defn- delete-tree [^File dir]
  (doseq [^File f (reverse (file-seq dir))] (.delete f)))

(defn- shipped-text [ns-sym]
  (slurp (io/file repo-root "resources" (core/resource-name ns-sym))))

(defn- write-resources!
  "Writes one directory of declaration resources. `texts` maps a namespace to file text;
  a namespace mapped to nil gets no file, an absent one gets the shipped file."
  [^File dir texts]
  (doseq [ns-sym core/declared-namespaces
          :let [text (if (contains? texts ns-sym) (texts ns-sym) (shipped-text ns-sym))]
          :when text]
    (let [f (io/file dir (core/resource-name ns-sym))]
      (io/make-parents f)
      (spit f text))))

(defn- loader-of
  "A class loader that sees only these directories: no parent, so nothing leaks in from the
  classpath of the test run."
  ^URLClassLoader [dirs]
  (URLClassLoader. ^"[Ljava.net.URL;" (into-array URL (map #(.toURL (.toURI ^File %)) dirs)) nil))

(defn- load-from
  "Loads declarations from temporary directories, one map of texts per directory."
  [& texts-per-dir]
  (let [dirs (mapv (fn [texts] (doto (temp-dir) (write-resources! texts))) texts-per-dir)]
    (try
      (with-open [loader (loader-of dirs)] (core/load-declarations loader))
      (finally (run! delete-tree dirs)))))

(defn- failure
  "ex-data of the failure of loading, or nil if loading succeeded."
  [& texts-per-dir]
  (try (apply load-from texts-per-dir) nil
       (catch clojure.lang.ExceptionInfo e (assoc (ex-data e) :message (ex-message e)))))

;; ---------------------------------------------------------------------------------------
;; loading

(deftest resources-are-on-the-classpath-exactly-once
  (is (= '[clojure.core clojure.string clojure.spec.alpha] core/declared-namespaces))
  (is (= "typespec/types/clojure/core.edn" (core/resource-name 'clojure.core)))
  (is (= "typespec/types/clojure/string.edn" (core/resource-name 'clojure.string)))
  (doseq [ns-sym core/declared-namespaces
          :let [urls (enumeration-seq (.getResources (clojure.lang.RT/baseLoader) (core/resource-name ns-sym)))]]
    (is (= 1 (count (distinct (map str urls)))) (str ns-sym " => " (pr-str urls)))))

(deftest api-is-the-content-of-the-resources
  (let [plain (fn [ns-sym] (edn/read-string (shipped-text ns-sym)))
        files (into {} (map plain) core/declared-namespaces)
        ctx {:aliases {} :imports {} :fail (fn [form] (throw (ex-info "bad type" {:form form})))}]
    (is (seq core/externs))
    (is (= files core/externs) "externs is exactly what the files say")
    (is (= (set (keys core/externs)) (set (keys core/table))))
    (is (every? #(= :fn (:t %)) (vals core/table)))
    (is (= (into {} (map (fn [[k form]] [k (t/parse-sig ctx form)])) files) core/table))
    (doseq [ns-sym core/declared-namespaces]
      (is (seq (plain ns-sym)) (str ns-sym " declares something"))
      (is (every? #(= (str ns-sym) (namespace %)) (keys (plain ns-sym)))))
    (is (= (load-from {}) {:externs core/externs :table core/table})
        "an isolated loader with the shipped files gives the same result")))

(deftest declarations-survive-the-move-to-edn
  ;; A sample of shapes, not a second copy of the table: a symbol that is hard to read,
  ;; rest parameters, several arities, a nullary arity, a class, a function parameter.
  (are [sym form] (= 'form (core/externs 'sym))
    clojure.core//           [number? & number? :-> number?]
    clojure.core/str         [& any? :-> string?]
    clojure.core/inc         ([int? :-> int?] [number? :-> number?])
    clojure.core/random-uuid [:-> uuid?]
    clojure.core/namespace   [clojure.lang.Named :-> (s/nilable string?)]
    clojure.string/blank?    [(s/nilable java.lang.CharSequence) :-> boolean?]
    clojure.string/split     ([java.lang.CharSequence java.util.regex.Pattern :-> (s/coll-of string? :kind vector?)]
                              [java.lang.CharSequence java.util.regex.Pattern int? :-> (s/coll-of string? :kind vector?)])
    clojure.string/replace   ([some? java.lang.Character java.lang.Character :-> string?]
                              [some? java.lang.CharSequence java.lang.CharSequence :-> string?]
                              [some? java.util.regex.Pattern java.lang.CharSequence :-> string?]
                              [some? java.util.regex.Pattern
                               [(t/or string? (s/coll-of (s/nilable string?) :kind vector?)) :-> string?]
                               :-> string?]))
  (let [arities #(mapv (fn [a] [(count (:params a)) (boolean (:rest a))]) (:arities (core/table %)))]
    (is (= [[1 true]] (arities 'clojure.core//)))
    (is (= [[0 false] [1 false] [2 false] [2 false] [3 false] [3 false]]
           (arities 'clojure.core/range)))
    (is (= [[3 false] [3 false] [3 false] [3 false]] (arities 'clojure.string/replace)))
    (is (= :fn (get-in core/table ['clojure.string/replace :arities 3 :params 2 :t])))))

(deftest built-in-files-are-ordinary-extern-files
  ;; Same format as a project's :externs: the user loader reads the shipped files as they
  ;; are and arrives at the same types.
  (let [root (temp-dir)]
    (try
      (doseq [ns-sym core/declared-namespaces]
        (spit (io/file root (str ns-sym ".edn")) (shipped-text ns-sym)))
      (let [cfg {:externs (mapv #(str % ".edn") core/declared-namespaces) :externs-core :override}
            res (externs/load-contracts (str root) cfg)]
        (is (empty? (:diagnostics res)))
        (is (empty? (:specs res)))
        (is (= core/table (update-vals (:vars res) :type))))
      (finally (delete-tree root)))))

;; ---------------------------------------------------------------------------------------
;; strict failures: there is no partial table

(deftest missing-resource-fails
  (let [f (failure {'clojure.string nil})]
    (is (= :missing (:reason f)))
    (is (= "typespec/types/clojure/string.edn" (:resource f)))
    (is (str/includes? (:message f) "typespec/types/clojure/string.edn")))
  (is (= :missing (:reason (failure {'clojure.core nil}))))
  (is (= :missing (:reason (failure {'clojure.core nil 'clojure.string nil})))))

(deftest duplicated-resource-fails
  ;; A second copy must not silently replace the built-in types, whichever comes first.
  (let [other "{clojure.core/inc [string? :-> string?]}"]
    (doseq [f [(failure {} {'clojure.core other 'clojure.string nil})
               (failure {'clojure.core other 'clojure.string nil} {})
               (failure {} {})]]
      (is (= :duplicate (:reason f)))
      (is (= 2 (count (:urls f)))))))

(deftest malformed-resource-fails
  (are [reason text] (= [reason "typespec/types/clojure/core.edn"]
                        ((juxt :reason :resource) (failure {'clojure.core text})))
    :malformed ""
    :malformed "   ;; only a comment\n"
    :malformed "{clojure.core/inc [int? :-> int?]"
    :malformed "[clojure.core/inc [int? :-> int?]]"
    :malformed "{clojure.core/inc [int? :-> int?]} {clojure.core/dec [int? :-> int?]}"
    :malformed "{clojure.core/inc [int? :-> int?]} trailing"
    :malformed "{clojure.core/inc [int? :-> int?] clojure.core/inc [number? :-> number?]}"
    :malformed "{clojure.core/inc #typespec/sig [int? :-> int?]}"
    :malformed "{clojure.core/inc #\"regex\"}"
    :bad-key "{inc [int? :-> int?]}"
    :bad-key "{:clojure.core/inc [int? :-> int?]}"
    :bad-key "{\"clojure.core/inc\" [int? :-> int?]}"
    :bad-key "{clojure.string/trim [string? :-> string?]}"
    :bad-key "{vendor.api/f [int? :-> int?]}"
    ;; #inst and #uuid are part of EDN itself, so they are read, and then are not a type.
    :bad-signature "{clojure.core/inc #inst \"2020-01-01\"}"
    :bad-signature "{clojure.core/inc #uuid \"00000000-0000-0000-0000-000000000000\"}"
    ;; EDN has no quote: this is a symbol named 'quoted.
    :bad-signature "{clojure.core/inc 'quoted}"
    :bad-signature "{clojure.core/inc int?}"
    :bad-signature "{clojure.core/inc nil}"
    :bad-signature "{clojure.core/inc ()}"
    :bad-signature "{clojure.core/inc [int? int?]}"
    :bad-signature "{clojure.core/inc [int? :->]}"
    :bad-signature "{clojure.core/inc [int? :-> int? int?]}"
    :bad-signature "{clojure.core/inc ([int? :-> int?] int?)}"
    :bad-signature "{clojure.core/inc [no-such-type? :-> int?]}"
    :bad-signature "{clojure.core/inc [int? :-> no.such.Class]}"
    :bad-signature "{clojure.core/inc [:unqualified :-> int?]}")
  (testing "one bad entry rejects the whole file, good entries included"
    (is (= :bad-signature
           (:reason (failure {'clojure.core "{clojure.core/inc [int? :-> int?] clojure.core/dec [oops :-> int?]}"})))))
  (testing "the second file is checked as strictly as the first"
    (is (= [:bad-key "typespec/types/clojure/string.edn"]
           ((juxt :reason :resource) (failure {'clojure.string "{clojure.core/inc [int? :-> int?]}"}))))))

(deftest nothing-is-evaluated
  (let [dir (temp-dir)
        marker (io/file dir "evaluated")
        code (pr-str (list 'spit (str marker) "x"))]
    (try
      (testing "read-eval is not EDN"
        (is (= :malformed (:reason (failure {'clojure.core (str "{clojure.core/inc #=" code "}")})))))
      (testing "a list is a signature form, never a call"
        (is (= :bad-signature (:reason (failure {'clojure.core (str "{clojure.core/inc " code "}")}))))
        (is (= :bad-signature (:reason (failure {'clojure.core (str "{clojure.core/inc [" code " :-> int?]}")})))))
      (is (not (.exists marker)))
      (finally (delete-tree dir))))
  (testing "a declared var is not resolved or required: it does not have to exist"
    (let [res (load-from {'clojure.core "{clojure.core/no-such-var-at-all [int? :-> int?]}"})]
      (is (contains? (:table res) 'clojure.core/no-such-var-at-all)))))

;; ---------------------------------------------------------------------------------------
;; distribution: what deps.edn publishes, used from a directory that is not the repository

(defn- jar-of!
  "Packs the directories into a jar, the way a consumer of deps.edn :paths sees them."
  [^File jar dirs]
  (with-open [out (JarOutputStream. (io/output-stream jar))]
    (doseq [^File dir dirs
            ^File f (sort (file-seq dir))
            :when (.isFile f)
            :let [base (.toPath dir)]]
      (.putNextEntry out (JarEntry. (str/replace (str (.relativize base (.toPath f))) File/separator "/")))
      (io/copy f out)
      (.closeEntry out))))

(defn- check-in-new-jvm
  "Runs `typespec.main check` in a fresh JVM whose classpath is `jar` plus the library jars
  of this run, with `cwd` as its working directory. No directory of the repository is on
  that classpath."
  [^File jar ^File cwd]
  (let [libs (filter #(str/ends-with? % ".jar")
                     (str/split (System/getProperty "java.class.path") (re-pattern File/pathSeparator)))
        cp (str/join File/pathSeparator (cons (str jar) libs))
        java (str (io/file (System/getProperty "java.home") "bin" "java"))
        proc (-> (ProcessBuilder. ^java.util.List [java "-cp" cp "clojure.main" "-m" "typespec.main" "check"])
                 (.directory cwd)
                 (.redirectErrorStream true)
                 .start)
        out (slurp (.getInputStream proc))]
    {:exit (.waitFor proc) :out out}))

(deftest packaged-declarations-work-outside-the-repository
  (let [dir (temp-dir)
        project (doto (io/file dir "project") .mkdirs)
        paths (:paths (edn/read-string (slurp (io/file repo-root "deps.edn"))))]
    (try
      (io/make-parents (io/file project "src/app.clj"))
      (spit (io/file project "src/app.clj")
            (str "(ns app (:require [clojure.string :as str]))\n"
                 "(defn ok [s] (str/upper-case (str/trim s)))\n"
                 "(str/upper-case 42)\n"
                 "(inc \"a\")\n"))
      (testing "the paths published by deps.edn carry code and declarations"
        (is (= ["src" "resources"] paths))
        (let [jar (io/file dir "typespec.jar")
              _ (jar-of! jar (map #(io/file repo-root %) paths))
              {:keys [exit out]} (check-in-new-jvm jar project)]
          (is (= 1 exit) out)
          (is (re-find #"src/app\.clj:3:\d+\s+TSP\d+" out) out)
          (is (re-find #"src/app\.clj:4:\d+\s+TSP\d+" out) out)
          (is (str/includes? out "2 error(s)") out)))
      (testing "code shipped without the declarations refuses to start"
        (let [jar (io/file dir "typespec-src-only.jar")
              _ (jar-of! jar [(io/file repo-root "src")])
              {:keys [exit out]} (check-in-new-jvm jar project)]
          ;; clojure.main exits with 1 here, the same code as "type errors found": the
          ;; exit code alone only says "not clean", the output says which.
          (is (not (zero? exit)) out)
          (is (str/includes? out "typespec/types/clojure/core.edn: not found on the classpath") out)
          (is (not (str/includes? out "error(s)")) out)))
      (finally (delete-tree dir)))))
