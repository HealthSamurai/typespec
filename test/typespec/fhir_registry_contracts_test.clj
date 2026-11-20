(ns typespec.fhir-registry-contracts-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [typespec.parse :as parse]
            [typespec.check :as check]
            [typespec.externs :as externs]))

(def overlay "examples/fhir-registry-check/externs.edn")
(defn checked [source]
  (check/check-files [(parse/parse-string "contracts.clj" source)]
    {:extern-contracts (externs/load-contracts "." {:externs [overlay]})}))
(def prefix "(ns app (:require [fhir.registry.semver :as v] [clojure.java.io :as io])) ")

(deftest contracts-load-without-override
  (is (empty? (:diagnostics (externs/load-contracts "." {:externs [overlay]})))))

(deftest accepted-and-rejected-calls
  (doseq [call ["(v/parse-semver nil)" "(v/parse-semver \"1.2.3\")"
                "(v/compare-pre-release nil [\"rc\" \"1\"])" "(v/semver-comparator nil \"1\")"
                "(v/latest [])" "(v/latest nil)" "(v/latest [nil \"1\"])"
                "(io/file nil)" "(io/file \"x\")" "(io/file nil \"x\")"
                "(io/reader \"x\")" "(io/writer \"x\")" "(io/output-stream \"x\")"]]
    (is (empty? (:diagnostics (checked (str prefix call)))) call))
  (doseq [call ["(v/parse-semver 42)" "(v/compare-pre-release [42] nil)"
                "(v/semver-comparator 42 nil)" "(v/latest [42])"
                "(io/file 42)" "(io/reader nil)" "(io/writer nil)" "(io/output-stream nil)"]]
    (is (some #(= "TSP2345" (:code %)) (:diagnostics (checked (str prefix call)))) call)))

(deftest source-bodies-are-not-trusted
  (doseq [source ["(ns fhir.registry.semver) (defn parse-semver [s] 42)"
                  "(ns fhir.registry.semver) (defn compare-pre-release [a b] \"wrong\")"
                  "(ns fhir.registry.semver) (defn semver-comparator [a b] \"wrong\")"
                  "(ns fhir.registry.semver) (defn latest [xs] 42)"]]
    (is (some #(= :error (:severity %)) (:diagnostics (checked source))) source)))

(deftest nullable-results-are-not-erased
  (doseq [expr ["(v/latest [])" "(v/parse-semver nil)" "(io/file nil)"]]
    (is (some #(= :error (:severity %))
              (:diagnostics (checked (str prefix "(defn f {:t/sig '[:-> string?]} [] " expr ")")))) expr)))

(deftest default-io-runtime-contracts
  ;; In-memory streams only: no network, filesystem writes, DB or cloud.
  (is (nil? (io/file nil)))
  (is (instance? java.io.File (io/file "x")))
  (is (instance? java.io.File (io/file nil "x")))
  (with-open [r (io/reader (java.io.StringReader. "ok"))]
    (is (instance? java.io.BufferedReader r))
    (is (= "ok" (.readLine r))))
  (with-open [w (io/writer (java.io.StringWriter.))]
    (is (instance? java.io.BufferedWriter w)))
  (with-open [s (io/output-stream (java.io.ByteArrayOutputStream.))]
    (is (instance? java.io.BufferedOutputStream s)))
  (doseq [f [io/file io/reader io/writer io/output-stream]]
    (is (thrown? IllegalArgumentException (f 42))))
  (doseq [f [io/reader io/writer io/output-stream]]
    (is (thrown? IllegalArgumentException (f nil)))))

(deftest pinned-pure-semver-runtime
  ;; Read only this reviewed pure source. Do not require upstream namespaces;
  ;; evaluate only its four defns in an isolated temporary namespace.
  (let [path ".hyper/tmp/ceya/upstream/src/fhir/registry/semver.clj"]
    (if-not (.exists (io/file path))
      (is false "Missing pinned checkout: run examples/fhir-registry-check/prepare.sh first")
      (let [bytes (java.nio.file.Files/readAllBytes (.toPath (io/file path)))
            sha (.formatHex (java.util.HexFormat/of)
                  (.digest (java.security.MessageDigest/getInstance "SHA-256") bytes))
            _ (assert (= "3b44c5f13463146d8896a37d1375138fc671d0d26c04773f3028998a2c3c8f5b" sha)
                      "Pinned pure source changed; refusing evaluation")
            n (create-ns (gensym "semver-probe-"))]
        (try
          (binding [*ns* n]
            (clojure.core/refer 'clojure.core)
            (alias 'str 'clojure.string)
            (with-open [r (java.io.PushbackReader. (java.io.StringReader. (String. bytes "UTF-8")))]
              (binding [*read-eval* false]
                (doseq [form (take-while #(not= ::eof %) (repeatedly #(read {:eof ::eof} r)))
                        :when (and (seq? form) (= 'defn (first form)))]
                  (eval form))))
            (let [p (ns-resolve n 'parse-semver) c (ns-resolve n 'compare-pre-release)
                  v (ns-resolve n 'semver-comparator) l (ns-resolve n 'latest)]
              (is (nil? (p nil)))
              (is (= {:major 1 :minor 2 :patch 3 :pre-release ["rc" "1"] :build "x"} (p "1.2.3-rc.1+x")))
              (is (= 0 (:major (p "."))))
              (is (= 0 (:major (p ""))))
              (is (nil? (:pre-release (p "1"))))
              (is (nil? (:build (p "1"))))
              (is (= 0 (c nil nil)))
              (is (neg? (c ["rc"] nil)))
              (is (= 0 (v nil nil)))
              (is (neg? (v "1" "2")))
              (is (= "2" (l ["1" "2"])))
              (is (nil? (l []))) (is (nil? (l nil))) (is (nil? (l [nil])))
              (doseq [f [#(p 42) #(c [42] ["a"]) #(v 42 "1") #(l [42])]]
                (is (thrown? Exception (f))))))
          (finally (remove-ns (ns-name n))))))))
