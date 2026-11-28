(ns typespec.contract-diff-test
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is testing]]
            [typespec.main :as main]))

(defn- write-project [root files]
  (doseq [[path content] files
          :let [file (io/file root "src" path)]]
    (io/make-parents file)
    (spit file content)))

(defn- compare-fixture [before after verify]
  (let [temp (.toFile (java.nio.file.Files/createTempDirectory
                        "typespec-contract-diff" (make-array java.nio.file.attribute.FileAttribute 0)))
        base (io/file temp "base")
        head (io/file temp "head")]
    (try
      (write-project base before)
      (write-project head after)
      (verify (main/run-contract-diff (str base) (str head) nil {}) base head)
      (finally
        (doseq [file (reverse (file-seq temp))]
          (io/delete-file file))))))

(def ^:private api-ns "(ns api (:require [clojure.spec.alpha :as s]))\n")

(defn- compare-api [before after]
  (compare-fixture {"api.clj" (str api-ns before)} {"api.clj" (str api-ns after)}
                   (fn [report _ _] (:changes report))))

(defn- statuses [changes]
  (mapv (juxt :status :kind) changes))

(deftest tracks-transitive-return-field-changes-to-the-consumer
  (let [schema (fn [id-type account]
                 (str "(ns catalog.schema (:require [clojure.spec.alpha :as s]))
(s/def ::id " id-type ")
(s/def ::account " account ")"))
        api (fn [value]
              (str "(ns catalog.api (:require [clojure.spec.alpha :as s] [catalog.schema :as schema]))
(s/fdef account :args (s/cat) :ret ::schema/account)
(defn account [] " value ")"))
        client "(ns view.page (:require [catalog.api :as api] [clojure.string :as str]))
(str/trim (:id (api/account)))"
        revision (fn [type shape value]
                   {"catalog/schema.clj" (schema type shape)
                    "catalog/api.clj" (api value)
                    "view/page.clj" client})
        base (revision "string?" "(s/keys :req-un [::id])" "{:id \"A\"}")]
    (doseq [[after expected-path] [[(revision "int?" "(s/keys :req-un [::id])" "{:id 1}")
                                    ["catalog.api/account" ":catalog.schema/account" ":catalog.schema/id"]]
                                   [(revision "string?" "(s/keys)" "{}")
                                    ["catalog.api/account" ":catalog.schema/account"]]]]
      (compare-fixture base after
        (fn [{:keys [changes diagnostics]} _ _]
          (let [issue (first changes)]
            (is (= [[:breaking :return]] (statuses changes)) (pr-str changes))
            (is (= expected-path (get-in issue [:spec-changes 0 :path])))
            (is (= (get-in issue [:before :signature]) (get-in issue [:after :signature])))
            (is (= ["src/view/page.clj" 2 :call]
                   ((juxt :file (comp :row :pos) :kind) (first (:uses issue)))))
            (is (empty? (:before diagnostics)) (pr-str diagnostics))
            (is (some #(and (= :error (:severity %)) (= "src/view/page.clj" (:file %)))
                      (:after diagnostics)) (pr-str diagnostics))))))))

(deftest compares-callable-domains-and-ranges-directionally
  (let [signature (fn [param ret]
                    (str "(s/fdef f :args (s/cat :x " param ") :ret " ret ")"))]
    (is (empty? (compare-api (signature "int?" "number?") (signature "number?" "int?"))))
    (is (empty? (compare-api (signature "int?" "any?") (signature "any?" "int?"))))
    (is (= [[:breaking :arguments]]
           (statuses (compare-api (signature "number?" "int?") (signature "int?" "int?")))))
    (is (= [[:breaking :return]]
           (statuses (compare-api (signature "int?" "int?") (signature "int?" "any?")))))))

(deftest compares-repeat-minima-and-covers-a-variadic-domain-with-fixed-arities
  (let [zero "(s/fdef f :args (s/cat :xs (s/* int?)) :ret int?)"
        one "(s/fdef f :args (s/cat :xs (s/+ int?)) :ret int?)"
        split "(s/fdef f :args (s/cat :x (s/? int?) :xs (s/+ int?)) :ret int?)"
        equivalent "(defn f {:t/sig '(t/fn ([:-> int?] [int? & int? :-> int?]))}
                        ([] 0) ([x & xs] x))"
        changes (compare-api zero one)]
    (is (= [[:breaking :arity]] (statuses changes)))
    (is (= 0 (:arity (first changes))))
    (is (empty? (compare-api one zero)))
    (is (empty? (compare-api zero equivalent)))
    (is (empty? (compare-api one split)))))

(deftest compares-optional-map-fields-as-constraints-on-open-maps
  (let [signature (fn [input output]
                    (str "(s/def ::id string?)
(s/fdef f :args (s/cat :x " input ") :ret " output ")"))
        open "(s/keys)"
        constrained "(s/keys :opt-un [::id])"]
    (is (= [[:breaking :arguments]]
           (statuses (compare-api (signature open open) (signature constrained open)))))
    (is (= [[:breaking :return]]
           (statuses (compare-api (signature open constrained) (signature open open)))))
    (is (empty? (compare-api (signature constrained open) (signature open constrained))))))

(deftest an-opaque-sibling-does-not-hide-an-incompatible-field
  (let [signature (fn [field]
                    (str "(s/def ::count pos-int?)
(s/def ::id " field ")
(s/def ::result (s/keys :req-un [::count ::id]))
(s/fdef f :args (s/cat) :ret ::result)"))]
    (is (= [[:inconclusive :return]]
           (statuses (compare-api (signature "string?") (signature "string?")))))
    (is (= [[:breaking :return]]
           (statuses (compare-api (signature "string?") (signature "int?")))))))

(deftest reports-opaque-specs-and-overlaps-without-a-proven-break
  (let [known "(s/fdef f :args (s/cat :x int?) :ret int?)"
        refined "(s/fdef f :args (s/cat :x (s/and int? pos?)) :ret int?)"
        named "(s/fdef f :args ::not-statically-resolved :ret int?)"
        single-union "(s/fdef f :args (s/cat :x (s/or :int int? :str string?)) :ret int?)"
        split-union "(s/fdef f :args (s/alt :int int? :str string?) :ret int?)"]
    (is (= [[:inconclusive :arguments]] (statuses (compare-api known refined))))
    (is (= [:unchecked-spec] (:reasons (first (compare-api known refined)))))
    (is (= [[:inconclusive :signature]] (statuses (compare-api known named))))
    (is (= "src/api.clj" (get-in (first (compare-api known named)) [:after :declaration :file])))
    (is (= [[:inconclusive :overload]] (statuses (compare-api single-union split-union))))
    (is (empty? (compare-api split-union single-union)))))

(deftest cannot-claim-a-break-when-structural-variants-may-cover-a-union
  (let [specs "(s/def :a/tag #{:a})
(s/def :b/tag #{:b})
(s/def ::tag #{:a :b})"
        old (str specs "(s/fdef f :args (s/cat) :ret
                          (s/or :a (s/keys :req-un [:a/tag]) :b (s/keys :req-un [:b/tag])))")
        new (str specs "(s/fdef f :args (s/cat) :ret (s/keys :req-un [::tag]))")
        issue (first (compare-api old new))]
    (is (= [:inconclusive :return [:structural-union]]
           ((juxt :status :kind :reasons) issue)))))

(deftest distinguishes-removing-a-public-var-from-dropping-its-contract
  (let [old "(s/fdef f :args (s/cat :x int?) :ret int?) (defn f [x] x)"]
    (is (= [[:breaking :removed-var]] (statuses (compare-api old "(defn- f [x] x)"))))
    (is (= [[:breaking :removed-var]] (statuses (compare-api old ""))))
    (is (= [[:inconclusive :signature]] (statuses (compare-api old "(defn f [x] x)"))))))

(deftest compares-conform-tags-and-tracks-direct-clients
  (let [revision (fn [branches]
                   {"schema.clj" (str "(ns schema (:require [clojure.spec.alpha :as s]))
(s/def ::value (s/or " branches "))")
                    "api.clj" "(ns api (:require [schema :as schema] [clojure.spec.alpha :as s]))
(s/fdef accept :args (s/cat :x ::schema/value) :ret any?)"
                    "client.clj" "(ns client (:require [schema :as schema] [clojure.spec.alpha :as spec]))
(defn display [x]
  (let [result (spec/conform ::schema/value x)]
    (when-not (spec/invalid? result)
      (let [[tag value] result]
        (case tag :text (str value))))))"})
        base (revision ":text string?")]
    (doseq [[branches added removed] [[":label string?" [":label"] [":text"]]
                                      [":text string? :count int?" [":count"] []]]]
      (compare-fixture base (revision branches)
        (fn [{:keys [changes diagnostics]} _ _]
          (let [issue (first changes)]
            (is (= [[:breaking :conform-tags]] (statuses changes)) (pr-str changes))
            (is (= [":schema/value" added removed ["api/accept"]]
                   ((juxt :spec :added-tags :removed-tags :affected-vars) issue)))
            (is (= [":schema/value"] (get-in issue [:spec-changes 0 :path])))
            (is (= ["src/client.clj" 3 :conform]
                   ((juxt :file (comp :row :pos) :kind) (first (:uses issue)))))
            (is (some #(= "TSP8001" (:code %)) (:after diagnostics)))))))))

(deftest cli-emits-json-and-distinct-exit-statuses
  (let [baseline {"api.clj" (str api-ns "(s/fdef f :args (s/cat :xs (s/* int?)) :ret int?)")}]
    (compare-fixture baseline
                     {"api.clj" (str api-ns "(s/fdef f :args (s/cat :xs (s/+ int?)) :ret int?)")}
      (fn [_ base head]
        (let [java (str (io/file (System/getProperty "java.home") "bin" "java"))
              run (fn [base head]
                    (shell/sh java "-cp" (System/getProperty "java.class.path")
                              "clojure.main" "-m" "typespec.main"
                              "contracts-diff" (str base) "--root" (str head) "--json"))]
          (testing "proven breaks fail CI"
            (let [{:keys [exit out err]} (run base head)]
              (is (= 1 exit) err)
              (is (= "breaking" (get-in (json/read-str out :key-fn keyword) [:changes 0 :status])))))
          (testing "compatible revisions pass"
            (let [{:keys [exit out err]} (run head base)]
              (is (zero? exit) err)
              (is (= [] (:changes (json/read-str out :key-fn keyword)))))))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no source files for current"
                              (main/run-contract-diff (str base) (str (io/file head "missing")) nil {})))))))
