(ns typespec.type-operators-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [typespec.check :as check]
            [typespec.externs :as externs]
            [typespec.parse :as parse]
            [typespec.type-operators :as operators]
            [typespec.types :as t])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(def ^:private header "(ns app (:require [clojure.spec.alpha :as s] [vendor :as v]))\n")
(def ^:private specs
  "(s/def ::id int?)
   (s/def ::name string?)
   (s/def ::note (s/nilable string?))
   (s/def ::email string?)
   (s/def ::profile (s/keys :req-un [::email]))
   (s/def ::person (s/keys :req-un [::id ::name ::profile] :opt-un [::note]))\n")

(defn- check-source
  ([body] (check-source body {}))
  ([body contracts]
   (check/check-files [(parse/parse-string "app.clj" (str header body "\n" specs))]
                      {:strict-nil true :extern-contracts contracts})))

(defn- codes [res] (mapv :code (:diagnostics res)))
(defn- arity [res sym] (get-in res [:env :vars sym :type :arities 0]))

(deftest projections-check-real-bodies-and-forward-specs
  (let [res (check-source
             "(defn brief {:t/sig '[::person :-> (t/pick ::person :id :note :id)]}
                [person] (select-keys person [:id :note]))
              (defn pass-through {:t/sig '[::person :-> (t/omit ::person :id)]}
                [person] person)
              (defn empty-view {:t/sig '[::person :-> (t/pick ::person)]}
                [person] {})
              (defn unchanged {:t/sig '[::person :-> (t/omit ::person)]}
                [person] person)")
        brief (:ret (arity res 'app/brief))
        omit (:ret (arity res 'app/pass-through))]
    (is (empty? (codes res)) (pr-str (:diagnostics res)))
    (is (= {:id (t/ref-t :app/id)} (:req brief)))
    (is (= {:note (t/ref-t :app/note)} (:opt brief)))
    (is (= #{:name :profile} (set (keys (:req omit)))))
    (is (= {:t :map :req {} :opt {}} (:ret (arity res 'app/empty-view))))
    (is (= (get-in res [:env :specs :app/person :type]) (:ret (arity res 'app/unchanged))))))

(deftest partial-is-shallow-and-does-not-add-nil-to-values
  (let [res (check-source
             "(defn patch {:t/sig '[(t/partial (t/partial (t/omit ::person :id))) :-> boolean?]}
                [x] true)
              (patch {})
              (patch {:name \"New name\"})
              (patch {:note nil})
              (patch {:profile {:email \"a@b\"}})
              (patch {:name nil})
              (patch {:profile {}})
              (defn accept-view {:t/sig '[(t/omit ::person :id) :-> boolean?]} [x] true)
              (accept-view {:name \"A\"})")
        p (first (:params (arity res 'app/patch)))]
    (is (= ["TSP2322" "TSP2322" "TSP2741"] (codes res)) (pr-str (:diagnostics res)))
    (is (= {} (:req p)))
    (is (= {:name (t/ref-t :app/name) :profile (t/ref-t :app/profile) :note (t/ref-t :app/note)} (:opt p)))))

(deftest key-operators-use-the-declared-shape-and-reading-semantics
  (let [res (check-source
             "(defn key? {:t/sig '[(t/keys-of ::person) :-> boolean?]} [k] true)
              (key? :id)
              (key? :note)
              (key? :app/id)
              (defn none {:t/sig '[(t/keys-of (t/pick ::person)) :-> boolean?]} [k] true)
              (none :id)
              (defn title {:t/sig '[::person :-> (t/get ::person :name)]} [p] (:name p))
              (defn maybe-title {:t/sig '[(t/partial ::person) :-> (t/get (t/partial ::person) :name)]}
                [p] (:name p))
              (defn note {:t/sig '[::person :-> (t/get ::person :note)]} [p] (:note p))")
        env (:env res)]
    (is (= ["TSP2345" "TSP2345"] (codes res)) (pr-str (:diagnostics res)))
    (is (= #{:id :name :note :profile} (set (map :val (t/members env (first (:params (arity res 'app/key?))))))))
    (is (= t/never (first (:params (arity res 'app/none)))))
    (is (= (t/ref-t :app/name) (:ret (arity res 'app/title))))
    (doseq [sym ['app/maybe-title 'app/note]]
      (is (= #{(t/prim :string) t/nil-t} (set (t/members env (:ret (arity res sym)))))))))

(deftest invalid-operator-forms-report-the-operator-position
  (doseq [[form code message]
          [["(t/partial)" "TSP270-01" "one map type"]
           ["(t/partial ::person :name)" "TSP270-01" "one map type"]
           ["(t/get ::person)" "TSP270-01" "one keyword literal"]
           ["(t/pick ::person k)" "TSP270-01" "keyword literals"]
           ["(t/get ::person :app/id)" "TSP270-02" "declared keys:"]
           ["(t/omit ::person :typo)" "TSP270-02" ":typo"]
           ["(t/pick ::person :app/email)" "TSP270-02" ":app/email"]]]
    (let [res (check-source (str "(defn verify {:t/sig '[\n    " form "\n    :-> boolean?]} [x] true)"))
          [d] (:diagnostics res)]
      (is (= [code] (codes res)) (pr-str (:diagnostics res)))
      (is (= [3 5] ((juxt :row :col) (:pos d))))
      (is (str/includes? (:message d) message)))))

(deftest resolver-rejects-unsupported-shapes-and-cycles
  (doseq [[input message]
          [["::missing" "undefined map type"]
           ["::loop-a" "cyclic map reference"]
           ["(s/nilable ::person)" "s/nilable"]
           ["(t/or ::person ::person)" "t/or"]
           ["(s/map-of keyword? string?)" "single s/keys shape"]
           ["(s/merge ::person ::person)" "single s/keys shape"]
           ["(t/and ::person ::person)" "t/and"]
           ["(t/exact (t/omit ::person :id))" "t/exact"]
           ["::wrapped" "s/and"]
           ["(s/keys :req-un [::id] :opt-un [::id])" "conflicting declarations"]
           ["(s/keys :req-un [::id :other/id])" "conflicting declarations"]]]
    (let [res (check-source (str "(s/def ::loop-a ::loop-b) (s/def ::loop-b ::loop-a)
                                 (s/def ::wrapped (s/and ::person map?))
                                 (defn verify {:t/sig '[(t/partial " input ") :-> boolean?]} [x] true)"))]
      (is (= ["TSP270-03"] (codes res)) (pr-str (:diagnostics res)))
      (is (str/includes? (get-in res [:diagnostics 0 :message]) message))))
  (let [r (check-source "(defn poly {:t/sig '(t/all [a] [(t/get a :id) :-> boolean?])} [x] true)")]
    (is (= ["TSP270-03"] (codes r)) (pr-str (:diagnostics r))))
  (let [names (mapv #(str ":app/link-" %) (range 260))
        chain (str/join "\n" (map #(str "(s/def " %1 " " %2 ")") names (concat (rest names) ["::person"])))
        res (check-source (str chain "\n(defn verify {:t/sig '[(t/partial " (first names) ") :-> boolean?]} [x] true)"))]
    (is (= ["TSP270-03"] (codes res)) (pr-str (:diagnostics res)))
    (is (str/includes? (get-in res [:diagnostics 0 :message]) "resolution limit"))))

(deftest operator-results-print-and-round-trip
  (let [res (check-source
             "(s/def ::child ::node)
              (s/def ::node (s/keys :req-un [::id] :opt-un [::child]))
              (defn node-view {:t/sig '[(t/partial ::node) :-> (t/get (t/pick ::node :child) :child)]}
                [x] (:child x))")
        ty (get-in res [:env :vars 'app/node-view :type])
        errors (atom [])
        lowered (operators/normalize (:env res)
                    (t/parse-type {:fail #(swap! errors conj %)
                                   :operator-error #(swap! errors conj [%1 %2 %3])}
                                  (read-string (t/show ty)))
                    #(swap! errors conj [%1 %2 %3]))]
    (is (empty? (codes res)) (pr-str (:diagnostics res)))
    (is (empty? @errors))
    (is (t/alpha-equivalent? ty lowered))
    (is (= (t/show ty) (t/show lowered)))))

(deftest cross-namespace-projections-agree-with-spec
  (let [client (parse/parse-string
                "client.clj"
                "(ns client (:require [clojure.spec.alpha :as s] [data :as d]))
                 (s/fdef name-of :args (s/cat :x (s/keys :req-un [::d/name])) :ret ::d/name)
                 (defn name-of {:t/sig '[(t/pick ::d/person :name) :-> (t/get ::d/person :name)]}
                   [x] (:name x))")
        data (parse/parse-string
              "data.clj"
              "(ns data (:require [clojure.spec.alpha :as s]))
               (s/def ::name string?) (s/def ::id int?)
               (s/def ::person (s/keys :req-un [::name ::id]))")
        res (check/check-files [client data] {})]
    (is (empty? (codes res)) (pr-str (:diagnostics res)))
    (is (= {:name (t/ref-t :data/name)} (get-in (arity res 'client/name-of) [:params 0 :req])))))

(deftest invalid-inner-operators-do-not-create-outer-errors
  (let [res (check-source "(defn verify {:t/sig '[(t/partial (t/omit ::person :absent)) :-> boolean?]}
                             [x] true)")]
    (is (= ["TSP270-02"] (codes res)) (pr-str (:diagnostics res)))
    (is (false? (get-in res [:env :vars 'app/verify :complete?]))))
  (let [res (check-source "(s/def ::bad (t/partial ::person))")]
    (is (= ["TSP9002"] (codes res)) (pr-str (:diagnostics res)))))

(deftest externs-share-the-source-spec-environment
  (let [root (.toFile (Files/createTempDirectory "typespec-operators" (make-array FileAttribute 0)))
        file (io/file root "types.edn")]
    (try
      (spit file "{:vendor/patch (t/partial :app/person)
                    :vendor/view (t/pick :vendor/patch :name)
                    vendor/save [:vendor/patch :-> (t/get :app/person :name)]
                    vendor/broken [(t/get :app/person :nope) :-> boolean?]
                    vendor/malformed [(t/get :app/person) :-> boolean?]}")
      (let [contracts (externs/load-contracts root {:externs ["types.edn"]})
            res (check-source
                 "(defn maybe-name {:t/sig '[(t/partial :vendor/view) :-> (t/get :vendor/view :name)]}
                    [patch] (:name patch))
                  (v/save {})
                  (v/save {:name nil})" contracts)
            operator-error (first (filter #(= "TSP270-02" (:code %)) (:diagnostics res)))]
        (is (= #{"TSP270-01" "TSP270-02" "TSP2322"} (set (codes res))) (pr-str (:diagnostics res)))
        (is (= (t/ref-t :app/name) (:ret (arity res 'vendor/save))))
        (is (not-any? (get-in res [:env :vars]) ['vendor/broken 'vendor/malformed]))
        (is (= (str file) (:file operator-error))))
      (finally (.delete file) (.delete root)))))
