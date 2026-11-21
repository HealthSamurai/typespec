(ns typespec.recursive-types-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [typespec.check :as check]
            [typespec.env :as env]
            [typespec.main :as main]
            [typespec.parse :as parse]
            [typespec.recursive-types :as recursive]
            [typespec.types :as t]))

(def ^:private schema
  "(ns schema (:require [clojure.spec.alpha :as s]))
(s/def ::node (s/keys :req-un [::value ::children]))
(s/def ::value int?)
(s/def ::children (s/coll-of ::node :kind vector?))")

(def ^:private api
  "(ns api (:require [clojure.spec.alpha :as s] [schema :as schema]))
(s/fdef consume :args (s/cat :node ::schema/node) :ret ::schema/node)
(defn consume [node] node)")

(defn- check-project [schema-source api-source client-source]
  (check/check-files [(parse/parse-string "client.clj" client-source)
                      (parse/parse-string "api.clj" api-source)
                      (parse/parse-string "schema.clj" schema-source)] {}))

(defn- tree [value depth]
  (nth (iterate (fn [node] {:value 1 :children [node]})
                {:value value :children []})
       depth))

(deftest checks-deep-spec-data-across-namespaces
  (doseq [depth [1 20 100]]
    (let [form (str "(api/consume " (pr-str (tree "bad" depth)) ")")
          source (str "(ns client (:require [api :as api]))\n"
                      "(api/consume " (pr-str (tree 1 depth)) ")\n" form)
          result (check-project schema api source)
          diagnostics (:diagnostics result)
          diag (first diagnostics)
          path (conj (vec (mapcat identity (repeat depth [:children 0]))) :value)]
      (is (= ["TSP2322"] (mapv :code diagnostics)) (pr-str diagnostics))
      (is (= ["client.clj" 3 (inc (str/index-of form "\"bad\""))]
             ((juxt :file (comp :row :pos) (comp :col :pos)) diag)))
      (is (= path (:type-path diag)))
      (is (= (mapv #(if (keyword? %) (str %) %) path) (:type-path (main/diag->json diag)))))))

(deftest reports-nested-return-and-missing-field-positions
  (let [producer (str api "
(s/fdef produce :args (s/cat) :ret ::schema/node)
(defn produce [] " (pr-str (tree :wrong 21)) ")")
        client "(ns client (:require [schema :as schema] [api :as api]))
(api/produce)
(let [value {:value 1 :children [{}]}] (api/consume value))"
        result (check-project schema producer client)
        [ret missing :as ds] (:diagnostics result)]
    (is (= [["api.clj" "TSP2322"] ["client.clj" "TSP2741"]] (mapv (juxt :file :code) ds)) (pr-str ds))
    (is (= :value (peek (:type-path ret))))
    (is (= [:children 0 :children] (:type-path missing)))))

(defn- schema-environment [source]
  (env/build [(parse/parse-string "schema.clj" (str "(ns schema (:require [clojure.spec.alpha :as s]))\n" source))]))

(deftest structurally-compares-different-recursive-bindings
  (let [[env diagnostics]
        (schema-environment
         "(s/def :a/value int?)
          (s/def :a/next (s/coll-of ::a :kind vector?))
          (s/def ::a (s/keys :req-un [:a/value :a/next]))
          (s/def :b/value number?)
          (s/def :b/next (s/coll-of ::b :kind vector?))
          (s/def ::b (s/keys :req-un [:b/value :b/next]))
          (s/def :c/value string?)
          (s/def :c/next (s/coll-of ::c :kind vector?))
          (s/def ::c (s/keys :req-un [:c/value :c/next]))")
        a (t/ref-t :schema/a)
        b (t/ref-t :schema/b)
        c (t/ref-t :schema/c)]
    (is (empty? diagnostics))
    (is (= :proved (:status (t/sub-result env a b))))
    (is (= [:mismatch [:value]] ((juxt :status :path) (t/sub-result env b a))))
    (is (= [:mismatch [:value]] ((juxt :status :path) (t/sub-result env a c))))
    (doseq [target [(t/union c b) (t/union b c)]]
      (is (= :proved (:status (t/sub-result env a target)))))
    (is (= {:status :inconclusive :reasons #{:recursive-budget}}
           (binding [recursive/*max-obligations* 3] (t/sub-result env a b))))
    (is (= :proved (:status (t/sub-result env a b))))))

(deftest mutual-recursion-through-optional-and-nilable-branches
  (let [schema "(ns schema (:require [clojure.spec.alpha :as s]))
(s/def ::item (s/keys :req-un [::label] :opt-un [::child]))
(s/def ::menu (s/keys :req-un [::item]))
(s/def ::label string?)
(s/def ::child (s/nilable ::menu))"
        api "(ns api (:require [clojure.spec.alpha :as s] [schema :as schema]))
(s/fdef execute :args (s/cat :menu ::schema/menu) :ret string?)
(defn execute [menu] (get-in menu [:item :label]))"
        client "(ns client (:require [api :as api]))
(api/execute {:item {:label \"Open\"}})
(api/execute {:item {:label \"Open\" :child nil}})
(api/execute {:item {:label \"Open\" :child {:item {:label \"Close\"}}}})
(api/execute {:item {:label \"Open\" :child {:item {:label 7}}}})"
        ds (:diagnostics (check-project schema api client))]
    (is (= [["client.clj" "TSP2322" [:item :child :item :label]]]
           (mapv (juxt :file :code :type-path) ds)) (pr-str ds))
    (let [without-nil (str/replace schema "(s/nilable ::menu)" "::menu")
          ds (:diagnostics (check-project without-nil api client))]
      (is (= [[3 [:item :child]] [5 [:item :child :item :label]]]
             (mapv (juxt (comp :row :pos) :type-path) ds)) (pr-str ds)))))

(deftest diagnoses-unguarded-cycles-and-distinguishes-missing-references
  (let [a (parse/parse-string "a.clj" "(ns a (:require [clojure.spec.alpha :as s])) (s/def ::a :b/b)")
        b (parse/parse-string "b.clj" "(ns b (:require [clojure.spec.alpha :as s]))
(s/def ::b :a/a)
(s/def ::missing ::not-defined)
(s/def ::children (s/coll-of ::node :kind vector?))
(s/def ::node (s/keys :req-un [::children] :opt-un [::missing]))")
        ds (:diagnostics (check/check-files [a b] {}))]
    (is (= [["a.clj" "TSP295-01"] ["b.clj" "TSP9003"]]
           (mapv (juxt :file :code) ds)) (pr-str ds))
    (is (str/includes? (:message (first ds)) ":a/a -> :b/b -> :a/a"))
    (is (= ds (:diagnostics (check/check-files [b a] {})))))
  (let [a (t/ref-t :schema/nullable)
        direct-env {:specs {:schema/nullable {:type (t/union a t/nil-t)}}}
        [env ds] (schema-environment "(s/def ::nullable (s/nilable ::nullable))")]
    (is (= ["TSP295-01"] (mapv :code ds)))
    (is (= t/unknown (t/remove-nil direct-env a)))
    (is (= {:status :inconclusive :reasons #{:unguarded-recursion}}
           (t/sub-result env a a)))))

(deftest required-recursion-without-a-finite-base-is-incomplete
  (let [[env ds] (schema-environment "(s/def ::next (s/keys :req-un [::next]))")
        a (t/ref-t :schema/next)]
    (is (= [[:warning "TSP295-02"]] (mapv (juxt :severity :code) ds)))
    (is (= {:status :inconclusive :reasons #{:unproductive-recursion}} (t/sub-result env a a)))))

(deftest a-bare-vector-does-not-prove-recursive-element-types
  (let [api "(ns api (:require [clojure.spec.alpha :as s] [schema :as schema]))
(s/fdef make-node :args (s/cat :children vector?) :ret ::schema/node)
(defn make-node [children] {:value 1 :children children})"
        ds (:diagnostics (check-project schema api "(ns client)"))]
    (is (= [["api.clj" "TSP210-01" #{:unsupported}]]
           (mapv (juxt :file :code :reasons) ds)) (pr-str ds))))

(deftest satisfies-checks-a-regular-recursive-spec
  (let [source (str schema "\n#_(t/satisfies ::node) "
                    (pr-str (tree 1 24)) "\n#_(t/satisfies ::node) "
                    (pr-str (tree "bad" 24)))
        ds (:diagnostics (check/check-files [(parse/parse-string "schema.clj" source)] {:strict-nil true}))]
    (is (= ["TSP2322"] (mapv :code ds)) (pr-str ds))
    (is (= :value (peek (:type-path (first ds)))))))
