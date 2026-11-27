(ns typespec.spec-predicate-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is]]
            [typespec.check :as check]
            [typespec.parse :as parse]
            [typespec.spec-conform :as conform]
            [typespec.types :as t]))

(s/def ::http-method #(contains? #{:get :post :put :delete :patch :head} %))
(s/def ::middleware #(or (list? %) (vector? %)))

(def ^:private api
  (parse/parse-string "api.clj"
                      "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::method #(contains? #{:get :post :put :delete :patch :head} %))
(s/def ::middleware #(or (list? %) (vector? %)))
(s/def ::maybe #(clojure.core/contains? #{nil false :ok} %1))
(s/def ::property (s/or :method ::method :middleware ::middleware))
(s/fdef method :args (s/cat :x ::method) :ret any?)
(defn method [x] x)
(s/fdef middleware :args (s/cat :x ::middleware) :ret any?)
(defn middleware [x] x)"))

(deftest spec-predicate-types-agree-with-runtime-on-known-values
  (let [cases (concat
               (for [value [:get :post :put :delete :patch :head :trace "get" nil false]]
                 ["method" (pr-str value) (s/valid? ::http-method value)])
               (for [[form value] [["()" '()]
                                   ["'(:auth)" '(:auth)]
                                   ["(list :auth)" (list :auth)]
                                   ["[]" []]
                                   ["[:auth]" [:auth]]
                                   ["{:a 1}" {:a 1}]
                                   ["#{:auth}" #{:auth}]
                                   ["nil" nil]
                                   ["(clojure.lang.Cons. :auth '(:more))" (cons :auth '(:more))]]]
                 ["middleware" form (s/valid? ::middleware value)]))
        client (parse/parse-string "client.clj"
                                   (str "(ns client (:require [api :as api]))\n"
                                        (apply str (for [[method form] cases]
                                                     (str "(api/" method " " form ")\n")))))
        issues (:diagnostics (check/check-files [client api] {}))]
    (is (= (keep-indexed (fn [i [_ _ valid?]] (when-not valid? [(+ i 2) "TSP2345"])) cases)
           (mapv (juxt (comp :row :pos) :code) issues)) (pr-str issues))
    (is (every? #(= "client.clj" (:file %)) issues))))

(deftest preserves-literal-sets-and-conformed-payload-types
  (let [env (:env (check/check-files [api] {}))
        maybe (t/ref-t :api/maybe)
        conform-type (conform/tagged-result env nil :api/property)
        methods (some (fn [branch]
                        (when (= :method (get-in branch [:items 0 :val]))
                          (get-in branch [:items 1])))
                      (t/members env conform-type))]
    (is (every? #(t/sub? env % maybe) [t/nil-t (t/lit false) (t/lit :ok)]))
    (is (= :mismatch (:status (t/sub-result env (t/lit true) maybe))))
    (is (= :proved (:status (t/sub-result env (t/lit :get) methods))))
    (is (= :mismatch (:status (t/sub-result env (t/lit :trace) methods))))))

(deftest leaves-unresolved-and-shadowed-predicates-opaque
  (let [source (parse/parse-string "schema.clj"
                                   "(ns schema (:require [clojure.spec.alpha :as s]))
(s/def ::local #(contains? #{:ok} %))
(s/def ::dynamic-set #(clojure.core/contains? #{status} %))
(s/def ::other-argument #(clojure.core/contains? #{:ok} %2))
(s/def ::parameter-call (fn [vector?] (vector? vector?)))
(s/def ::mixed #(or (list? %) (custom-predicate %)))
(s/def ::not-the-argument (fn [x] (instance? String value)))
(s/def ::protocol #(vector? %))
(s/def ::hinted (fn [^long x] (clojure.core/contains? #{:ok} x)))
(s/def ::qualified (clojure.core/fn [x] (clojure.core/contains? #{:ok} x)))")
        defs (parse/parse-string "predicates.clj"
                                 "(ns schema)
(defn contains? [_ _] true)
(defprotocol Custom (vector? [x]))")
        excluded (parse/parse-string "excluded.clj"
                                     "(ns excluded (:refer-clojure :exclude [contains?])
                                         (:require [clojure.spec.alpha :as s]))
(s/def ::value #(contains? #{:ok} %))")
        result (check/check-files [source excluded defs] {})
        issues (:diagnostics result)]
    (is (= (into [["excluded.clj" 3 "TSP9002"]]
                 (map #(vector "schema.clj" % "TSP9002") (range 2 10)))
           (mapv (juxt :file (comp :row :pos) :code) issues)) (pr-str issues))
    (is (= #{:ok} (into #{} (map :val) (t/members (:env result) (t/ref-t :schema/qualified)))))))

(deftest spec-predicates-resolve-through-namespace-mappings
  (let [custom (parse/parse-string "custom.clj"
                                  "(ns custom (:refer-clojure :exclude [string?]))
(defn string? [x] (keyword? x))")
        api (parse/parse-string "api.clj"
                               "(ns api (:refer-clojure :rename {string? text?})
  (:require [clojure.spec.alpha :as sp] [custom :refer [string?]]))
(sp/def ::category string?)
(sp/def ::title text?)
(sp/def ::summary #(text? %))
(sp/def ::entry (sp/keys :req-un [::title]))
(sp/fdef category :args (sp/cat :x ::category) :ret any?)
(sp/fdef entry :args (sp/cat :x ::entry :summary ::summary) :ret any?)
(defn category [x] x)
(defn entry [x summary] x)
(defn explicit {:t/sig '[string? :-> string?]} [x] x)
(sp/def ::tagged (sp/or :text text? :other string?))")
        client (parse/parse-string "client.clj"
                                  "(ns client (:require [api :as a]))
(a/category :valid)
(a/entry {:title \"valid\"} \"summary\")
(a/entry {:title 42} \"summary\")
(a/entry {:title \"valid\"} 42)
(a/explicit \"valid\")
(a/explicit :wrong)")
        {:keys [diagnostics env]} (check/check-files [client api custom] {})
        output (conform/tagged-result env nil :api/tagged)
        payloads (into {} (keep (fn [part]
                                 (when-let [[tag payload] (:items part)]
                                   [(:val tag) payload]))) (t/members env output))]
    (is (= [["api.clj" 3 "TSP9002"] ["api.clj" 12 "TSP9002"]
            ["client.clj" 4 "TSP2322"] ["client.clj" 5 "TSP2345"] ["client.clj" 7 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics))
    (is (= {:text (t/prim :string) :other t/unknown} payloads))))

(deftest an-unknown-qualified-key-write-remains-inconclusive
  (let [client (parse/parse-string "client.clj"
                                   "(ns client (:require [api]))
(defn- store [method] {:api/method method})
(defn- bad [] {:api/method :trace})")
        issues (fn [opts] (:diagnostics (check/check-files [client api] opts)))
        default (issues {})]
    (is (= [[2 "TSP210-01" :warning] [3 "TSP2322" :error]]
           (mapv (juxt (comp :row :pos) :code :severity) default)) (pr-str default))
    (is (= #{:unknown} (:reasons (first default))))
    (is (= ["TSP210-01" :error] ((juxt :code :severity) (first (issues {:strict true})))))))
