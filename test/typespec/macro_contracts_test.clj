(ns typespec.macro-contracts-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [typespec.check :as check]
            [typespec.main :as main]
            [typespec.parse :as parse]))

(def contract '{:shape :binding-body :bindings [:app/context map?] :ret (t/or map? nil?)})
(def contracts {'system/defstart contract
                'system/defstop (assoc contract :ret 'any?)})
(def prelude
  "(ns app (:require [system :as sys :refer [defstart]] [clojure.spec.alpha :as s]))
   (s/def :app/db string?)
   (s/def :app/context (s/keys :req-un [:app/db]))
   (defn query {:t/sig '[:app/context string? :-> map?]} [ctx sql] {})")
(defn checked
  ([body] (checked body contracts))
  ([body cs]
   (check/check-files [(parse/parse-string "app.clj" (str prelude body))]
                      {:macro-contracts cs})))
(defn codes [r] (set (map :code (:diagnostics r))))

(deftest potemkin-imports-preserve-cross-namespace-contracts-and-macro-status
  (let [origin (parse/parse-string "origin.clj" "(ns origin (:require [clojure.spec.alpha :as s]))
(s/fdef size :args (s/cat :text string?) :ret int?)
(defn size [text] (count text))
(defmacro twice [x] x)")
        facade (parse/parse-string "facade.cljc" "(ns facade (:require [origin :as impl] [potemkin :as p]))
#?(:clj (p/import-vars [impl size twice]))
(do (p/import-vars [impl :refer [size] :rename {size renamed}]))
(inc (size \"local\"))
(size 0)")
        bridge (parse/parse-string "bridge.clj" "(ns bridge (:require [facade :as f] [potemkin.namespaces :refer [import-vars]]))
(import-vars f/renamed)")
        client (parse/parse-string "client.clj" "(ns client (:require [facade :as f] [bridge :as b]))
(inc (f/size \"text\"))
(inc (b/renamed \"text\"))
(f/size 42)
(b/renamed 42)
(f/twice 1)
(f/typo \"text\")")
        result (check/check-files [client bridge facade origin] {})]
    (is (= [["client.clj" 4 "TSP2345"] ["client.clj" 5 "TSP2345"]
            ["client.clj" 6 "TSP7031"] ["client.clj" 7 "TSP2305"]
            ["facade.cljc" 5 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))))

(deftest potemkin-import-reports-a-missing-source-at-the-import
  (let [source (parse/parse-string "source.clj" "(ns example.source)
(def known 1)")
        facade (parse/parse-string "facade.clj" "(ns example.facade (:require [example.source :as src] [potemkin :as p]))
(p/import-vars [src missing] [external.library supplied])")
        client (parse/parse-string "client.clj" "(ns example.client (:require [example.facade :as f]))
(f/supplied 1)")
        diagnostics (:diagnostics (check/check-files [facade source client] {}))]
    (is (= [["facade.clj" 2 21 "TSP2305"]]
           (mapv (juxt :file (comp :row :pos) (comp :col :pos) :code) diagnostics))
        (pr-str diagnostics))))

(deftest malli-declaration-contracts-check-bodies-and-index-functions
  (let [api (parse/parse-string
             "api.clj"
             "(ns api (:refer-clojure :exclude [defn-])
  (:require [schema.declarations :as m :refer [defn-]] [clojure.spec.alpha :as s]))
(s/def ::code int?)
(s/fdef code :args (s/cat :m (s/keys :req-un [::code])) :ret int?)
(m/defn code :- :int
  \"Read a code.\"
  {:private false}
  [{:keys [code]} :- [:map [:code :int]]]
  (inc code))
(defn- label
  ([^String x :- :string] (.toUpperCase x))
  ([[^String x :- :string :as pair :- [:tuple :string]] & more :- [:* :string]]
    (.toUpperCase x)))
(m/defn wrong [^String x :- external.schema/name] (inc x))
(m/defn unsupported [x :-] (inc \"hidden\"))")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [api :as a]))
(inc (a/code {:code 42}))
(a/code {:code \"bad\"})
(a/code)")
        config '{schema.declarations/defn {:shape :malli-defn}
                 schema.declarations/defn- {:shape :malli-defn :private? true}}
        result (check/check-files [client api] {:macro-contracts config})
        ds (:diagnostics result)]
    (is (= [["api.clj" 14 "TSP2345"] ["api.clj" 15 "TSP7031"]
            ["client.clj" 3 "TSP2322"] ["client.clj" 4 "TSP2554"]]
           (mapv (juxt :file (comp :row :pos) :code) ds))
        (pr-str ds))
    (is (true? (get-in result [:env :vars 'api/label :private?])))
    (is (nil? (find-ns 'schema.declarations)))))

(deftest qualified-core-defn-keeps-its-rule-with-a-local-malli-defn
  (let [source (parse/parse-string
                "source.clj"
                "(ns fixture.declarations (:refer-clojure :exclude [defn])
  (:require [clojure.core :as core]))
(defmacro defn [& forms] forms)
(core/defn ordinary [] (inc \"bad\"))
(fixture.declarations/defn annotated [] (inc \"bad\"))")
        ds (:diagnostics
            (check/check-files [source]
                               {:macro-contracts '{fixture.declarations/defn {:shape :malli-defn}}}))]
    (is (= [[4 "TSP2345"] [5 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) ds))
        (pr-str ds))))

(deftest core-wrapper-bodies-use-their-original-positions
  (let [r (checked "\n(time (inc \"bad\"))
(clojure.core/future (inc \"bad\") :done)
(lazy-seq (inc \"bad\") nil)
(lazy-seq 42)
(time)")
        ds (:diagnostics r)]
    (is (= [[5 12 "TSP2345"] [6 27 "TSP2345"] [7 16 "TSP2345"]
            [8 11 "TSP2345"] [9 1 "TSP2554"]]
           (mapv (juxt (comp :row :pos) (comp :col :pos) :code) ds))
        (pr-str ds))))

(deftest core-wrapper-types-reflect-when-their-bodies-run
  (let [r (checked "\n(time :done)
(future :done)
(future (throw (Exception.)))
(lazy-seq (map inc [1 2]))
(lazy-seq nil)
(lazy-seq (throw (Exception.)))
(time (throw (Exception.)))
(let [time (fn [x y] y)] (time 1 :local))")]
    (is (empty? (codes r)) (pr-str (:diagnostics r)))
    (is (= [":done" "java.util.concurrent.Future" "java.util.concurrent.Future"
            "(s/coll-of int? :kind seq? :min-count 2 :max-count 2)"
            "(s/coll-of t/never :kind seq? :max-count 0)"
            "(s/coll-of t/never :kind seq?)" "t/never" "keyword?"]
           (mapv #(main/type-at r "app.clj" % 1) (range 5 13))))))

(deftest with-test-exposes-wrapped-definitions-across-namespaces
  (let [api (parse/parse-string
             "api.clj"
             "(ns sample.api (:require [clojure.test :as test :refer [with-test]]
                                       [clojure.spec.alpha :as s]))
(s/fdef echo :args (s/cat :x string?) :ret string?)
(test/with-test (defn echo [x] x)
  (echo \"ok\")
  (echo 42))
(with-test (defn answer [] 42)
  (def delayed :test-only))
(clojure.test/with-test (defn upper [^String x] (.toUpperCase x)))
(with-test)")
        client (parse/parse-string
                "client.clj"
                "(ns sample.client (:require [sample.api :as api :refer [answer]]))
(api/echo \"ok\")
(api/echo 42)
(answer)
(api/answer 1)
(api/upper \"ok\")
(api/upper 1)
api/delayed")
        ds (:diagnostics (check/check-files [api client] {}))]
    (is (= [["api.clj" 6 "TSP2345"]
            ["api.clj" 10 "TSP2554"]
            ["client.clj" 3 "TSP2345"]
            ["client.clj" 5 "TSP2554"]
            ["client.clj" 7 "TSP2345"]
            ["client.clj" 8 "TSP721-01"]]
           (mapv (juxt :file (comp :row :pos) :code) ds))
        (pr-str ds))))

(deftest conditional-steps-check-reachable-calls-in-source-order
  (let [r (checked "\n(cond-> 1 true (+ \"bad\"))
                       (cond->> \"bad\" true (+ 1))
                       (cond-> 1 false (+ \"skipped\") true inc)
                       (cond-> 1 true inc (inc \"bad test\") inc)
                       (let [__threaded \"bad\"] (cond-> 1 true (+ __threaded)))")
        ds (:diagnostics r)]
    (is (= (repeat 4 "TSP2345") (mapv :code ds)) (pr-str ds))
    (is (= [5 6 8 9] (mapv (comp :row :pos) ds)))
    (is (= 19 (get-in ds [0 :pos :col]))))
  (is (= #{"TSP2554"} (codes (checked "(cond-> 0 true)")))))

(deftest threaded-value-does-not-shadow-source-names
  (let [r (checked "\n(defn __threaded {:t/sig '[:-> string?]} [] \"abc\")
(->> 1 (subs (__threaded)))
(let [__threaded \"abc\" __threaded_0 \"def\"]
  (->> 1 (subs (str (subs __threaded 0) (subs __threaded_0 0)))))
(->> \"bad\" (subs (__threaded)))")]
    (is (= ["string?" "string?"] (mapv #(main/type-at r "app.clj" % 1) [6 7])))
    (is (= [[9 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))))

(deftest conditional-thread-joins-shapes-and-uses-branch-narrowing
  (let [r (checked "\n(defn thread-value {:t/sig '[(t/or nil? string?) :-> (t/or nil? int?)]}
                          [s] (cond-> s (some? s) count))
                       (defn options {:t/sig '[boolean? :-> map?]}
                          [flag] (cond-> {} flag (assoc :added 1) true (assoc :required \"yes\")))
                       (defn join-types {:t/sig '[boolean? :-> (t/or string? int?)]}
                          [flag] (cond->> 1 flag str))
                       (cond-> :identity)")]
    (is (empty? (:diagnostics r)) (pr-str (:diagnostics r)))
    (is (= "(s/nilable int?)" (main/type-at r "app.clj" 6 31)))
    (is (= "(t/or (s/keys :req [:added :required]) (s/keys :req [:required]))"
           (main/type-at r "app.clj" 8 34)))
    (is (= "(t/or string? 1)" (main/type-at r "app.clj" 10 34)))
    (is (= ":identity" (main/type-at r "app.clj" 11 24)))))

(deftest qualified-aliased-and-referred-bodies
  (doseq [head '[system/defstart sys/defstart defstart]]
    (let [r (checked (str "(" head " [ctx config] (query ctx \"sql\"))"))]
      (is (empty? (:diagnostics r)) (pr-str (:diagnostics r)))))
  (is (empty? (:diagnostics (checked "(sys/defstop [ctx state] (query ctx \"sql\") 42)"))))
  (is (empty? (:diagnostics (checked "(sys/defstart [ctx config])")))))

(deftest body-errors-are-not-hidden
  (doseq [body ["(sys/defstart [ctx config] (query ctx 42))"
                "(system/defstart [ctx config] (query config \"sql\"))"
                "(sys/defstop [ctx state] (query ctx 42))"]]
    (is (seq (:diagnostics (checked body))) body))
  (is (contains? (codes (checked "(sys/defstart [ctx config] (query ctx 42))")) "TSP2345"))
  (is (contains? (codes (checked "(sys/defstart [ctx config] 42)")) "TSP2322"))
  (is (contains? (codes (checked "(sys/defstart [ctx config] (:typo ctx) nil)")) "TSP2339")))

(deftest malformed-shape-fails-closed
  (doseq [body ["(sys/defstart ctx nil)" "(sys/defstart [ctx] nil)"
                "(sys/defstart [ctx cfg extra] nil)" "(sys/defstart [ctx ctx] nil)"
                "(sys/defstart [{:keys [db]} cfg] nil)" "(sys/defstart [ctx &] nil)"
                "(sys/defstart [foo/ctx cfg] nil)" "(sys/defstart)"]]
    (is (contains? (codes (checked body)) "TSP7032") body)))

(deftest malformed-config-fails-closed
  (doseq [cs [nil [] {'defstart contract}
              {'system/defstart (assoc contract :expand 'evil/run)}
              {'system/defstart (dissoc contract :ret)}
              {'system/defstart (assoc contract :shape :anything)}
              {'system/defstart (assoc contract :bindings '(map?))}
              {'system/defstart (assoc contract :bindings ['bogus?])}
              {'system/defstart (assoc contract :ret :missing/type)}
              {'system/defstart (assoc contract :ret '(t/or map? :missing/type))}
              {'system/defstart {:shape :malli-defn :private? :yes}}
              {'system/defstart {:shape :malli-defn :expand 'evil/run}}
              {'clojure.core/let contract}]]
    (is (contains? (codes (checked "" cs)) "TSP9004") (pr-str cs))))

(deftest lexical-shadowing-and-opt-in
  (is (empty? (:diagnostics
               (checked "(let [defstart (fn [n] n)] (defstart 42))"))))
  (let [r (check/check-files
           [(parse/parse-string "system.clj" "(ns system) (defmacro defstart [& xs] (throw (Exception. \"never run\")))")
            (parse/parse-string "app.clj" (str prelude "(sys/defstart [ctx cfg] 42)"))] {})]
    (is (contains? (codes r) "TSP7031"))
    (is (not (contains? (codes r) "TSP2322")))))

(deftest no-namespace-loading-or-macro-execution
  (let [r (checked "(missing.vendor/lifecycle [ctx cfg] (query ctx 42))"
                   {'missing.vendor/lifecycle contract})]
    (is (nil? (find-ns 'missing.vendor)))
    (is (contains? (codes r) "TSP2345")))
  (let [r (check/check-files
           [(parse/parse-string "system.clj"
              "(ns system) (defmacro defstart [& xs] (throw (Exception. \"must not expand\")))")
            (parse/parse-string "app.clj" (str prelude "(sys/defstart [ctx cfg] (query ctx 42))"))]
           {:macro-contracts contracts})]
    (is (contains? (codes r) "TSP2345"))
    (is (not (contains? (codes r) "TSP7031")))))

(def ^:private spec-guard-provider
  "(ns fixture.guard-api (:require [clojure.spec.alpha :as s]))
(s/def ::method #{:get :post})
(s/def ::request (s/keys :req-un [::method]))
(defn accept [ctx request] request)
(defmacro install [ctx {_method :method :as request}]
  `(let [result# (s/conform ~::request ~request)]
     (if (s/invalid? result#)
       (throw (ex-info \"Invalid request\" (s/explain-data ~::request ~request)))
       (accept ~ctx ~request))))")

(deftest static-spec-macro-argument-checks-work-across-namespaces
  (let [provider (parse/parse-string "provider.clj" spec-guard-provider)
        client (parse/parse-string "client.clj"
                                   "(ns client (:require [fixture.guard-api :as g :refer [install]]
                                                         [clojure.spec.alpha :as s]))
(g/install {} {:method :get})
(g/install {} {:method :trace})
(install {} {})
(install {})
(defn inbound [request] (install {} request))
(let [install (fn [_ request] request)] (install {} {:method :trace}))
(with-redefs [s/conform (fn [_ x] x)] (g/install {} {:method :trace}))")
        result (check/check-files [client provider] {})
        issues (:diagnostics result)]
    (is (= {:spec :fixture.guard-api/request :index 1 :arity 2}
           (get-in result [:env :spec-macro-contracts 'fixture.guard-api/install])))
    (is (= [[4 15 "TSP2322" :error] [5 13 "TSP2741" :error] [6 1 "TSP2554" :error]
            [7 37 "TSP210-01" :warning] [9 40 "TSP7031" :warning]]
           (mapv (juxt (comp :row :pos) (comp :col :pos) :code :severity) issues))
        (pr-str issues))
    (is (= issues (:diagnostics (check/check-files [provider client] {}))))
    (is (= "t/unknown" (main/type-at result "client.clj" 3 1)))
    (is (= {:cast 0 :any 0 :ignore 0} (:trust result)))
    (is (nil? (find-ns 'fixture.guard-api)))))

(deftest unsupported-spec-templates-retain-the-macro-warning
  (let [client (parse/parse-string "client.clj"
                                   "(ns client (:require [fixture.guard-api :as g]))
(g/install {} {:method :trace})")]
    (doseq [source [(str/replace spec-guard-provider "(s/invalid? result#)" "(s/invalid? different#)")
                    (str/replace spec-guard-provider "~::request" "~(identity ::request)")
                    (str/replace spec-guard-provider "(accept ~ctx ~request)" "(accept ~ctx ~@request)")
                    (str/replace spec-guard-provider "(throw (ex-info" "(do (ex-info")
                    (str/replace spec-guard-provider "(s/conform" "(custom/conform")
                    (str/replace spec-guard-provider "[ctx {_method" "[ctx & {_method")
                    (str spec-guard-provider "\n(defmacro install [& forms] (throw (Exception. \"opaque\")))")]]
      (let [result (check/check-files [client (parse/parse-string "provider.clj" source)] {})]
        (is (= ["TSP7031"] (mapv :code (:diagnostics result))) (pr-str (:diagnostics result)))
        (is (empty? (get-in result [:env :spec-macro-contracts])))))))
