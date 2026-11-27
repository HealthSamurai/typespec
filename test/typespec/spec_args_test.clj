(ns typespec.spec-args-test
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [typespec.check :as check]
            [typespec.main :as main]
            [typespec.parse :as parse]))

(defn- checked [source]
  (check/check-files [(parse/parse-string "spec_args.clj"
                        (str "(ns spec-args (:require [clojure.spec.alpha :as sp]))\n" source))] {}))

(defn- diagnostic-codes [result]
  (mapv :code (:diagnostics result)))

(deftest precondition-contracts-require-the-predicate-arity
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::text string?)
(defn valid [x] {:pre [(string? x)]} x)
(defn valid-spec [x] {:pre [(s/valid? ::text x)]} x)
(defn malformed [x] {:pre [(string? x 1)]} x)
(defn malformed-spec [x] {:pre [(s/valid? ::text x 1 true)]} x)
(defn spec-with-form [x] {:pre [(s/valid? ::text x 'text)]} x)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api]))
(api/valid \"text\")
(api/valid 42)
(api/valid-spec \"text\")
(api/valid-spec 42)
(api/malformed 42)
(api/malformed-spec 42)
(api/spec-with-form \"text\")
(api/spec-with-form 42)")
        r (check/check-files [api client] {})]
    (is (= [["api.clj" 5 "TSP2554"] ["api.clj" 6 "TSP2554"]
            ["client.clj" 3 "TSP2345"] ["client.clj" 5 "TSP2345"] ["client.clj" 9 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))))

(deftest preconditions-distinguish-core-vars-from-shadowed-predicates
  (let [api (parse/parse-string "api.clj" "(ns api (:refer-clojure :exclude [string?]))
(defn string? [x] (number? x))
(defn local-var [x] {:pre [(string? x)]} x)
(defn local-param [number? x] {:pre [(number? x)]} x)
(defn explicit [number? x] {:pre [(clojure.core/number? x)]} x)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api]))
(api/local-var 42)
(api/local-param string? \"text\")
(api/explicit string? 42)
(api/explicit string? \"text\")")
        r (check/check-files [api client] {})]
    (is (= [["client.clj" 5 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r))))
  (let [f (fn [number? x] {:pre [(number? x)]} x)]
    (is (= "text" (f string? "text")))))

(deftest preconditions-recognize-shadowing-through-parameter-destructuring
  (let [r (checked "(defn by-key [{:keys [string?]} x] {:pre [(string? x)]} x)
(defn by-position [[string?] x] {:pre [(string? x)]} x)
(defn by-rest [x & {:keys [string?]}] {:pre [(string? x)]} x)
(defn explicit [{:keys [string?]} x] {:pre [(clojure.core/string? x)]} x)
(by-key {:string? number?} 42)
(by-position [number?] 42)
(by-rest 42 :string? number?)
(explicit {:string? number?} \"text\")
(explicit {:string? number?} 42)")]
    (is (= [[10 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r))))
  (let [f (fn [{:keys [string?]} x] {:pre [(string? x)]} x)]
    (is (= 42 (f {:string? number?} 42)))))

(deftest preconditions-keep-the-narrower-compatible-type-in-either-order
  (let [api (parse/parse-string "api.clj" "(ns api)
(defn a [x] {:pre [(int? x) (number? x)]} x)
(defn b [x] {:pre [(number? x) (int? x)]} x)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api]))
(api/a 2)
(api/b 2)
(api/a 2.5)
(api/b 2.5)")
        r (check/check-files [api client] {})]
    (is (= [["client.clj" 4 "TSP2345"] ["client.clj" 5 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics r))))
    (is (= ["int?" "int?"] (mapv #(main/type-at r "client.clj" % 1) [2 3]))))
  (let [f (fn [x] {:pre [(int? x) (number? x)]} x)]
    (is (= 2 (f 2)))
    (is (thrown? AssertionError (f 2.5)))))

(deftest incompatible-preconditions-point-at-the-conflicting-condition
  (let [r (checked "(defn possible [x] {:pre [(int? x) (number? x)]} x)
(defn impossible [x] {:pre [(string? x) (number? x)]} x)
(possible 42)
(impossible 42)
(impossible \"text\")")]
    (is (= [[3 "TSP114-01"] [5 "TSP2345"] [6 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= 41 (get-in r [:diagnostics 0 :pos :col]))))
  (let [f (fn [x] {:pre [(string? x) (number? x)]} x)]
    (is (thrown? AssertionError (f 42)))
    (is (thrown? AssertionError (f "text")))))

(deftest precondition-nil-tests-preserve-false-and-detect-missing-values
  (let [r (checked "(defn present [x] {:pre [(some? x)]} x)
(defn absent [x] {:pre [(nil? x)]} x)
(present false)
(absent nil)
(present nil)
(absent false)
(defn impossible [x] {:pre [(some? x) (nil? x)]} x)")]
    (is (= [[6 "TSP2345"] [7 "TSP2345"] [8 "TSP114-01"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["some?" "nil?"]
           (mapv #(main/type-at r "spec_args.clj" % 1) [4 5]))))
  (let [present (fn [x] {:pre [(some? x)]} x)]
    (is (false? (present false)))
    (is (thrown? AssertionError (present nil)))))

(deftest preconditions-complement-direct-nil-tests
  (let [api (parse/parse-string "api.clj" "(ns api)
(defn present [x] {:pre [(not (nil? x))]} x)
(defn absent [x] {:pre [(not (some? x))]} x)
(defn shadowed [nil? x] {:pre [(not (nil? x))]} x)
(defn compound [x] {:pre [(not (and (nil? x) false))]} x)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api]))
(api/present false)
(api/present nil)
(api/absent nil)
(api/absent false)
(api/shadowed (constantly false) nil)
(api/compound nil)")
        r (check/check-files [api client] {})]
    (is (= [["client.clj" 3 "TSP2345"] ["client.clj" 5 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r))))
  (let [present (fn [x] {:pre [(not (nil? x))]} x)
        absent (fn [x] {:pre [(not (some? x))]} x)]
    (is (false? (present false)))
    (is (nil? (absent nil)))
    (is (thrown? AssertionError (present nil)))
    (is (thrown? AssertionError (absent false)))))

(deftest preconditions-infer-resolved-instance-contracts
  (let [api (parse/parse-string "api.clj" "(ns api (:import [java.io File]))
(defn file-name [x] {:pre [(instance? File x)]} (.getName x))
(defn shadowed [File x] {:pre [(instance? File x)]} x)
(defn present [x] {:pre [(instance? Object x)]} x)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api]))
(api/file-name (java.io.File. \"text\"))
(api/file-name \"text\")
(api/shadowed java.lang.String \"text\")
(api/present nil)")
        r (check/check-files [api client] {})]
    (is (= [["api.clj" 2 "TSP7023"] ["client.clj" 3 "TSP2345"] ["client.clj" 5 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r))))
  (let [f (fn [x] {:pre [(instance? java.io.File x)]} (.getName ^java.io.File x))]
    (is (= "text" (f (java.io.File. "text"))))
    (is (thrown? AssertionError (f "text")))))

(deftest preconditions-extract-facts-from-conjunctions
  (let [api (parse/parse-string "api.clj" "(ns api)
(defn text [x] {:pre [(and (string? x) (seq x))]} x)
(defn index [x] {:pre [(and (number? x) (and (int? x)))]} x)
(defn shadowed [and x] {:pre [(and (string? x))]} x)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api]))
(api/text \"text\")
(api/text 42)
(api/index 0)
(api/index 1.5)
(api/shadowed not 42)")
        r (check/check-files [api client] {})]
    (is (= [["client.clj" 3 "TSP2345"] ["client.clj" 5 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r))))
  (let [f (fn [x] {:pre [(and (string? x) (seq x))]} x)]
    (is (= "text" (f "text")))
    (is (thrown? AssertionError (f 42)))))

(deftest preconditions-infer-unions-only-with-a-fact-for-each-alternative
  (let [api (parse/parse-string "api.clj" "(ns api)
(defn label [x] {:pre [(or (string? x) (keyword? x))]} (name x))
(defn nil-or-positive [x] {:pre [(or (nil? x) (and (number? x) (pos? x)))]} x)
(defn mixed [x y] {:pre [(or (string? x) (number? y))]} x)
(defn opaque [x] {:pre [(or (string? x) (identity x))]} x)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api]))
(api/label \"text\")
(api/label :ok)
(api/label 42)
(api/nil-or-positive nil)
(api/nil-or-positive 42)
(api/nil-or-positive :wrong)
(api/mixed :ok 42)
(api/opaque 42)")
        r (check/check-files [api client] {})]
    (is (= [["client.clj" 4 "TSP2345"] ["client.clj" 7 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r))))
  (let [f (fn [x] {:pre [(or (nil? x) (and (number? x) (pos? x)))]} x)]
    (is (nil? (f nil)))
    (is (= 42 (f 42)))
    (is (thrown? AssertionError (f -1)))
    (is (thrown? AssertionError (f :wrong)))))

(deftest later-preconditions-eliminate-disjoint-union-alternatives
  (let [api (parse/parse-string "api.clj" "(ns api)
(defn choose [x]
  {:pre [(or (string? x) (number? x))
         (or (string? x) (keyword? x))
         (number? x)]}
  x)
(defn available [x]
  {:pre [(or (string? x) (number? x))
         (or (string? x) (keyword? x))]}
  x)
(defn present [x]
  {:pre [(some? x) (or (nil? x) (string? x))]}
  x)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api]))
(api/choose 42)
(api/available \"text\")
(api/available 42)
(api/present \"text\")
(api/present false)")
        r (check/check-files [api client] {})]
    (is (= [["api.clj" 5 "TSP114-01"] ["client.clj" 2 "TSP2345"]
            ["client.clj" 4 "TSP2345"] ["client.clj" 6 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["string?" "string?"] (mapv #(main/type-at r "client.clj" % 1) [3 5]))))
  (let [f (fn [x] {:pre [(or (string? x) (number? x))
                         (or (string? x) (keyword? x))]} x)]
    (is (= "text" (f "text")))
    (is (thrown? AssertionError (f 42)))))

(deftest an-unconstrained-spec-precondition-keeps-the-concrete-fact
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::anything any?)
(defn guarded [x] {:pre [(string? x) (s/valid? ::anything x)]} (subs x 0))
(defn reversed [x] {:pre [(s/valid? ::anything x) (string? x)]} (subs x 0))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api]))
(api/guarded \"text\")
(api/guarded 42)
(api/reversed \"text\")
(api/reversed 42)")
        r (check/check-files [client api] {})]
    (is (= [["client.clj" 3 "TSP2345"] ["client.clj" 5 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["string?" "string?"] (mapv #(main/type-at r "client.clj" % 1) [2 4]))))
  (let [f (fn [x] {:pre [(string? x) (s/valid? any? x)]} (subs x 0))]
    (is (= "text" (f "text")))
    (is (thrown? AssertionError (f 42)))))

(deftest explicit-contracts-keep-their-call-types-and-refine-function-bodies
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/fdef text :args (s/cat :x (s/nilable string?)) :ret string?)
(defn text [x] {:pre [(some? x)]} (subs x 0))
(defn tagged {:t/sig '[any? :-> string?]} [x] {:pre [(string? x)]} (subs x 0))
(defn wrong {:t/sig '[any? :-> string?]} [x] {:pre [(string? x)]} (inc x))
(defn malformed {:t/sig '[any? :-> string?]} [x]
  {:pre [(string? x) (number? x 0)]}
  (subs x 0))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api]))
(api/text \"text\")
(api/text nil)
(api/text 42)
(api/tagged :tag)")
        r (check/check-files [api client] {})]
    (is (= [["api.clj" 5 "TSP2322"] ["api.clj" 5 "TSP2345"] ["api.clj" 7 "TSP2554"]
            ["client.clj" 4 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r))))
  (let [f (fn [x] {:pre [(some? x)]} (subs x 0))]
    (is (= "text" (f "text")))
    (is (thrown? AssertionError (f nil)))))

(deftest one-or-more-in-calls-and-partial
  (let [r (checked "(sp/fdef total :args (sp/cat :prefix string? :xs (sp/+ int?)) :ret string?)
(defn total [prefix & xs] (if xs prefix nil))
(total \"ok\" 1 2)
(total \"too short\")
(apply total \"too short\" [])
(apply total [\"ok\" 1])
(apply total [\"wrong\" :value])
(let [f (partial total \"too short\")] (f))
(let [f (partial total \"ok\" 1)] (f))
(sp/fdef optional :args (sp/cat :xs (sp/* int?)) :ret any?)
(defn optional [& xs] xs)
(optional)")]
    (is (= ["TSP2554" "TSP2554" "TSP2345" "TSP2554"] (diagnostic-codes r))
        (pr-str (:diagnostics r)))
    (is (str/includes? (:message (first (:diagnostics r))) "expects 2+ argument(s)"))))

(deftest finite-alternatives-in-another-namespace
  (let [api (parse/parse-string "api.clj"
              "(ns api (:require [clojure.spec.alpha :as spec]))
(spec/fdef lookup :args (spec/alt :one int? :two (spec/cat :key string? :id int?)) :ret string?)
(defn lookup ([id] (str id)) ([key id] (str key id)))")
        client (parse/parse-string "client.clj"
                 "(ns client (:require [api :as a]))
(a/lookup 1)
(apply a/lookup [\"key\" 1])
(a/lookup \"key\")
(a/lookup \"key\" :oops)
(apply a/lookup [])")
        r (check/check-files [client api] {})]
    (is (= [["client.clj" 4 "TSP2345"] ["client.clj" 5 "TSP2345"] ["client.clj" 6 "TSP2554"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["string?" "string?"] (mapv #(main/type-at r "client.clj" % 1) [2 3])))))

(deftest named-argument-regexes-use-their-declaring-namespace
  (let [domain (parse/parse-string "domain.clj" "(ns domain (:require [clojure.spec.alpha :as spec]))
(spec/def ::pair (spec/cat :text string? :n int?))
(spec/def ::kind (spec/? keyword?))
(spec/def ::count int?)")
        api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s] [domain :as d]))
(s/fdef render :args ::alias :ret string?)
(s/def ::alias ::args)
(s/def ::args (s/cat :pair ::d/pair :kind ::d/kind))
(defn render ([text n] (render text n :normal)) ([text n kind] (str text n kind)))
(s/fdef total :args (s/cat :counts (s/+ ::d/count)) :ret int?)
(defn total [& counts] (reduce + counts))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(a/render \"hi\" 2)
(apply a/render [\"hi\" 2 :fast])
(a/render 2 \"hi\")
(a/render \"hi\" 2 \"fast\")
(a/render \"hi\")
(a/total 2 3)
(a/total :wrong)
(a/total)")
        result (check/check-files [client api domain] {})]
    (is (= [["api.clj" 4 "TSP9002"]
            ["client.clj" 4 "TSP2345"] ["client.clj" 4 "TSP2345"]
            ["client.clj" 5 "TSP2345"] ["client.clj" 6 "TSP2554"]
            ["client.clj" 8 "TSP2345"] ["client.clj" 9 "TSP2554"]
            ["domain.clj" 2 "TSP9002"] ["domain.clj" 3 "TSP9002"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))))

(deftest named-argument-regexes-preserve-unsupported-cases
  (let [result (checked "(sp/def ::ordinary int?)
(sp/def ::part (sp/cat :x int? :y int?))
(sp/def ::recursive (sp/cat :next ::recursive))
(sp/fdef ordinary :args ::ordinary)
(sp/fdef repeated :args (sp/cat :parts (sp/+ ::part)))
(sp/fdef recursive :args ::recursive)
(sp/fdef missing :args ::missing)")]
    (is (= [[3 "TSP9002"] [4 "TSP9002"] [5 "TSP9002"] [6 "TSP9002"]
            [7 "TSP9002"] [8 "TSP9002"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))))

(deftest nested-alternatives-and-optional-before-rest
  (let [spec (s/cat :prefix (s/? string?) :xs (s/+ int?))]
    (is (= [true true false false]
           (mapv #(s/valid? spec %) [[1] ["prefix" 1 2] [] ["prefix"]]))))
  (let [r (checked "(sp/fdef render :args (sp/cat :prefix (sp/? string?) :xs (sp/+ int?)) :ret any?)
(defn render [& xs] xs)
(render 1)
(render \"prefix\" 1 2)
(render)
(apply render [\"prefix\"])
(sp/fdef lookup :args (sp/cat :target (sp/alt :idx int? :name string?) :fallback (sp/? keyword?)) :ret any?)
(defn lookup [target & fallback] [target fallback])
(lookup \"name\" :default)
(lookup 1)
(lookup :wrong)
(sp/fdef nested :args (sp/cat :part (sp/cat :value int? :label string?) :flag boolean?) :ret any?)
(nested 1 \"name\" true)")]
    (is (= ["TSP2554" "TSP2345" "TSP2345"] (diagnostic-codes r))
        (pr-str (:diagnostics r)))))

(deftest checks-implementation-and-fspec-minimum
  (let [r (checked "(sp/fdef render :args (sp/alt :idx int? :name string?) :ret int?)
(defn render [x] x)
(sp/fdef only-zero :args (sp/alt :zero (sp/cat) :one int?) :ret int?)
(defn only-zero [] 0)
(sp/fdef invoke :args (sp/cat :f (sp/fspec :args (sp/cat :xs (sp/+ int?)) :ret int?)) :ret int?)
(defn invoke [f] (f 2))
(defn one {:t/sig '[int? :-> int?]} [x] x)
(invoke one)
(invoke (fn [x & xs] x))")]
    (is (= ["TSP2322" "TSP2790" "TSP2345"] (diagnostic-codes r))
        (pr-str (:diagnostics r)))))

(deftest finite-overloads-as-callbacks-and-equivalent-rest-signatures
  (let [r (checked "(sp/fdef show :args (sp/alt :idx int? :name string?) :ret string?)
(defn show [x] (str x))
(sp/fdef invoke :args (sp/cat :f (sp/fspec :args (sp/alt :idx int? :name string?) :ret string?)) :ret string?)
(defn invoke [f] (f \"text\"))
(invoke show)
(sp/fdef keep-first :args (sp/cat :xs (sp/+ int?)) :ret int?)
(defn keep-first {:t/sig '[int? & int? :-> int?]} [x & xs] x)
(keep-first 2)")]
    (is (empty? (:diagnostics r)) (pr-str (:diagnostics r)))))

(deftest unsupported-regexes-are-reported
  (let [r (checked "(sp/fdef nonterminal :args (sp/cat :xs (sp/+ int?) :last string?) :ret any?)
(sp/fdef unbounded-alt :args (sp/alt :one int? :more (sp/+ string?)) :ret any?)
(sp/fdef malformed :args (sp/alt :missing) :ret any?)")]
    (is (= ["TSP9002" "TSP9002" "TSP9002"] (diagnostic-codes r))
        (pr-str (:diagnostics r)))))
