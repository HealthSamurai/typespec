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
