(ns typespec.destructuring-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [typespec.check :as check]
            [typespec.parse :as parse]))

(defn- diagnostics [source]
  (:diagnostics
   (check/check-files [(parse/parse-string "client.clj"
                                         (str "(ns client (:require [clojure.spec.alpha :as s]))\n" source))]
                      {})))

(defn- locations [issues]
  (mapv (juxt (comp :row :pos) :code) issues))

(deftest destructured-parameters-accept-extra-literal-keys-across-namespaces
  (let [api (parse/parse-string
             "storage.clj"
             "(ns storage (:require [clojure.spec.alpha :as s]))
(s/def ::_account string?)
(s/def ::bucket string?)
(defn upload [{:keys [_account upload-threads storage/bucket] :as options}]
  options)
(s/fdef declared :args (s/cat :options (s/keys :req-un [::_account])) :ret map?)
(defn declared [options] options)")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [storage :as store]))
(store/upload {:account \"north\" :storage/bucket \"uploads\"})
(store/upload {:_account \"north\" :upload-threads 4})
(store/upload {:storage/bucket 42})
(store/declared {:_account \"north\"})
(store/declared {:_account \"north\" :account \"typo\"})
(store/declared {})")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 4 "TSP2322"]
            ["client.clj" 6 "TSP2353"]
            ["client.clj" 7 "TSP2741"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest map-of-supplies-types-for-string-and-symbol-bindings
  (let [api "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::counts (s/map-of string? int?))
(s/def ::symbols (s/map-of symbol? string?))
(s/fdef count-one :args (s/cat :m ::counts) :ret int?)
(defn count-one [{:strs [n] :or {n 0}}] (inc n))
(s/fdef print-one :args (s/cat :m ::symbols) :ret string?)
(defn print-one [{:syms [name] :or {name \"missing\"}}] name)
(s/fdef broken :args (s/cat :m ::counts) :ret string?)
(defn broken [{:strs [n] :or {n 0}}] n)"
        client "(ns client (:require [api :as api]))
(api/count-one {\"n\" 2})
(api/count-one {:n 2})
(api/count-one {\"n\" \"two\"})
(api/print-one {'name \"file\"})
(api/print-one {\"name\" \"file\"})"
        issues (:diagnostics
                (check/check-files [(parse/parse-string "client.clj" client)
                                    (parse/parse-string "api.clj" api)] {}))]
    (is (= [["api.clj" 9 "TSP2322"]
            ["client.clj" 3 "TSP2345"]
            ["client.clj" 4 "TSP2345"]
            ["client.clj" 6 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest destructuring-defaults-run-for-absent-keys-and-preserve-stored-nil
  (let [source "(let [{n :count :or {n 7}} {}] (inc n))
(let [{n \"count\" :or {n 7}} nil] (inc n))
(let [{n 'count :or {n 7}} {'count 3}] (inc n))
(let [{:strs [n] :or {n 7}} {:n nil}] (inc n))
(let [{:syms [n] :or {n 7}} {\"n\" nil}] (inc n))
(let [{:strs [n] :or {n 7}} {\"n\" nil}] (inc n))
(let [{n :count :or {n 7}} {:count nil}] (inc n))
(let [{:keys [n] :or {n (.substring 3 1)}} {:n 2}] (inc n))
(let [{:keys [n] :or {n (inc (:seed m))} :as m} {:seed 2}] (inc n))"
        issues (diagnostics source)]
    (is (= [[7 "TSP2531"] [8 "TSP2531"] [9 "TSP2339"]] (locations issues))
        (pr-str issues))
    (is (every? #(str/includes? (:message %) ":or applies only when the key is absent")
                (take 2 issues)))))

(deftest literal-map-types-survive-destructuring-and-narrowing
  (let [source "(let [{n \"fixture/n\"} {\"fixture/n\" 1}] (inc n))
(let [{:fixture/syms [n]} {'fixture/n 1}] (inc n))
(let [{:syms [fixture/n]} {'fixture/n 1}] (inc n))
(let [{{:strs [n]} 'outer} {'outer {\"n\" 1}}] (inc n))
(let [{:strs [n] :or {n \"missing\"}} {\"n\" 1}] (inc n))
(let [{:syms [n]} {'n 1}] (.substring n 1))
(defn branch [flag]
  (let [{:strs [tag value]} (if flag {\"tag\" :left \"value\" 1}
                                     {\"tag\" :right \"value\" \"text\"})]
    (case tag
      :left (inc value)
      :right (.substring value 1))))"
        issues (diagnostics source)]
    (is (= [[7 "TSP2339"] [13 "TSP7023"]] (locations issues)) (pr-str issues))
    (is (str/includes? (:message (second issues)) "value is known to be String"))))

(deftest type-hints-are-checked-against-the-selected-field
  (let [issues (diagnostics "(let [{:strs [^String label]} {\"label\" \"text\"}] (.substring label 1))
(let [{:strs [^String label]} {\"label\" 7}] (.substring label 1))")]
    (is (= [[3 "TSP2322"]] (locations issues)) (pr-str issues))))

(deftest computed-selectors-check-the-expression-and-bind-the-selected-field
  (let [issues (diagnostics "(let [key :n {n key} {:n 2}] (inc n))
(let [key \"n\" {n key} {\"n\" \"bad\"}] (inc n))
(let [key :missing {n key :or {n 7}} {:n nil}] (inc n))
(let [key :n {n key :or {n 7}} {:n nil}] (inc n))
(let [key :outer {{:strs [n]} key} {:outer {\"n\" 2}}] (inc n))
(let [{n (:selector m) :as m} {:selector :n :n 2}] (inc n))
(let [{n (inc \"bad\")} {:n 2}] n)")]
    (is (= [[3 "TSP2345"] [5 "TSP2531"] [8 "TSP2345"]] (locations issues))
        (pr-str issues))
    (is (str/includes? (:message (second issues)) ":or applies only when the key is absent"))))

(deftest computed-selectors-use-map-of-contracts-across-namespaces
  (let [api (parse/parse-string
             "lookup.clj"
             "(ns lookup (:require [clojure.spec.alpha :as s]))
(s/fdef read-at :args (s/cat :selector string? :values (s/map-of string? int?)) :ret int?)
(defn read-at [selector {value selector :or {value 0}}] (inc value))
(s/fdef nilable-at :args (s/cat :selector string? :values (s/map-of string? (s/nilable int?))) :ret int?)
(defn nilable-at [selector {value selector :or {value 0}}] (inc value))")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [lookup :as l]))
(l/read-at \"n\" {\"n\" 2})
(l/read-at \"missing\" {\"n\" 2})
(l/read-at \"n\" {\"n\" \"bad\"})")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 4 "TSP2345"] ["lookup.clj" 5 "TSP2531"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))
    (is (str/includes? (:message (second issues)) "value selected by selector is (s/nilable int?)"))))

(deftest finite-selectors-preserve-types-presence-and-defaults
  (let [issues (diagnostics "(defn present [flag] (let [key (if flag :n \"n\") {n key :or {n :unused}} {:n 1 \"n\" 2}] (inc n)))
(defn fallback [flag] (let [key (if flag :left :right) {n key :or {n 0}} {:left 1}] (inc n)))
(defn missing [flag] (let [key (if flag :left :right) {n key} {:left 1}] (inc n)))
(defn stored-nil [flag] (let [key (if flag :left :right) {n key :or {n 0}} {:left 1 :right nil}] (inc n)))
(defn nested [flag] (let [key (if flag :left :right) {{:strs [n]} key} {:left {\"n\" 1} :right {\"n\" 2}}] (inc n)))
(defn checked-default [flag] (let [key (if flag :left :right) {n key :or {n (inc \"bad\")}} {:left 1 :right 2}] n))")]
    (is (= [[4 "TSP2531"] [5 "TSP2531"] [7 "TSP2345"]] (locations issues))
        (pr-str issues))
    (is (str/includes? (:message (second issues)) ":or applies only when the key is absent"))))

(deftest finite-selectors-use-specs-from-another-namespace
  (let [api (parse/parse-string
             "sides.clj"
             "(ns sides (:require [clojure.spec.alpha :as s]))
(s/def ::side #{:left :right})
(s/def ::left int?)
(s/def ::right int?)
(s/fdef choose :args (s/cat :left? boolean?) :ret ::side)
(defn choose [left?] (if left? :left :right))
(s/fdef numbers :args (s/cat) :ret (s/keys :req-un [::left ::right]))
(defn numbers [] {:left 1 :right 2})")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [sides :as sides] [clojure.spec.alpha :as s]))
(s/fdef positive :args (s/cat :left? boolean?) :ret int?)
(defn positive [left?] (let [key (sides/choose left?) {n key} (sides/numbers)] (inc n)))
(s/fdef wrong :args (s/cat :left? boolean?) :ret any?)
(defn wrong [left?] (let [key (sides/choose left?) {n key} {:left 1 :right \"bad\"}] (inc n)))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 5 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest map-destructuring-normalizes-known-sequences
  (let [issues (diagnostics "(let [{:keys [n]} (list :n \"first\" :n 2)] (inc n))
(let [{:keys [n]} (list :n \"first\" {:n 2})] (inc n))
(let [{:strs [n]} (list \"n\" 2)] (inc n))
(let [{:keys [n]} (list {:n 2})] (inc n))
(let [{:keys [n] :or {n 0}} (list)] (inc n))
(let [{:keys [n] :or {n 0}} (list nil)] (inc n))
(let [[{:keys [n]}] [(seq [:n 2])]] (inc n))
(let [{:keys [n]} (list :n 2 :n \"bad\")] (inc n))
(let [{:keys [n] :or {n 0}} (list :n 2 {:n nil})] (inc n))
(s/fdef vector-as :args (s/cat) :ret (s/tuple #{:n} int?))
(defn vector-as [] (let [{:as raw} [:n 2]] raw))")]
    (is (= [[9 "TSP2345"] [10 "TSP2531"]] (locations issues)) (pr-str issues))))

(deftest map-destructuring-normalizes-a-tuple-contract-from-another-namespace
  (let [api (parse/parse-string
             "options.clj"
             "(ns options (:require [clojure.spec.alpha :as s]))
(s/fdef good :args (s/cat) :ret (s/tuple #{:n} int?))
(defn good [] [:n 2])
(s/fdef bad :args (s/cat) :ret (s/tuple #{:n} string?))
(defn bad [] [:n \"bad\"])")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [options :as options] [clojure.spec.alpha :as s]))
(s/def ::n int?)
(s/fdef read-options :args (s/cat) :ret (s/keys :req-un [::n]))
(defn read-options [] (let [{:keys [n] :as m} (seq (options/good))] (inc n) m))
(let [{:keys [n]} (seq (options/bad))] (inc n))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 5 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest named-map-bindings-follow-the-expanded-map-order
  (let [issues (diagnostics "(let [i \"outer\" {:keys [a b c d e f g h i] :or {a (inc i)}} {:i 2}] a)
(let [a \"outer\" {:keys [a b c d e f g h i] :or {i (inc a)}} {:a 2}] i)
(let [i :outer {n i :keys [a b c d e f g h i]} {:i :value :outer \"bad\" :value 3}] (inc n))
(let [i \"outer\" {a :a b :b c :c d :d e :e f :f g :g h :h i :i :or {a (inc i)}} {:i 2}] a)
(let [{n (inc \"unused\") :keys [n]} {:n 7}] (inc n))
(let [{:keys [n] :strs [n]} {:n 2 \"n\" \"right\"}] (subs n 0))
(let [{^String n :unused :keys [n]} {:n \"kept\"}] (.substring n 0))
(let [{^String n :unused :keys [n]} {:n 1}] (.substring n 0))")]
    (is (= [[3 "TSP2345"] [9 "TSP2322"]] (locations issues)) (pr-str issues))))

(deftest binding-order-preserves-the-spec-contract-across-namespaces
  (let [api (parse/parse-string
             "rows.clj"
             "(ns rows (:require [clojure.spec.alpha :as s]))
(s/def ::a int?)
(s/def ::b int?)
(s/def ::c int?)
(s/def ::d int?)
(s/def ::e int?)
(s/def ::f int?)
(s/def ::g int?)
(s/def ::h int?)
(s/def ::i int?)
(s/fdef default-a :args (s/cat :row (s/keys :req-un [::i] :opt-un [::a ::b ::c ::d ::e ::f ::g ::h])) :ret int?)
(defn default-a [row]
  (let [i \"outer\" {:keys [a b c d e f g h i] :or {a (inc i)}} row] a))")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [rows :as rows]))
(rows/default-a {:i 2})
(rows/default-a {:i \"bad\"})")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 3 "TSP2322"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest clojure-1-12-destructuring-runtime-cases
  (is (= [7 nil 3 1 3 4]
         (let [{absent :count :or {absent 7}} nil
               {:strs [n] :or {n 7}} {"n" nil}
               {present 'count :or {present 7}} {'count 3}
               {a "fixture/a"} {"fixture/a" 1}
               {:fixture/syms [c]} {'fixture/c 3}
               {:syms [fixture/d]} {'fixture/d 4}]
           [absent n present a c d])))
  (is (= [2 1]
         (let [calls (atom 0)
               selector (fn [] (swap! calls inc) :n)
               {value (selector)} {:n 2}]
           [value @calls]))))
