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

(deftest map-alias-type-hints-are-checked-and-used-for-interop
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::label string?)
(s/fdef options-size :args (s/cat :opts (s/keys :req-un [::label])) :ret int?)
(defn options-size [{:keys [label] :as ^java.util.Map opts}]
  (.size opts))
(defn invalid-hint []
  (let [{:as ^String opts} {:label \"hello\"}]
    (.length opts)))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(a/options-size {:label \"hello\"})
(a/options-size {:label 42})")
        issues (:diagnostics (check/check-files [client api] {}))]
    (is (= [["api.clj" 7 "TSP2322"] ["client.clj" 3 "TSP2322"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))
    (is (str/includes? (:message (first issues)) "hint String contradicts the type"))))

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

(deftest nested-map-and-vector-bindings-follow-the-expanded-map-order
  (let [issues (diagnostics "(let [i :outer {{:keys [n]} i :keys [a b c d e f g h i]} {:i :value :outer {:n \"bad\"} :value {:n 3}}] (inc n))
(let [i :outer {[amount] i :keys [a b c d e f g h i]} {:i :value :outer [\"bad\"] :value [3]}] (inc amount))
(let [i :outer {[amount] i :keys [a b c d e f g h i]} {:i :value :outer [3] :value [\"bad\"]}] (inc amount))
(let [i :outer {[n] i :keys [a b c d e f g h i]} {:i :value :outer [3] :value [\"bad\"]}] (inc n))")]
    (is (= [[4 "TSP2345"]] (locations issues)) (pr-str issues))))

(deftest nested-binding-order-uses-specs-from-another-namespace
  (let [api (parse/parse-string
             "options.clj"
             "(ns options (:require [clojure.spec.alpha :as s]))
(s/def ::n int?)
(s/def :outer/n string?)
(s/fdef data :args (s/cat)
  :ret (s/tuple #{:i} #{:value} #{:value} (s/keys :req-un [::n]) #{:outer} (s/keys :req-un [:outer/n])))
(defn data [] [:i :value :value {:n 3} :outer {:n \"bad\"}])")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [options :as options] [clojure.spec.alpha :as s]))
(s/fdef read-value :args (s/cat) :ret int?)
(defn read-value []
  (let [i :outer {{:keys [n]} i :keys [a b c d e f g h i]} (seq (options/data))] (inc n)))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (empty? issues) (pr-str issues))))

(deftest map-destructuring-keeps-quoted-list-positions
  (let [issues (diagnostics "(let [{:keys [port]} '(:port 5432)] (inc port))
(let [{:syms [port]} '(port 5432)] (inc port))
(let [{:keys [port]} '({:port 5432})] (inc port))
(let [{:keys [port] :or {port 0}} (quote ())] (inc port))
(let [{[port] :ports} '(:ports [5432])] (inc port))
(let [{:keys [expression]} '(:expression (inc \"data\"))] (first expression))
(let [{:keys [port]} '(:port 5432 :port \"bad\")] (inc port))
(let [{:keys [port] :or {port 0}} '(:port 5432 {:port nil})] (inc port))")]
    (is (= [[8 "TSP2345"] [9 "TSP2531"]] (locations issues)) (pr-str issues))))

(deftest quoted-list-destructuring-supports-spec-contracts-across-namespaces
  (let [api (parse/parse-string
             "options.clj"
             "(ns options (:require [clojure.spec.alpha :as s]))
(s/def ::port int?)
(s/fdef configured :args (s/cat) :ret (s/keys :req-un [::port]))
(defn configured [] (let [{:keys [port] :as m} (quote (:port 5432))] (inc port) m))")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [options :as options]))
(inc (:port (options/configured)))
(subs (:port (options/configured)) 0)")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 3 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest sequence-pairs-preserve-map-choices-and-last-writes
  (let [issues (diagnostics "(defn fallback [flag] (let [{:keys [n] :or {n 0}} (list (if flag :n :other) 7)] (inc n)))
(defn suffix [flag] (let [{:keys [n]} (list :n \"old\" (if flag :n :other) \"bad\" {:n 2})] (inc n)))
(defn wrong [flag] (let [{:keys [n]} (list :n 2 (if flag :n :other) \"bad\")] (inc n)))
(defn stored-nil [flag] (let [{:keys [n] :or {n 0}} (list (if flag :n :other) nil)] (inc n)))
(s/def ::left int?)
(s/def ::right int?)
(s/fdef pick :args (s/cat :flag boolean?) :ret (s/or :left (s/keys :req-un [::left]) :right (s/keys :req-un [::right])))
(defn pick [flag] (let [{:as m} (list (if flag :left :right) 1)] m))")]
    (is (= [[4 "TSP2345"] [5 "TSP2531"]] (locations issues)) (pr-str issues)))
  (let [issues (diagnostics (str "(defn many [flag] (let [key (if flag :n :other) "
                                 "{:keys [n]} (list " (apply str (repeat 9 "key 1 ")) ":n 10)] (inc n)))"))]
    (is (= [[2 "TSP210-01"]] (locations issues)) (pr-str issues))))

(deftest sequence-pairs-use-selector-and-tuple-specs-across-namespaces
  (let [api (parse/parse-string
             "options.clj"
             "(ns options (:require [clojure.spec.alpha :as s]))
(s/def ::selector #{:n :other})
(s/fdef key-for :args (s/cat :flag boolean?) :ret ::selector)
(defn key-for [flag] (if flag :n :other))
(s/fdef entries :args (s/cat :flag boolean?) :ret (s/tuple ::selector int?))
(defn entries [flag] [(key-for flag) 7])")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [options :as options] [clojure.spec.alpha :as s]))
(s/fdef read-value :args (s/cat :flag boolean?) :ret int?)
(defn read-value [flag] (let [{:keys [n] :or {n 0}} (seq (options/entries flag))] (inc n)))
(s/fdef wrong :args (s/cat :flag boolean?) :ret any?)
(defn wrong [flag] (let [{:keys [n]} (list :n 2 (options/key-for flag) \"bad\")] (inc n)))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 5 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest odd-sequence-tails-follow-map-conj-rules
  (let [issues (diagnostics "(let [{:keys [n]} (list :n \"old\" [:n 3])] (inc n))
(let [{:keys [n]} (list :n 2 \"\")] (inc n))
(let [{:as m} (list :n 2 (seq {:n 4}))] m)
(let [{:as singleton} (list 42)] singleton)
(let [{:as m} (list :n 2 42)] m)
(let [{:as m} (list :n 2 \"bad\")] m)
(let [{:as m} (list :n 2 [:n])] m)
(let [{:keys [n]} (list :n 2 [:n \"bad\"])] (inc n))
(defn maybe-tail [flag] (let [{:keys [n]} (list :n 2 (if flag 42 {:n 3}))] (inc n)))
(defn unknown-key [k] (let [{:as m} (list k 2 42)] m))")]
    (is (= [[6 "TSP2345"] [7 "TSP2345"] [8 "TSP2345"] [9 "TSP2345"] [10 "TSP2345"] [11 "TSP2345"]]
           (locations issues))
        (pr-str issues))
    (is (str/includes? (:message (first issues)) "cannot append 42 as map entries"))
    (is (= "map destructuring tail expects a two-element vector, got length 1"
           (:message (nth issues 2))))))

(deftest odd-sequence-tails-use-tuple-specs-across-namespaces
  (let [api (parse/parse-string
             "options.clj"
             "(ns options (:require [clojure.spec.alpha :as s]))
(s/fdef good :args (s/cat) :ret (s/tuple #{:n} int? (s/tuple #{:n} int?)))
(defn good [] [:n 2 [:n 3]])
(s/fdef bad :args (s/cat) :ret (s/tuple #{:n} int? int?))
(defn bad [] [:n 2 42])")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [options :as options] [clojure.spec.alpha :as s]))
(s/fdef amount :args (s/cat) :ret int?)
(defn amount [] (let [{:keys [n]} (seq (options/good))] (inc n)))
(let [{:as m} (seq (options/bad))] m)")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 4 "TSP2345"]] (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest find-entries-preserve-presence-and-the-selected-value
  (let [issues (diagnostics "(let [{:keys [n]} (list :n \"old\" (find {:n 4} :n))] (inc n))
(let [{:keys [n]} (list :n 2 (find {} :n))] (inc n))
(let [{:keys [n]} (list :n 2 (find nil :n))] (inc n))
(let [{:keys [n] :or {n 0}} (list :n 2 (find {:n nil} :n))] (inc n))
(let [{:strs [n]} (list \"n\" 2 (find {\"n\" \"bad\"} \"n\"))] (inc n))
(s/fdef entry-key :args (s/cat) :ret #{:n})
(defn entry-key [] (key (find {:n 4} :n)))
(s/fdef from-map :args (s/cat :values (s/map-of string? int?)) :ret int?)
(defn from-map [values] (let [{:strs [n]} (list \"n\" 2 (find values \"n\"))] (inc n)))
(let [{:syms [n]} (list :unused \"old\" (find {'n 4} 'n))] (inc n))
(find {} :n :extra)")]
    (is (= [[5 "TSP2531"] [6 "TSP2345"] [12 "TSP2554"]] (locations issues)) (pr-str issues))))

(deftest find-entries-preserve-field-contracts-across-namespaces
  (let [api (parse/parse-string
             "options.clj"
             "(ns options (:require [clojure.spec.alpha :as s]))
(s/def ::n int?)
(s/def :text/n string?)
(s/fdef numbers :args (s/cat) :ret (s/keys :req-un [::n]))
(defn numbers [] {:n 4})
(s/fdef texts :args (s/cat) :ret (s/keys :req-un [:text/n]))
(defn texts [] {:n \"bad\"})")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [options :as options] [clojure.spec.alpha :as s]))
(s/fdef amount :args (s/cat) :ret int?)
(defn amount [] (let [{:keys [n]} (list :n \"old\" (find (options/numbers) :n))] (inc n)))
(let [{:keys [n]} (list :n 2 (find (options/texts) :n))] (inc n))
(s/fdef entry-value :args (s/cat) :ret int?)
(defn entry-value [] (val (find (options/numbers) :n)))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 4 "TSP2345"]] (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest odd-sequence-tails-check-map-entry-positions
  (let [issues (diagnostics "(let [{:keys [n]} (list :n \"old\" (list (find {:n 4} :n)))] (inc n))
(let [{:keys [n]} (list :n 2 (list))] (inc n))
(let [{:keys [n]} (list :n 2 (find {} :n))] (inc n))
(let [{:keys [n] :or {n 0}} (list :n 2 (list (find {:n nil} :n)))] (inc n))
(let [{:as m} (list :n 2 (list (find {} :n)))] m)
(let [{:as m} (list :n 2 (list (find {:n 4} :n) 42))] m)
(s/fdef maybe-entry :args (s/cat :values (s/map-of keyword? int?)) :ret int?)
(defn maybe-entry [values] (let [{:keys [n]} (list :n 2 (list (find values :n)))] (inc n)))")]
    (is (= [[5 "TSP2531"] [6 "TSP2531"] [7 "TSP2345"] [9 "TSP2531"]]
           (locations issues))
        (pr-str issues))
    (is (= ["map destructuring tail entry at index 0 may be nil"
            "map destructuring tail entry at index 1 must be a Map.Entry, got 42"
            "map destructuring tail entry at index 0 may be nil"]
           (mapv :message (rest issues))))))

(deftest odd-sequence-tails-apply-map-entries-in-order-across-namespaces
  (let [api (parse/parse-string
             "options.clj"
             "(ns options (:require [clojure.spec.alpha :as s]))
(s/def ::n int?)
(s/def :text/n string?)
(s/fdef numbers :args (s/cat) :ret (s/keys :req-un [::n]))
(defn numbers [] {:n 4})
(s/fdef texts :args (s/cat) :ret (s/keys :req-un [:text/n]))
(defn texts [] {:n \"bad\"})")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [options :as options] [clojure.spec.alpha :as s]))
(s/fdef amount :args (s/cat) :ret int?)
(defn amount []
  (let [{:keys [n]} (list :n \"old\" (list (find (options/texts) :n) (find (options/numbers) :n)))]
    (inc n)))
(let [{:keys [n]} (list :n 2 (list (find (options/numbers) :n) (find (options/texts) :n)))]
  (inc n))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 7 "TSP2345"]] (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest odd-sequence-tails-use-the-source-map-of-seq
  (let [issues (diagnostics "(let [{:keys [n]} (list :n \"old\" (seq {:n 4 :other 3}))] (inc n))
(let [{:keys [n]} (list :n 2 (seq {:other \"unrelated\"}))] (inc n))
(let [{:keys [n]} (list :n 2 (seq (seq {})))] (inc n))
(let [{:keys [n]} (list :n 2 (next (seq {:n \"removed\"})))] (inc n))
(let [{:keys [n]} (list :n 2 (rest (seq {:n \"removed\"})))] (inc n))
(let [{:keys [n]} (list :n 2 (seq {:n \"bad\"}))] (inc n))
(let [{:keys [n] :or {n 0}} (list :n 2 (seq {:n nil}))] (inc n))
(let [{:keys [n]} (list :n \"old\" (conj (seq {:other 2}) (find {:n 4} :n)))] (inc n))
(let [{:keys [n]} (seq {:n \"bad\" :other 4})] (inc n))")]
    (is (= [[7 "TSP2345"] [8 "TSP2531"] [9 "TSP210-01"] [10 "TSP210-01"]]
           (locations issues))
        (pr-str issues))))

(deftest odd-sequence-tails-use-open-map-contracts-across-namespaces
  (let [api (parse/parse-string
             "options.clj"
             "(ns options (:require [clojure.spec.alpha :as s]))
(s/def ::n int?)
(s/def ::amount int?)
(s/def :text/n string?)
(s/fdef numbers :args (s/cat) :ret (s/keys :req-un [::n] :opt-un [::amount]))
(defn numbers [] {:n 4})
(s/fdef texts :args (s/cat) :ret (s/keys :req-un [:text/n]))
(defn texts [] {:n \"bad\" :extra \"text\"})")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [options :as options] [clojure.spec.alpha :as s]))
(s/fdef amount :args (s/cat) :ret int?)
(defn amount []
  (let [entries (seq (options/numbers))
        {:keys [n amount]} (list :n \"old\" :amount 2 (seq entries))]
    (+ n amount)))
(let [{:keys [n]} (list :n 2 (seq (options/texts)))] (inc n))
(let [{:keys [extra]} (list :n 2 :extra 0 (seq (options/texts)))] (inc extra))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 7 "TSP2345"] ["client.clj" 8 "TSP210-01"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest odd-sequence-tails-use-enumerated-map-of-keys-across-namespaces
  (let [api (parse/parse-string
             "options.clj"
             "(ns options (:require [clojure.spec.alpha :as s]))
(s/def ::number-key #{:n :limit})
(s/fdef numbers :args (s/cat) :ret (s/map-of ::number-key int?))
(defn numbers [] {:n 4})
(s/fdef nullable :args (s/cat) :ret (s/map-of #{:n} (s/nilable int?)))
(defn nullable [] {:n nil})")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [options :as options] [clojure.spec.alpha :as s]))
(s/fdef amount :args (s/cat) :ret int?)
(defn amount []
  (let [entries (seq (options/numbers))
        {:keys [n limit] :or {limit 0} :as all} (list :n 2 entries)]
    (+ n limit (get all :limit 0))))
(let [{:keys [n] :or {n 0}} (list :n 2 (options/nullable))] (inc n))
(let [{:keys [limit]} (list :n 2 (seq (options/numbers)))] (inc limit))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 7 "TSP2531"] ["client.clj" 8 "TSP2531"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest odd-sequence-tails-with-broad-map-of-keys-check-overwrites
  (let [issues (diagnostics "(s/fdef strings :args (s/cat) :ret (s/map-of string? string?))
(defn strings [] {\"n\" \"replacement\"})
(let [{:keys [n] :as all} (list :n 2 (seq (strings)))] (+ n (get all :n)))
(let [{:strs [n]} (list \"n\" 2 (seq (strings)))] (inc n))
(let [{:strs [n]} (list \"n\" \"old\" (strings))] (.length n))")]
    (is (= [[5 "TSP2345"]] (locations issues))
        (pr-str issues))))

(deftest odd-sequence-tails-retain-broad-map-of-values-across-namespaces
  (let [api (parse/parse-string
             "options.clj"
             "(ns options (:require [clojure.spec.alpha :as s]))
(s/fdef counts :args (s/cat) :ret (s/map-of string? int?))
(defn counts [] {\"amount\" 4})
(s/fdef texts :args (s/cat) :ret (s/map-of string? string?))
(defn texts [] {\"amount\" \"bad\"})")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [options :as options] [clojure.spec.alpha :as s]))
(s/fdef amount :args (s/cat :key string?) :ret int?)
(defn amount [key]
  (let [{:strs [amount] :or {amount 0} :as all} (list :tag :count (seq (options/counts)))]
    (+ amount (get all \"amount\" 0) (get all key 0) (get-in all [key] 0))))
(let [{:strs [amount] :or {amount 0}} (list :tag :count (options/texts))] (inc amount))
(let [{:strs [amount]} (list :tag :count (options/counts))] (inc amount))
(let [{:as all} (list :tag :count (seq (options/texts)))] (inc (get all \"amount\" 0)))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 6 "TSP2345"] ["client.clj" 7 "TSP2531"] ["client.clj" 8 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest map-of-tails-check-additional-entries-at-contract-boundaries
  (let [issues (diagnostics "(s/def ::amount int?)
(s/fdef accept-keys :args (s/cat :values (s/keys :opt-un [::amount])) :ret any?)
(defn accept-keys [values] values)
(s/fdef accept-counts :args (s/cat :values (s/map-of keyword? int?)) :ret any?)
(defn accept-counts [values] values)
(s/fdef counts :args (s/cat) :ret (s/map-of keyword? int?))
(defn counts [] {:amount 4})
(s/fdef texts :args (s/cat) :ret (s/map-of keyword? string?))
(defn texts [] {:amount \"bad\"})
(let [{:as all} (list :seed 2 (counts))]
  (accept-counts all)
  (accept-keys all))
(let [{:as all} (list :seed 2 (texts))]
  (accept-counts all)
  (accept-keys all)
  (accept-keys (assoc all :amount 4))
  (accept-keys (dissoc all :amount))
  (inc (get (dissoc all :amount) :amount 0))
  (inc (get (into all [[:amount 4]]) :amount 0)))")]
    (is (= [[15 "TSP2345"] [16 "TSP2345"] [20 "TSP210-01"]] (locations issues))
        (pr-str issues))))

(deftest contains-refines-map-of-tails-across-namespaces
  (let [api (parse/parse-string
             "options.clj"
             "(ns options (:require [clojure.spec.alpha :as s]))
(s/fdef counts :args (s/cat) :ret (s/map-of string? int?))
(defn counts [] {\"n\" 4})
(s/fdef keyword-counts :args (s/cat) :ret (s/map-of keyword? int?))
(defn keyword-counts [] {:n 4})
(s/fdef nullable :args (s/cat) :ret (s/map-of string? (s/nilable int?)))
(defn nullable [] {\"n\" nil})")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [options :as options] [clojure.spec.alpha :as s]))
(s/fdef amount :args (s/cat) :ret int?)
(defn amount []
  (let [{:strs [n] :as all} (list :tag :count (seq (options/counts)))]
    (if (contains? all \"n\") (+ n (get all \"n\")) 0)))
(let [{:keys [n] :as all} (list :tag :count (options/keyword-counts))]
  (if (contains? all :n) (inc n) (get all :n 0)))
(let [{:strs [n] :as all} (list :tag :count (options/nullable))]
  (if (contains? all \"n\") (inc n) 0))
(let [{:strs [n] :as all} (list :tag :count (options/nullable))]
  (if (and (contains? all \"n\") (some? (get-in all [\"n\"]))) (+ n (get all \"n\")) 0))
(let [{:as all} (list :tag :count (seq (options/counts)))]
  (if (contains? all :unexpected) (inc \"unreachable\") 0))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 9 "TSP2531"]] (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))
    (is (= "n may be nil" (:message (first issues))))))

(deftest contains-refines-absence-of-an-indexed-tail-key
  (let [issues (diagnostics "(s/fdef counts :args (s/cat) :ret (s/map-of string? int?))
(defn counts [] {})
(let [{:strs [n] :as all} (list :tag :count (seq (counts)))]
  (if (contains? all \"n\") (inc n) (if (nil? n) 0 (inc \"unreachable\"))))
(let [{:strs [n] :as all} (list :tag :count (counts))]
  (when-not (contains? all \"n\") (inc n)))")]
    (is (= [[7 "TSP2531"]] (locations issues)) (pr-str issues))))

(deftest some-refines-fields-from-map-of-tails-across-namespaces
  (let [api (parse/parse-string
             "options.clj"
             "(ns options (:require [clojure.spec.alpha :as s]))
(s/fdef nullable :args (s/cat) :ret (s/map-of string? (s/nilable int?)))
(defn nullable [] {\"n\" nil})
(s/def ::n int?)
(s/fdef nested :args (s/cat) :ret (s/map-of string? (s/keys :req-un [::n])))
(defn nested [] {\"value\" {:n 4}})")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [options :as options] [clojure.spec.alpha :as s]))
(s/fdef total :args (s/cat) :ret int?)
(defn total []
  (let [{:strs [n] :as all} (list :tag :count (seq (options/nullable)))]
    (if (some? (get all \"n\")) (+ n (get all \"n\")) 0)))
(let [{:strs [n] :as all} (list :tag :count (options/nullable))]
  (if (some? n) (inc (get all \"n\")) 0))
(let [{{:keys [n]} \"value\" :as all} (list :tag :count (seq (options/nested)))]
  (if (some? (get-in all [\"value\" :n])) (+ n (get-in all [\"value\" :n])) 0))
(let [{:as all} (list :tag :count (options/nullable))]
  (if (nil? (get all \"n\")) (inc (get all \"n\" 0)) 0))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 11 "TSP2531"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest map-of-tail-value-guards-distinguish-absence-and-false
  (let [issues (diagnostics "(s/fdef counts :args (s/cat) :ret (s/map-of string? int?))
(defn counts [] {})
(let [{:as all} (list :tag :count (seq (counts)))]
  (if (nil? (get all \"n\")) (inc (get all \"n\" 0)) (inc (get all \"n\"))))
(s/fdef flags :args (s/cat) :ret (s/map-of string? #{false}))
(defn flags [] {\"enabled\" false})
(let [{:as all} (list :tag :flags (flags))]
  (if (some? (get all \"enabled\")) (inc (get all \"enabled\")) 0))")]
    (is (= [[9 "TSP2345"]] (locations issues)) (pr-str issues))))

(deftest some-bindings-refine-map-of-tails-across-namespaces
  (let [api (parse/parse-string
             "options.clj"
             "(ns options (:require [clojure.spec.alpha :as s]))
(s/fdef counts :args (s/cat) :ret (s/map-of string? int?))
(defn counts [] {\"n\" 4})
(s/fdef nullable :args (s/cat) :ret (s/map-of string? (s/nilable int?)))
(defn nullable [] {\"n\" nil})
(s/fdef flags :args (s/cat) :ret (s/map-of keyword? #{false}))
(defn flags [] {:enabled false})")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [options :as options] [clojure.spec.alpha :as s]))
(s/fdef total :args (s/cat) :ret int?)
(defn total []
  (let [{:strs [n] :as all} (list :tag :count (seq (options/counts)))]
    (if-some [value (get all \"n\")] (+ value n (get all \"n\")) (inc (get all \"n\" 0)))))
(let [{:strs [n] :as all} (list :tag :count (options/nullable))]
  (when-some [value n] (+ value (get all \"n\"))))
(let [{:as all} (list :tag :count (options/nullable))]
  (if-some [value (get all \"n\")] (inc value) (inc (get all \"n\" 0))))
(let [{:keys [enabled] :as all} (list :tag :flags (options/flags))]
  (when-some [value (get all :enabled)] (inc enabled)))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 9 "TSP2531"] ["client.clj" 11 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))
    (is (= "false is not assignable to number?" (:message (second issues))))))

(deftest some-binding-with-a-default-does-not-prove-key-presence
  (let [issues (diagnostics "(s/fdef counts :args (s/cat) :ret (s/map-of string? int?))
(defn counts [] {})
(let [{:as all} (list :tag :count (counts))]
  (if-some [n (get all \"n\" 0)] (inc (get all \"n\")) (inc \"unreachable\")))")]
    (is (= [[5 "TSP2531"]] (locations issues)) (pr-str issues))))

(deftest truth-bindings-refine-map-of-tails-across-namespaces
  (let [api (parse/parse-string
             "options.clj"
             "(ns options (:require [clojure.spec.alpha :as s]))
(s/fdef counts :args (s/cat) :ret (s/map-of string? (s/or :count int? :disabled #{false})))
(defn counts [] {\"n\" 4})
(s/fdef nullable :args (s/cat) :ret (s/map-of string? (s/nilable int?)))
(defn nullable [] {\"n\" nil})")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [options :as options] [clojure.spec.alpha :as s]))
(s/fdef total :args (s/cat) :ret int?)
(defn total []
  (let [{:strs [n] :as all} (list :tag :count (seq (options/counts)))]
    (if-let [value (get all \"n\")] (+ value n (get all \"n\")) 0)))
(let [{:strs [n] :as all} (list :tag :count (options/counts))]
  (when-let [value n] (+ value (get all \"n\"))))
(let [{:as all} (list :tag :count (options/counts))]
  (if-let [value (get all \"n\")] (inc value) (inc (get all \"n\" 0))))
(let [{:as all} (list :tag :count (options/nullable))]
  (if-let [value (get all \"n\")] (inc value) (inc (get all \"n\" 0))))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 9 "TSP2345"] ["client.clj" 11 "TSP2531"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))
    (is (str/includes? (:message (first issues)) "false"))))

(deftest truth-binding-with-a-default-does-not-prove-key-presence
  (let [issues (diagnostics "(s/fdef counts :args (s/cat) :ret (s/map-of string? int?))
(defn counts [] {})
(let [{:as all} (list :tag :count (counts))]
  (when-let [n (get all \"n\" 0)] (inc (get all \"n\"))))")]
    (is (= [[5 "TSP2531"]] (locations issues)) (pr-str issues))))

(deftest conditional-bindings-share-field-narrowing-across-namespaces
  (let [api (parse/parse-string
             "options.clj"
             "(ns options (:require [clojure.spec.alpha :as s]))
(s/fdef values :args (s/cat) :ret (s/map-of string? (s/or :text string? :count int?)))
(defn values [] {\"n\" \"word\"})
(s/fdef size-of :args (s/cat :text string?) :ret int?)
(defn size-of [^String text] (.length text))")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [options :as options] [clojure.spec.alpha :as s]))
(s/fdef total :args (s/cat) :ret int?)
(defn total []
  (let [{:strs [n] :as all} (list :tag :count (seq (options/values)))]
    (if-some [value (get all \"n\")]
      (if (string? value) (+ (options/size-of n) (options/size-of (get all \"n\"))) (inc n))
      0)))
(let [{:strs [n] :as all} (list :tag :count (options/values))]
  (when-some [value n] (when (string? value) (options/size-of (get all \"n\")))))
(let [{:strs [n] :as all} (list :tag :count (options/values))]
  (if-let [value (get all \"n\")] (if (string? value) (options/size-of n) (inc n)) 0))
(let [{:strs [n] :as all} (list :tag :count (options/values))]
  (when-let [value n] (when (string? value) (options/size-of (get all \"n\")))))
(let [{:as all} (list :tag :count (options/values))]
  (when-some [value (get all \"n\")] (when (string? value) (inc (get all \"n\")))))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 15 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))
    (is (= "string? is not assignable to number?" (:message (first issues))))))

(deftest conditional-bindings-keep-proofs-about-the-bound-value
  (let [issues (diagnostics "(s/def ::n int?)
(s/def ::flag boolean?)
(s/fdef open-options :args (s/cat) :ret (s/keys :opt-un [::flag]))
(defn open-options [] {})
(let [opts (open-options)]
  (if-some [n (::n opts)] (if (contains? opts :flag) (inc n) 0) 0))
(s/fdef counts :args (s/cat) :ret (s/map-of string? int?))
(defn counts [] {})
(let [{:strs [n] :as all} (list :tag :count (counts))]
  (when-some [value (get all \"n\" 0)] (when (int? value) (inc n))))")]
    (is (= [[11 "TSP2531"]] (locations issues)) (pr-str issues))))

(deftest quoted-symbol-keys-share-field-narrowing-across-namespaces
  (let [api (parse/parse-string
             "options.clj"
             "(ns options (:require [clojure.spec.alpha :as s]))
(s/fdef values :args (s/cat) :ret (s/map-of symbol? (s/or :text string? :count int?)))
(defn values [] {'n \"word\"})
(s/fdef counts :args (s/cat) :ret (s/map-of symbol? int?))
(defn counts [] {'fixture/n 4})
(s/fdef nullable :args (s/cat) :ret (s/map-of symbol? (s/nilable int?)))
(defn nullable [] {'n nil})
(s/fdef size-of :args (s/cat :text string?) :ret int?)
(defn size-of [^String text] (.length text))")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [options :as options] [clojure.spec.alpha :as s]))
(s/fdef total :args (s/cat) :ret int?)
(defn total []
  (let [{:syms [n] :as all} (list :tag :count (seq (options/values)))]
    (if-some [value (get all 'n)]
      (if (string? value) (+ (options/size-of n) (options/size-of (get-in all ['n]))) (inc n))
      0)))
(let [{:syms [fixture/n] :as all} (list :tag :count (options/counts))]
  (if (contains? all (quote fixture/n)) (+ n (get all 'fixture/n)) 0))
(let [{:syms [fixture/n] :as all} (list :tag :count (seq (options/counts)))]
  (if-some [value (get-in all [(quote fixture/n)])] (+ n value (get all 'fixture/n)) 0))
(let [{:as all} (list :tag :count (options/nullable))]
  (inc (get all 'n 0)))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 13 "TSP2531"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest quoted-symbol-reads-distinguish-key-kind-and-namespace
  (let [issues (diagnostics "(inc (get {'n 4 :n \"keyword\" \"n\" nil} 'n))
(inc (get {\"n\" nil} 'n 0))
(inc (get-in {'fixture/n {'n 4}} [(quote fixture/n) 'n]))
(inc (get {'n 4 'fixture/n \"bad\"} 'fixture/n))")]
    (is (= [[5 "TSP2345"]] (locations issues)) (pr-str issues))))

(deftest quoted-symbol-map-edits-retain-shapes-across-namespaces
  (let [api (parse/parse-string
             "options.clj"
             "(ns options (:require [clojure.spec.alpha :as s]))
(s/fdef stamp :args (s/cat :options map?) :ret map?)
(defn stamp [options] (assoc options 'fixture/n 4))
(s/fdef discard :args (s/cat :options map?) :ret map?)
(defn discard [options] (-> options (dissoc (quote fixture/n))))
(s/fdef counts :args (s/cat) :ret (s/map-of symbol? int?))
(defn counts [] {'fixture/n 4})")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [options :as options] [clojure.spec.alpha :as s]))
(s/fdef total :args (s/cat) :ret int?)
(defn total []
  (let [{:syms [fixture/n] :as all} (options/stamp {'fixture/n \"old\" 'n 2})]
    (+ n (get all 'n))))
(inc (get (options/discard {'fixture/n nil 'n 4}) 'fixture/n 0))
(let [{:as all} (list :tag :count (options/counts))
      {:syms [fixture/n]} (assoc all 'fixture/n \"bad\")]
  (inc n))
(inc (get (assoc {'n 1} (quote fixture/n) nil) 'fixture/n 0))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 9 "TSP2345"] ["client.clj" 10 "TSP2531"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest quoted-symbol-map-edits-distinguish-missing-and-stored-values
  (let [issues (diagnostics "(s/fdef words :args (s/cat) :ret (s/map-of symbol? string?))
(defn words [] {'fixture/n \"old\"})
(let [{:as all} (list :tag :count (seq (words)))]
  (inc (get (dissoc all (quote fixture/n)) 'fixture/n 0)))
(inc (get (assoc {'n nil} 'n \"old\" (quote n) 4) 'n))
(inc (get (dissoc {'n 1 \"n\" \"bad\"} 'n) \"n\"))")]
    (is (= [[7 "TSP2345"]] (locations issues)) (pr-str issues))))

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
