(ns typespec.records-test
  (:require [clojure.test :refer [deftest is]]
            [typespec.check :as check]
            [typespec.parse :as parse]))

(defn- checked [provider client]
  (check/check-files [(parse/parse-string "provider.cljc" provider)
                      (parse/parse-string "client.clj" client)]
                     {}))

(deftest map-record-factories-check-the-exact-primitive-boxes
  (let [result (checked
                "(ns records.provider (:require [clojure.spec.alpha :as s]))
(defrecord Reading [^long n ^double rate label])
(defrecord Tagged [^long n])
(s/fdef map->Tagged :ret map?)
(s/def ::n number?)
(s/def ::rate number?)
(s/def ::args (s/keys :req-un [::rate] :opt-un [::n]))
(s/fdef make-record :args (s/cat :m ::args) :ret map?)
(defn make-record [m] (map->Reading m))"
                "(ns records.client (:require [records.provider :as a]))
(inc (:n (a/map->Reading {:n 2 :rate 1.0})))
(a/map->Reading {:rate 1.0})
(a/map->Reading {:n \"bad\" :rate 1.0})
(a/map->Reading {:n 1 :rate nil})
(a/map->Reading nil)
(a/map->Reading [[:n 1] [:rate false]])
(a/map->Tagged {:n 1})
(a/map->Tagged {:n nil})
(a/map->Reading {:n 2N :rate 1.0})
(a/map->Reading {:n 2 :rate 1})
(inc (:rate (a/map->Reading [[:n 2] [:rate 1.0]])))")
        issues (:diagnostics result)]
    (is (= (frequencies [["client.clj" 3 "TSP2531" [:n]] ["client.clj" 4 "TSP2345" [:n]]
                         ["client.clj" 5 "TSP2531" [:rate]] ["client.clj" 6 "TSP2531" [:n]]
                         ["client.clj" 6 "TSP2531" [:rate]] ["client.clj" 7 "TSP2345" [:rate]]
                         ["client.clj" 9 "TSP2531" [:n]] ["client.clj" 10 "TSP2345" [:n]]
                         ["client.clj" 11 "TSP2345" [:rate]] ["provider.cljc" 9 "TSP210-01" [:n]]
                         ["provider.cljc" 9 "TSP210-01" [:rate]] ["provider.cljc" 9 "TSP2531" [:n]]])
           (frequencies (map (juxt :file (comp :row :pos) :code :type-path) issues)))
        (pr-str issues))))

(deftest map-record-factories-check-sequences-of-entries
  (let [result (checked
                "(ns records.provider (:require [clojure.spec.alpha :as s]))
(defrecord Row [label n])
(s/def ::entries (s/coll-of (s/tuple #{:n} int?) :kind vector? :min-count 1))
(s/fdef build :args (s/cat :entries ::entries) :ret (s/nilable int?))
(defn build [entries] (:n (map->Row entries)))
(s/fdef accept :args (s/cat :n int?) :ret int?)
(defn accept [n] n)"
                "(ns records.client (:require [records.provider :as a]))
(a/accept (:n (a/map->Row [[:n 42]])))
(subs (:label (a/map->Row (list [:label \"ok\"]))) 0)
(inc (:n (a/map->Row [[:n \"discarded\"] [:n 42]])))
(subs (:n (a/map->Row [[:n \"discarded\"] [:n 42]])) 0)
(inc (:n (a/map->Row [])))
(a/map->Row [[:bad]])
(a/map->Row [[1 2 3]])
(a/map->Row [true])
(a/build [[:n 42]])
(a/build [[:n \"bad\"]])")
        issues (:diagnostics result)]
    (is (= [[5 "TSP2345"] [6 "TSP2531"] [7 "TSP2345"]
            [8 "TSP2345"] [9 "TSP2345"] [11 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) issues))
        (pr-str issues))
    (is (every? #(= "client.clj" (:file %)) issues))))

(deftest positional-record-factories-check-primitive-field-arguments
  (let [result (checked
                "(ns records.provider (:require [clojure.spec.alpha :as s]))
(defrecord Reading [^long n ^double rate ^String label])
(s/fdef build :args (s/cat :n (s/nilable number?)) :ret any?)
(defn build [n] (->Reading n 0.5 \"x\"))
(s/fdef accept :args (s/cat :n int?) :ret int?)
(defn accept [n] n)"
                "(ns records.client (:require [records.provider :as a]))
(a/accept (:n (a/->Reading 1.9 2 42)))
(a/->Reading 1M 2/3 \"ok\")
(a/->Reading \\A 1 \"bad\")
(a/->Reading 1 \\B \"bad\")
(a/->Reading nil 1 \"nil\")
(a/->Reading \"not-number\" 1 \"bad\")
(a/->Reading 1 false \"bad\")
(apply a/->Reading [1 \"wrong\" \"bad\"])
(a/->Reading [1] 1 \"bad\")")
        issues (:diagnostics result)]
    (is (= [["client.clj" 4 "TSP2345"] ["client.clj" 5 "TSP2345"] ["client.clj" 6 "TSP2345"]
            ["client.clj" 7 "TSP2345"] ["client.clj" 8 "TSP2345"] ["client.clj" 9 "TSP2345"]
            ["client.clj" 10 "TSP2345"] ["provider.cljc" 4 "TSP2531"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest map-record-factories-preserve-extra-and-nil-fields-across-files
  (let [result (checked
                "(ns records.provider (:require [clojure.spec.alpha :as s]))
(s/def ::label string?)
(s/def ::n int?)
(s/def ::row-input (s/keys :req-un [::label ::n]))
(s/def ::any-fields (s/map-of string? int?))
(defrecord Row [label n])
(s/fdef pack :args (s/cat :m ::row-input) :ret ::row-input)
(defn pack [m] (map->Row m))
(s/fdef from-index :args (s/cat :m ::any-fields) :ret nil?)
(defn from-index [m] (:label (map->Row m)))"
                "(ns records.client (:require [records.provider :as a]))
(subs (:label (a/map->Row {:label \"ok\" :n 42})) 0)
(inc (:extra (a/map->Row {:extra 42})))
(inc (:label (a/map->Row {:label \"ok\"})))
(inc (:n (a/map->Row {:label \"ok\"}) 0))
(inc (:n (a/map->Row nil)))
(inc (:n (apply a/map->Row [{:n 42}])))
(subs (:label (a/pack {:label \"ok\" :n 42})) 0)
(a/pack {:label \"ok\" :n \"bad\"})
(inc (a/from-index {\"x\" 42}))")
        issues (:diagnostics result)]
    (is (= [[4 "TSP2345"] [5 "TSP2531"] [6 "TSP2531"] [9 "TSP2322"] [10 "TSP2531"]]
           (mapv (juxt (comp :row :pos) :code) issues))
        (pr-str issues))
    (is (every? #(= "client.clj" (:file %)) issues))))

(deftest positional-record-factories-preserve-field-types-across-files
  (let [result (checked
                "(ns records.provider (:require [clojure.spec.alpha :as s]))
(s/def ::label string?)
(s/def ::n int?)
(s/def ::row (s/keys :req-un [::label ::n]))
(defrecord Row [label n])
(defrecord Empty [])
(defrecord Typed [^String label ^long n])
(s/fdef pack :args (s/cat :label string? :n int?) :ret ::row)
(defn pack [label n] (->Row label n))
(s/fdef accept :args (s/cat :row ::row) :ret int?)
(defn accept [{:keys [n]}] n)"
                "(ns records.client (:require [records.provider :as a]))
(subs (:label (a/->Row \"ok\" 42)) 0)
(inc (:label (a/->Row \"ok\" 42)))
(inc (:n (apply a/->Row [\"ok\" 42])))
(inc (:n (a/->Row \"ok\" nil)))
(a/accept (a/->Row \"ok\" 42))
(a/accept (a/->Row 42 \"bad\"))
(subs (:label (a/pack \"ok\" 42)) 0)
(subs (:value (a/->Empty) \"fallback\") 0)
(into-array Long [(:n (a/->Typed 42 1.9))])
(inc (:label (a/->Typed 42 1)))")
        issues (:diagnostics result)]
    (is (= [[3 "TSP2345"] [5 "TSP2531"] [7 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) issues))
        (pr-str issues))
    (is (every? #(= "client.clj" (:file %)) issues))))

(deftest generated-record-factories-are-callable-across-files
  (let [r (checked
           "(ns records.provider)
            (declare ->Entry)
            (defrecord Entry [^String label data])
            (clojure.core/defrecord Empty [])
            (->Entry 42 {})"
           "(ns records.client
                (:require [records.provider :as p :refer [->Entry map->Empty]]))
            (p/->Entry \"ok\" {})
            (->Entry 42 nil)
            (p/map->Entry nil)
            (p/map->Entry [[:label \"ok\"]])
            (p/->Empty)
            (map->Empty {})
            (p/->Entry 1)
            (map->Empty)
            (p/->Empty 1)
            (apply p/->Entry [1])")
        ds (:diagnostics r)]
    (is (= [[9 "TSP2554"] [10 "TSP2554"] [11 "TSP2554"] [12 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) ds))
        (pr-str ds))
    (is (= ["p/->Entry expects 2 argument(s), got 1"
            "map->Empty expects 1 argument(s), got 0"
            "p/->Empty expects 0 argument(s), got 1"]
           (mapv :message (take 3 ds))))
    (is (= {:file "provider.cljc" :source :record}
           (select-keys (get-in r [:env :vars 'records.provider/->Entry]) [:file :source])))))

(deftest explicit-factory-specs-respect-the-generated-arity
  (let [r (checked
           "(ns records.provider (:require [clojure.spec.alpha :as s]))
            (defrecord Entry [label])
            (s/fdef ->Entry :args (s/cat :label string?) :ret map?)
            (s/fdef map->Entry :args (s/cat :m map? :extra any?) :ret map?)"
           "(ns records.client (:require [records.provider :as p]))
            (p/->Entry \"ok\")
            (p/->Entry 42)
            (p/map->Entry {})
            (p/map->Entry {} nil)")
        ds (:diagnostics r)]
    (is (= [["client.clj" 3 "TSP2345"] ["client.clj" 5 "TSP2554"]
            ["provider.cljc" 4 "TSP2790"]]
           (mapv (juxt :file (comp :row :pos) :code) ds))
        (pr-str ds))
    (is (= :fdef (get-in r [:env :vars 'records.provider/->Entry :source])))))

(deftest record-factories-keep-return-only-specs-and-generated-arities
  (let [r (checked "(ns records.provider (:require [clojure.spec.alpha :as s]))
(s/def ::label string?)
(s/def ::entry (s/keys :req-un [::label]))
(defrecord Entry [label])
(s/fdef ->Entry :ret ::entry)
(s/fdef map->Entry :ret ::entry)"
                   "(ns records.client (:require [records.provider :as p] [clojure.string :as str]))
(str/upper-case (:label (p/->Entry \"ok\")))
(inc (:label (p/->Entry \"ok\")))
(str/upper-case (:label (apply p/map->Entry [{:label \"ok\"}])))
(inc (:label (p/map->Entry {:label \"ok\"})))
(p/->Entry)
(p/map->Entry {} {})")
        diagnostics (:diagnostics r)]
    (is (= [["client.clj" 3 "TSP2345"] ["client.clj" 5 "TSP2345"]
            ["client.clj" 6 "TSP2554"] ["client.clj" 7 "TSP2554"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics))))

(deftest explicit-redefinitions-replace-factory-signatures
  (let [r (checked
           "(ns records.provider)
            (defn ->Entry [x y] x)
            (defrecord Entry [label])
            (defn map->Entry [x y] x)
            (defrecord Rebound [label])
            (def ->Rebound (fn [] :ok))"
           "(ns records.client (:require [records.provider :as p]))
            (p/->Entry \"ok\")
            (p/->Entry \"bad\" nil)
            (p/map->Entry {} nil)
            (p/->Rebound)")]
    (is (= [[3 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))))
