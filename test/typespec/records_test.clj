(ns typespec.records-test
  (:require [clojure.test :refer [deftest is]]
            [typespec.check :as check]
            [typespec.parse :as parse]))

(defn- checked [provider client]
  (check/check-files [(parse/parse-string "provider.cljc" provider)
                      (parse/parse-string "client.clj" client)]
                     {}))

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
