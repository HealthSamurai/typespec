(ns typespec.reader-test
  (:require [clojure.test :refer [deftest is]]
            [typespec.check :as check]
            [typespec.parse :as parse]))

(deftest tagged-literals-stay-opaque
  (let [source "(vector #js [(inc \"inside\")] #inst \"2020-01-01\" #sql/inline \"SELECT 1\" (inc \"bad\"))"
        file (parse/parse-string "tagged.cljc" source)
        args (rest (get-in file [:forms 0 :children]))]
    (is (= [:opaque :opaque :opaque :list] (mapv :op args)))
    (is (= [9 30 49] (mapv (comp :col :pos) (take 3 args))))
    (is (= [[1 77 "TSP2345"]]
           (mapv (juxt (comp :row :pos) (comp :col :pos) :code)
                 (:diagnostics (check/check-files [file] {})))))))

(deftest clojure-reader-branches-use-source-order-and-splice
  (let [source "[#?(:default :first :clj :second)
                  #?(:cljs :absent)
                  #?(:clj #_:discarded :kept)
                  #?@(:clj [:vector #?@(:clj [:nested])])
                  #?@(:default (:list :last))
                  #?@(:cljs [:absent])]
                #?(:cljs (inc \"skipped\"))
                #?(:clj (inc \"bad\") :default (inc 1))"
        {:keys [forms] :as file} (parse/parse-string "branches.cljc" source)]
    (is (= (read-string {:read-cond :allow} source)
           (parse/node->data (first forms))))
    (is (= 2 (count forms)))
    (is (= [[8 30 "TSP2345"]]
           (mapv (juxt (comp :row :pos) (comp :col :pos) :code)
                 (:diagnostics (check/check-files [file] {})))))))

(deftest spliced-namespace-clauses-and-calls-use-cross-file-contracts
  (let [api (parse/parse-string
             "api.cljc"
             "(ns reader.api (:require #?@(:clj [[clojure.spec.alpha :as s]]
                                           :cljs [[cljs.spec.alpha :as s]])))
              #?(:clj (s/fdef echo :args (s/cat :x string?) :ret string?))
              #?(:cljs (defn echo [] nil) :clj (defn echo [x] x))")
        use (parse/parse-string
             "client.clj"
             "(ns reader.client #?@(:clj [(:require [reader.api :as api :refer [echo]])
                                         (:import [java.time Instant])]))
              (api/echo #?@(:cljs [42] :clj [\"ok\"]))
              (echo #?@(:clj [42]))
              (Instant/parse \"2020-01-01T00:00:00Z\")")
        ds (:diagnostics (check/check-files [api use] {}))]
    (is (= {'s 'clojure.spec.alpha} (:aliases api)))
    (is (= {'Instant "java.time.Instant"} (:imports use)))
    (is (= [["client.clj" 4 31 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) (comp :col :pos) :code) ds))
        (pr-str ds))))
