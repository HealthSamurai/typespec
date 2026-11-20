(ns typespec.protocol-contracts-test
  (:require [clojure.test :refer [deftest is]]
            [typespec.check :as check]
            [typespec.parse :as parse]
            [typespec.project-contracts :as project-contracts]))

(def ^:private provider
  "(ns catalog.display (:require [clojure.spec.alpha :as s]))
(defprotocol Display
  (render [this] [this prefix]))
(s/fdef render :args (s/cat :this any? :prefix (s/? string?)) :ret string?)")

(defn- checked [consumer]
  (check/check-files [(parse/parse-string "client.clj" (str "(ns client (:require [catalog.display :as api]))\n" consumer))
                      (parse/parse-string "display.clj" provider)]
                     {:track-uses true}))

(deftest checks-reify-and-extensions-against-the-same-method-spec
  (let [r (checked "(let [captured 42]
  (reify api/Display
    (render [_] \"ok\")
    (render [_ prefix] captured)))
(extend-type String
  api/Display
  (render ([this] this) ([this prefix] (subs prefix 1))))
(api/render nil 42)
(reify java.util.Comparator (compare [_ a b] \"opaque Java body\"))")]
    (is (= [[5 "TSP2322"] [9 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r))) (pr-str (:diagnostics r)))
    (is (= 'catalog.display/Display (get-in r [:env :vars 'catalog.display/render :protocol])))
    (is (= [[:implementation 4] [:implementation 5] [:implementation 8] [:call 9]]
           (mapv (juxt :kind (comp :row :pos)) (:uses r))))
    (is (= ["catalog.display/render" "catalog.display/Display"]
           ((juxt :var :protocol) (first (project-contracts/index r identity)))))))

(deftest extension-bodies-see-the-class-of-the-receiver
  (let [r (checked "(extend-type Long
  api/Display
  (render ([this] this) ([this prefix] prefix)))")]
    (is (= ["TSP2322"] (mapv :code (:diagnostics r))) (pr-str (:diagnostics r)))
    (is (re-find #"int\?" (get-in r [:diagnostics 0 :message])))))

(deftest diagnoses-missing-duplicate-and-unknown-method-arities
  (let [r (checked "(reify api/Display
  (render [_] \"ok\")
  (render [_] \"duplicate\")
  (render [_ prefix suffix] \"bad arity\")
  (other [_] \"unknown method\"))")]
    (is (= ["TSP430-02" "TSP430-01" "TSP430-01" "TSP430-01"]
           (mapv :code (:diagnostics r))) (pr-str (:diagnostics r)))))

(deftest untyped-protocol-methods-still-have-declared-arities
  (let [f (parse/parse-string "untyped.clj" "(ns untyped)
(defprotocol P (call-it [this]))
(reify P (call-it [_] 42))
(call-it nil 1)")
        r (check/check-files [f] {})]
    (is (= ["TSP430-03" "TSP2554"] (mapv :code (:diagnostics r))) (pr-str (:diagnostics r)))))

(deftest rejects-a-method-spec-that-invents-an-arity
  (let [f (parse/parse-string "wrong.clj" "(ns wrong (:require [clojure.spec.alpha :as s]))
(defprotocol P (call-it [this]))
(s/fdef call-it :args (s/cat :this any? :extra int?) :ret int?)
(call-it nil 1)")
        r (check/check-files [f] {})]
    (is (= [["wrong.clj" "TSP430-01"] ["wrong.clj" "TSP2554"]]
           (mapv (juxt :file :code) (:diagnostics r))) (pr-str (:diagnostics r)))))
