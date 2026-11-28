(ns typespec.protocol-contracts-test
  (:require [clojure.test :refer [deftest is]]
            [typespec.check :as check]
            [typespec.main :as main]
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

(deftest potemkin-protocols-use-method-arities-and-spec
  (let [api (parse/parse-string
             "api.cljc"
             "(ns api (:require [potemkin :as p] [clojure.spec.alpha :as s]))
(#?(:clj p/defprotocol+ :cljs defprotocol) Display
  (render [this] [this prefix])
  (ready? [this]))
(s/fdef render :args (s/cat :this any? :prefix (s/? string?)) :ret string?)")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [api :as a]))
(def display (reify a/Display
  (render [_] \"ready\")
  (render [_ prefix] (str prefix \"ready\"))
  (ready? [_] true)))
(subs (a/render display \"prefix\") 0)
(a/render display 42)
(a/ready? display)
(a/ready? display 42)
(reify a/Display
  (render [_] 42)
  (render [_ prefix] (str prefix \"ready\"))
  (ready? [_] true))")
        ds (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 5 "TSP430-03"] ["client.clj" 7 "TSP2345"] ["client.clj" 9 "TSP2554"]
            ["client.clj" 11 "TSP2322"] ["client.clj" 13 "TSP430-03"]]
           (mapv (juxt :file (comp :row :pos) :code) ds))
        (pr-str ds))))

(deftest checks-literal-extend-methods-including-a-forname-array
  (let [r (checked "(extend String api/Display
  {:render (fn ([this] this) ([this prefix] (str prefix this)))})
(extend (Class/forName \"[B\") api/Display
  {:render (fn [body] (alength body))})
(extend nil api/Display {:render (fn [_] \"nil\")})")]
    (is (= [[5 "TSP2322"]] (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= [3 5 6] (mapv (comp :row :pos) (filter #(and (= :implementation (:kind %))
                                                          (= 'catalog.display/render (:var %))) (:uses r)))))))

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

(deftest return-only-protocol-spec-applies-to-each-arity
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(defprotocol Display (describe [this] [this prefix]))
(s/fdef describe :ret string?)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a] [clojure.string :as str]))
(def good
  (reify a/Display
    (describe [this] \"ready\")
    (describe [this prefix] (str prefix \"ready\"))))
(def wrong
  (reify a/Display
    (describe [this] 42)
    (describe [this prefix] (str prefix \"ready\"))))
(str/upper-case (a/describe good))
(inc (a/describe good \"current: \"))
(extend-type Long a/Display
  (describe ([this] (str this)) ([this prefix] this)))")
        diagnostics (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 8 "TSP2322"] ["client.clj" 11 "TSP2345"] ["client.clj" 13 "TSP2322"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics))))

(deftest extend-protocol-checks-each-receiver-and-the-shared-spec
  (let [r (checked "(clojure.core/extend-protocol api/Display
  String
  (api/render [this prefix] (str (subs this 1) (inc prefix)))
  java.time.LocalDate
  (catalog.display/render [this] (subs this 1))
  nil
  (render [this] (if (nil? this) \"empty\" 42))
  Long
  (render [this] this))")]
    (is (= [[4 "TSP2345"] [6 "TSP2345"] [10 "TSP2322"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))
    (is (= ["nil?" "nil?"]
           [(main/type-at r "client.clj" 2 1) (main/type-at r "client.clj" 8 12)]))
    (is (= (mapv #(vector 'catalog.display/render :implementation %) [4 6 8 10])
           (mapv (juxt :var :kind (comp :row :pos)) (:uses r))))))

(deftest extend-protocol-checks-method-identities-and-arities-per-class
  (let [r (checked "(extend-protocol api/Display
  String
  (api/render ([this] this) ([this prefix] prefix))
  (render [this] this)
  (render [this x y] this)
  (other [this] this)
  Long
  (render [this prefix] prefix))")]
    (is (= [[5 "TSP430-01"] [6 "TSP430-01"] [7 "TSP430-01"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics r)))
        (pr-str (:diagnostics r)))))

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
