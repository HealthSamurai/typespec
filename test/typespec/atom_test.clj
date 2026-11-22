(ns typespec.atom-test
  (:require [clojure.test :refer [deftest is]]
            [typespec.check :as check]
            [typespec.main :as main]
            [typespec.parse :as parse]
            [typespec.types :as t]))

(defn- checked [source]
  (check/check-files [(parse/parse-string "state.clj"
                       (str "(ns state (:require [clojure.spec.alpha :as s]))\n" source))] {}))

(defn- locations [result]
  (mapv (juxt :file (comp :row :pos) :code) (:diagnostics result)))

(deftest atom-constructor-uses-supplied-content-type
  (let [result (checked "(s/def ::cache (s/map-of keyword? string?))
(s/fdef make-cache :args (s/cat) :ret (t/atom ::cache))
(defn make-cache [] (atom {}))
(s/fdef bad-cache :args (s/cat) :ret (t/atom ::cache))
(defn bad-cache [] (atom {:title 42}))
(s/fdef read-count :args (s/cat :cell (t/atom number?)) :ret number?)
(defn read-count [cell] @cell)
(read-count (atom 1))
(let [cell (atom 1)] (read-count cell))")]
    (is (= [["state.clj" 6 "TSP2345"] ["state.clj" 10 "TSP2345"]]
           (locations result))
        (pr-str (:diagnostics result))))
  (let [errors (atom [])
        ctx {:fail #(swap! errors conj %)}
        ty (t/parse-type ctx '(t/atom (s/nilable int?)))
        integer (t/parse-type ctx '(t/atom int?))
        number (t/parse-type ctx '(t/atom number?))]
    (is (= "(t/atom (s/nilable int?))" (t/show ty)))
    (is (= [:proved :mismatch :mismatch]
           (mapv (comp :status #(apply t/contract-sub-result %))
                 [[integer integer] [integer number] [number integer]])))
    (t/parse-type ctx '(t/atom int? string?))
    (is (= ['(t/atom int? string?)] @errors))))

(deftest atom-references-retain-invariant-types-across-namespaces
  (let [provider (parse/parse-string "cache.clj"
                   "(ns cache (:require [clojure.spec.alpha :as s]))
(defonce store (atom {}))
(s/def ::content (s/map-of keyword? string?))
(s/fdef make-typed :args (s/cat) :ret (t/atom ::content))
(defn make-typed [] (atom {}))")
        client (parse/parse-string "client.clj"
                 "(ns client (:require [cache :as c] [clojure.spec.alpha :as s]))
(s/fdef put :args (s/cat :cell (t/atom (s/map-of keyword? string?))) :ret map?)
(defn put [cell] (swap! cell assoc :name \"value\"))
(put (c/make-typed))
(put c/store)
(swap! c/store assoc :name \"value\")
(let [alias c/store] (swap! alias (fn [_] 42)))
(deref c/store)
@c/store")
        result (check/check-files [client provider] {})]
    (is (= [["client.clj" 5 "TSP2345"] ["client.clj" 7 "TSP2345"]]
           (locations result))
        (pr-str (:diagnostics result)))
    (is (= ["map?" "map?"] (mapv #(main/type-at result "client.clj" % 1) [8 9])))
    (is (= (:diagnostics result) (:diagnostics (check/check-files [provider client] {}))))))

(deftest atom-writes-check-aliases-and-update-functions
  (let [r (checked "(let [cell (atom 0), alias cell]
  (reset! alias 3)
  (reset! alias \"wrong\")
  (swap! cell inc)
  (swap! cell (fn [n x] (+ n x)) 2)
  (swap! cell (fn [_] \"wrong\"))
  (swap! cell (fn [n] n) 1))
(s/fdef update-cache :args (s/cat :cell (t/atom (s/map-of keyword? string?))) :ret any?)
(defn update-cache [cell] (swap! cell assoc :name 42))
(s/fdef update-inline :args (s/cat :cell (t/atom (s/map-of keyword? string?))) :ret any?)
(defn update-inline [cell] (swap! cell (fn [m] (assoc m :name 42))))
(let [cell (atom [1])] (swap! cell assoc 0 \"wrong\"))
(let [swap! str] (swap! (atom 0) \"local\"))")]
    (is (= [["state.clj" 4 "TSP2345"] ["state.clj" 7 "TSP2345"]
            ["state.clj" 8 "TSP2554"] ["state.clj" 10 "TSP2345"]
            ["state.clj" 12 "TSP2345"] ["state.clj" 13 "TSP2345"]]
           (locations r))
        (pr-str (:diagnostics r)))))

(deftest dereference-narrowing-applies-to-a-snapshot
  (let [r (checked "(s/def ::content (s/or :text string? :count int?))
(s/fdef snapshot :args (s/cat :cell (t/atom ::content)) :ret string?)
(defn snapshot [cell]
  (let [old @cell]
    (if (string? old) (do (reset! cell 42) (subs old 1)) \"\")))
(s/fdef reread :args (s/cat :cell (t/atom ::content)) :ret string?)
(defn reread [cell]
  (if (string? @cell) (subs @cell 1) \"\"))
(s/fdef update-cell :args (s/cat :cell (t/atom ::content)) :ret any?)
(defn update-cell [cell] (swap! cell inc))
(s/fdef maybe-atom :args (s/cat :cell (s/nilable (t/atom int?))) :ret any?)
(defn maybe-atom [cell] @cell)")]
    (is (= [["state.clj" 9 "TSP2345"] ["state.clj" 11 "TSP2345"] ["state.clj" 13 "TSP2531"]]
           (locations r))
        (pr-str (:diagnostics r)))))

(deftest atom-value-pairs-and-compare-and-set-use-the-write-contract
  (let [r (checked "(s/fdef replace-count :args (s/cat :cell (t/atom int?)) :ret (s/tuple int? #{7}))
(defn replace-count [cell] (reset-vals! cell 7))
(s/fdef increment-count :args (s/cat :cell (t/atom int?)) :ret (s/tuple int? int?))
(defn increment-count [cell] (swap-vals! cell + 7))
(s/fdef compare-count :args (s/cat :cell (t/atom int?)) :ret boolean?)
(defn compare-count [cell] (compare-and-set! cell :different 7))
(let [cell (atom 0), alias cell]
  (reset-vals! alias \"bad\")
  (swap-vals! alias (fn [_] \"bad\"))
  (compare-and-set! alias 0 \"bad\"))
(swap-vals! (atom 1) (fn [n] n) 1)
(compare-and-set! (atom 1) 1)
(reset-vals! 42 {})
(let [compare-and-set! str] (compare-and-set! \"custom\"))
(s/fdef abort-update :args (s/cat :cell (t/atom int?)) :ret string?)
(defn abort-update [cell] (swap-vals! cell (fn [_] (throw (Exception.)))))")]
    (is (= [["state.clj" 9 "TSP2345"] ["state.clj" 10 "TSP2345"] ["state.clj" 11 "TSP2345"]
            ["state.clj" 12 "TSP2554"] ["state.clj" 13 "TSP2554"] ["state.clj" 14 "TSP2345"]]
           (locations r))
        (pr-str (:diagnostics r)))))
