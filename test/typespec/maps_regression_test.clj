(ns typespec.maps-regression-test
  (:require [clojure.test :refer [deftest is testing]]
            [typespec.check :as check]
            [typespec.main :as main]
            [typespec.parse :as parse]
            [typespec.types :as t]))

(defn infer-type
  ([source] (infer-type source {}))
  ([source locals]
   (let [file (parse/parse-string "maps-regression.clj" source)]
     (binding [check/*diags* (atom [])]
       (check/infer {:env {} :file file :locals locals :opts {:strict-nil true}}
                    (first (:forms file)))))))

(defn alternatives [ty] (set (t/members {} ty)))
(defn map-type [req opt] {:t :map :req req :opt opt})

(deftest defaults-replace-absence-not-nil
  (doseq [source ["(get m :x 42)" "(:x m 42)"]]
    (testing source
      (is (= t/nil-t (infer-type source {'m (map-type {:x t/nil-t} {})})))
      (is (= #{t/nil-t (t/prim :string) (t/lit 42)}
             (alternatives (infer-type source {'m (map-type {} {:x (t/union t/nil-t (t/prim :string))})}))))
      (is (= (t/lit 42) (infer-type source {'m t/nil-t})))
      (is (= (t/prim :string) (infer-type source {'m (map-type {:x (t/prim :string)} {})})))
      (is (= #{t/nil-t (t/lit 42)}
             (alternatives (infer-type source {'m (t/union t/nil-t (map-type {:x t/nil-t} {}))}))))))
  (is (= t/nil-t (infer-type "(get {:x nil} :x 42)")))
  (is (= t/nil-t (infer-type "(:x {:x nil} 42)"))))

(deftest get-in-default-tracks-absence-across-the-entire-path
  (doseq [[source expected]
          [["(get-in {:a {:b nil}} [:a :b] :missing)" t/nil-t]
           ["(get-in {:a nil} [:a :b] :missing)" (t/lit :missing)]
           ["(get-in {:a {}} [:a :b] :missing)" (t/lit :missing)]
           ["(let [m {}] (get-in m [:a :b] :missing))" (t/lit :missing)]
           ["(get-in {:a 1} [:a :b] :missing)" (t/lit :missing)]
           ["(get-in {:a {}} [:a :b])" t/nil-t]
           ["(get-in nil [] :missing)" t/nil-t]
           ["(get-in :root nil :missing)" (t/lit :root)]
           ["(get-in {:a [nil]} [:a 0] :missing)" t/nil-t]
           ["(get-in {:a [nil]} [:a 1] :missing)" (t/lit :missing)]]]
    (is (= (alternatives expected) (alternatives (infer-type source))) source))
  (let [m (map-type {} {:a (t/union t/nil-t (map-type {} {:b (t/union t/nil-t (t/prim :int))}))})]
    (is (= #{t/nil-t (t/prim :int) (t/lit :missing)}
           (alternatives (infer-type "(get-in m [:a :b] :missing)" {'m m})))))
  (let [entries {:t :coll :kind nil :of (t/prim :vector)}
        by-name {:t :map-of :k (t/prim :string) :v (t/prim :int)}]
    (is (= t/unknown (infer-type "(get-in m [0] :missing)" {'m entries})))
    (is (= #{(t/prim :int) (t/lit :missing)}
           (alternatives (infer-type "(get-in m [\"name\"] :missing)" {'m by-name}))))))

(deftest literal-key-path-writes-rebuild-the-map
  (is (= (t/lit "text")
         (infer-type "(get-in (assoc-in nil [:a :b] \"text\") [:a :b])")))
  (is (= (t/lit :sibling)
         (infer-type "(get-in (assoc-in {:a {:c :sibling}} [:a :b] 2) [:a :c])")))
  (is (= t/nil-t
         (infer-type "(get-in (assoc-in {:a nil} [:a :b] nil) [:a :b] :missing)")))
  (is (= (t/lit 7)
         (infer-type "(get-in (update-in {} [:a :b] (fn [old n] (if (nil? old) n (inc old))) 7) [:a :b])")))
  (is (= (t/prim :string)
         (infer-type "(let [{:keys [a]} (update-in {:a {:b 1}} [:a :b] str) {:keys [b]} a] b)")))
  (is (= (t/union (t/prim :int) t/nil-t)
         (infer-type "(:k (cond-> {} flag (assoc :k 1)))" {'flag (t/prim :boolean)})))
  (is (= t/unknown (infer-type "(get-in (assoc {} k 1) [:a] :missing)" {'k t/unknown})))
  (is (= t/unknown (infer-type "(assoc-in {} [:a 0] 1)")))
  (is (= t/unknown (infer-type "(update-in {} ks identity)"))))

(deftest literal-key-path-writes-check-types-from-spec
  (let [file (parse/parse-string "nested.clj"
                                "(ns nested (:require [clojure.spec.alpha :as s]))
                                 (s/def ::n int?)
                                 (s/def ::inner (s/keys :req [::n]))
                                 (s/def ::m (s/keys :req [::inner]))
                                 (s/fdef grow :args (s/cat :m ::m) :ret int?)
                                 (defn grow [m] (get-in (update-in m [::inner ::n] inc) [::inner ::n]))
                                 (assoc-in {} [::inner ::n] \"bad\")
                                 (update-in {:a {}} [:a :b] inc)
                                 (assoc-in {:a 1} [:a :b] true)
                                 (update-in {:a {:b \"text\"}} [:a :b] inc)
                                 (assoc-in {} [::n :child] true)")
        result (check/check-files [file] {})
        ds (:diagnostics result)]
    (is (= [[7 "TSP2322"] [8 "TSP2531"] [9 "TSP2339"] [10 "TSP2345"] [11 "TSP2322"]]
           (mapv (juxt (comp :row :pos) :code) ds)) (pr-str ds))
    (is (= "int?" (main/type-at result "nested.clj" 6 49)))))

(deftest string-path-writes-preserve-siblings-and-stored-nil
  (doseq [[source expected]
          [["(get-in (assoc-in {:headers nil} [:headers \"content-type\"] \"json\") [:headers \"content-type\"] :missing)"
            (t/lit "json")]
           ["(get-in (assoc-in {:headers {\"vary\" \"Accept\"}} [:headers \"content-type\"] 42) [:headers \"vary\"])"
            (t/lit "Accept")]
           ["(get-in (assoc-in {} [\"outer\" :inner] nil) [\"outer\" :inner] :missing)"
            t/nil-t]
           ["(get-in (update-in {:headers {\"count\" 2}} [:headers \"count\"] inc) [:headers \"count\"])"
            (t/prim :int)]
           ["(get-in (update-in {:headers {}} [:headers \"count\"] (fn [old n] (if-some [x old] (+ x n) n)) 1) [:headers \"count\"])"
            (t/lit 1)]]]
    (is (= expected (infer-type source)) source)))

(deftest select-keys-derives-presence-and-value-types-for-string-keys
  (let [by-name {:t :map-of :k (t/prim :string) :v (t/prim :string)}
        projected (infer-type "(select-keys m [\"url\" :other])" {'m by-name})
        unknown (infer-type "(select-keys m [\"url\"])" {'m t/unknown})]
    (is (= {"url" (t/prim :string)} (:opt projected)))
    (is (empty? (:req projected)))
    (is (true? (:complete-keys? projected)))
    (is (= {"url" t/unknown} (:opt unknown)))
    (is (empty? (:req unknown))))
  (doseq [[source expected]
          [["(get-in (select-keys {\"url\" nil :tag 1} [\"url\" :tag]) [\"url\"] :missing)" t/nil-t]
           ["(get-in (select-keys {\"url\" 1 \"kind\" 2} [\"url\"]) [\"kind\"] :missing)" (t/lit :missing)]
           ["(get-in (select-keys nil [\"url\"]) [\"url\"] :missing)" (t/lit :missing)]
           ["(get-in (select-keys {\"url\" 1} []) [\"url\"] :missing)" (t/lit :missing)]
           ["(get-in (select-keys (if flag {\"url\" 1} {}) [\"url\"]) [\"url\"] :missing)"
            (t/union (t/lit 1) (t/lit :missing))]]]
    (is (= (alternatives expected)
           (alternatives (infer-type source {'flag (t/prim :boolean)}))) source)))

(deftest string-map-operations-preserve-contract-checks-across-namespaces
  (let [api (parse/parse-string "api.clj"
                               "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::headers (s/map-of string? string?))
(s/def ::response (s/keys :req-un [::headers]))
(s/fdef send-response :args (s/cat :response ::response) :ret any?)
(defn send-response [response] response)
(s/fdef store-index :args (s/cat :row (s/map-of string? string?)) :ret any?)
(defn store-index [row] row)")
        client (parse/parse-string "client.clj"
                                  "(ns client (:require [api :as api] [clojure.spec.alpha :as s]))
(api/send-response (assoc-in {:headers {}} [:headers \"content-type\"] \"json\"))
(api/send-response (assoc-in {:headers {}} [:headers \"content-type\"] 42))
(api/store-index (select-keys {\"url\" \"urn:test\" \"ignored\" 42} [\"url\"]))
(api/store-index (select-keys {\"url\" 42 \"ignored\" \"anything\"} [\"url\"]))
(s/fdef good :args (s/cat :headers ::api/headers) :ret ::api/headers)
(defn good [headers] (update-in headers [\"content-type\"] (fn [old] (or old \"json\"))))
(s/fdef bad :args (s/cat :headers ::api/headers) :ret ::api/headers)
(defn bad [headers] (assoc-in headers [\"content-type\"] 42))
(s/fdef count-entry :args (s/cat :counts (s/map-of string? int?)) :ret any?)
(defn count-entry [counts] (update-in counts [\"count\"] inc))
(s/fdef modes :args (s/cat :m (s/map-of string? #{:enabled :disabled})) :ret (s/map-of string? #{:enabled :disabled}))
(defn modes [m] (assoc-in m [\"mode\"] :enabled))
(s/fdef literal-mode :args (s/cat) :ret (s/map-of string? #{\"json\" \"xml\"}))
(defn literal-mode [] (assoc-in {} [\"mode\"] \"json\"))")
        issues (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 3 "TSP2345"]
            ["client.clj" 5 "TSP2345"]
            ["client.clj" 9 "TSP2322"]
            ["client.clj" 11 "TSP2531"]]
           (mapv (juxt :file (comp :row :pos) :code) issues)) (pr-str issues))))

(deftest merge-required-left-optional-right
  (let [a (map-type {:x (t/prim :string)} {:z (t/prim :string)})
        b (map-type {} {:x (t/prim :int) :z t/nil-t})
        merged (infer-type "(merge a b)" {'a a 'b b})]
    (is (= #{(t/prim :string) (t/prim :int)} (alternatives (get-in merged [:req :x]))))
    (is (not (contains? (:opt merged) :x)))
    (is (= #{(t/prim :string) t/nil-t} (alternatives (get-in merged [:opt :z]))))
    (is (= (t/prim :string) (get-in (infer-type "(merge b a)" {'a a 'b b}) [:req :x])))
    (is (= t/nil-t (get-in (infer-type "(merge a b c)" {'a a 'b b 'c (map-type {:x t/nil-t} {})}) [:req :x])))))

(deftest clojure-truthiness
  (doseq [[source expected]
          [["(if false :yes :no)" :no]
           ["(if nil :yes :no)" :no]
           ["(if 0 :yes :no)" :yes]
           ["(if \"\" :yes :no)" :yes]
           ["(if (some? false) :yes :no)" :yes]
           ["(if-not false :yes :no)" :yes]
           ["(if-let [x false] :yes :no)" :no]
           ["(if-some [x false] :yes :no)" :yes]]]
    (is (= (t/lit expected) (infer-type source)) source))
  (is (= (t/lit true) (infer-type "(some? false)")))
  (is (= (t/lit false) (infer-type "(nil? false)")))
  (is (= (t/prim :boolean) (infer-type "(some? x)" {'x t/unknown})))
  (is (= #{(t/lit :yes) (t/lit :no)}
         (alternatives (infer-type "(if (some? x) :yes :no)" {'x t/unknown}))))
  (is (= t/nil-t (infer-type "(when false 1)")))
  (is (= (t/lit 1) (infer-type "(when-not false 1)")))
  (is (= (t/lit false) (infer-type "(and false 1)")))
  (is (= (t/lit 1) (infer-type "(or false 1)")))
  (is (= t/nil-t (infer-type "(when-let [x false] x)")))
  (is (= (t/lit false) (infer-type "(when-some [x false] x)"))))

(deftest branches-narrow-false-and-nil-separately
  (let [locals {'x (t/union t/nil-t (t/prim :boolean) (t/prim :string))}]
    (is (= #{(t/lit true) (t/prim :string) (t/lit :no)}
           (alternatives (infer-type "(if x x :no)" locals))))
    (is (= #{t/nil-t (t/lit false) (t/lit :yes)}
           (alternatives (infer-type "(if x :yes x)" locals))))
    (is (= #{(t/prim :boolean) (t/prim :string) (t/lit :no)}
           (alternatives (infer-type "(if (some? x) x :no)" locals))))
    (is (= #{t/nil-t (t/lit :yes)}
           (alternatives (infer-type "(if (some? x) :yes x)" locals))))
    (is (= #{t/nil-t (t/lit :no)}
           (alternatives (infer-type "(if (nil? x) x :no)" locals))))))
