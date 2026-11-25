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

(deftest keys-and-vals-project-map-entries
  (let [by-name {:t :map-of :k (t/prim :string) :v (t/prim :int)}
        complete (assoc (map-type {:required (t/lit 1)} {:optional t/nil-t}) :complete-keys? true)]
    (doseq [[source bounds element locals]
            [["(keys {:known 1})" [1 1] (t/lit :known) {}]
             ["(vals {:a 1 :b 1})" [2 2] (t/lit 1) {}]
             ["(keys m)" [1 2] (t/union (t/lit :required) (t/lit :optional)) {'m complete}]
             ["(vals m)" [1 2] (t/union (t/lit 1) t/nil-t) {'m complete}]
             ["(keys m)" [1 nil] (t/prim :string) {'m by-name}]
             ["(vals m)" [1 nil] (t/prim :int) {'m by-name}]
             ["(keys m)" [1 nil] t/any {'m (map-type {:open (t/lit 1)} {})}]]]
      (let [members (alternatives (infer-type source locals))
            items {:t :coll :kind :seq :of element :min-count (first bounds)}
            items (cond-> items (second bounds) (assoc :max-count (second bounds)))]
        (is (= (if (= locals {'m by-name}) #{items t/nil-t} #{items}) members) source)))
    (is (= t/nil-t (infer-type "(keys {})")))
    (is (= t/nil-t (infer-type "(vals nil)")))
    (is (= t/nil-t (infer-type "(first (vals {:key nil}))")))))

(deftest map-projections-check-spec-contracts-across-namespaces
  (let [api (parse/parse-string "api.clj"
              "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::index (s/map-of string? int?))
(s/fdef names :args (s/cat :m ::index) :ret (s/nilable (s/coll-of string? :kind seq?)))
(defn names [m] (keys m))")
        consumer (parse/parse-string "consumer.clj"
                   "(ns consumer (:require [api :as api] [clojure.spec.alpha :as s]))
(s/fdef bad-names :args (s/cat :m ::api/index) :ret (s/coll-of int? :kind seq?))
(defn bad-names [m] (sort (api/names m)))
(s/fdef wrong-values :args (s/cat :m ::api/index) :ret (s/nilable (s/coll-of string? :kind seq?)))
(defn wrong-values [m] (vals m))
(keys)")
        ds (:diagnostics (check/check-files [consumer api] {:strict-nil true}))]
    (is (= [["consumer.clj" 3 "TSP2322"] ["consumer.clj" 5 "TSP2322"] ["consumer.clj" 6 "TSP2554"]]
           (mapv (juxt :file (comp :row :pos) :code) ds))
        (pr-str ds))))

(deftest get-uses-the-map-of-domain-for-computed-keys
  (let [by-name {:t :map-of :k (t/prim :string) :v (t/prim :int)}
        by-tag {:t :map-of :k (t/prim :keyword) :v (t/prim :boolean)}
        either (t/union by-name by-tag t/nil-t)
        nullable (update by-name :v t/union t/nil-t)
        string-key (t/prim :string)
        fallback (t/lit :missing)]
    (doseq [[source map-type key-type expected]
            [["(get m k)" by-name string-key #{t/nil-t (t/prim :int)}]
             ["(get m k :missing)" by-name string-key #{fallback (t/prim :int)}]
             ["(get m k :missing)" nullable string-key #{fallback t/nil-t (t/prim :int)}]
             ["(get m k :missing)" either string-key #{fallback (t/prim :int)}]
             ["(get m k :missing)" either (t/prim :int) #{fallback}]
             ["(get m k :missing)" either (t/union string-key (t/prim :keyword))
              #{fallback (t/prim :int) (t/prim :boolean)}]
             ["(get m \"name\" :missing)" by-name t/unknown #{fallback (t/prim :int)}]
             ["(get m k :missing)" by-name t/unknown #{fallback (t/prim :int)}]
             ["(get m k :missing)" t/nil-t string-key #{fallback}]]]
      (is (= expected (alternatives (infer-type source {'m map-type 'k key-type}))) source))))

(deftest get-reads-literal-string-keys-from-structural-maps
  (let [name-type (t/prim :string)
        status-type (t/prim :keyword)
        fallback (t/lit :missing)
        complete (assoc (map-type {"name" name-type "present-null" t/nil-t}
                                  {"status" status-type}) :complete-keys? true)
        open (map-type {"name" name-type} {})]
    (doseq [[source ty expected]
            [["(get m \"name\" :missing)" complete #{name-type}]
             ["(get m \"present-null\" :missing)" complete #{t/nil-t}]
             ["(get m \"absent\" :missing)" complete #{fallback}]
             ["(get m \"absent\")" complete #{t/nil-t}]
             ["(get m \"status\" :missing)" complete #{status-type fallback}]
             ["(get m \"name\" :missing)" (t/union complete t/nil-t) #{name-type fallback}]
             ["(let [k \"name\"] (get m k))" open #{name-type}]
             ["(get m \"unknown\")" open #{t/unknown}]]]
      (is (= expected (alternatives (infer-type source {'m ty}))) source))
    (is (= (t/lit "StructureDefinition")
           (infer-type "(get {\"resourceType\" \"StructureDefinition\"} \"resourceType\")")))))

(deftest get-reads-vector-indices-with-clojure-presence-semantics
  (doseq [index [0 -1 2 4294967296 4294967297 1.0 1N]]
    (let [expected (get [nil :one] index :missing)
          expected (if (nil? expected) t/nil-t (t/lit expected))
          source (str "(get [nil :one] " (pr-str index) " :missing)")]
      (is (= expected (infer-type source)) source)))
  (let [element (t/prim :string)
        vector-type {:t :coll :kind :vector :of element :min-count 1 :max-count 2}
        optional (t/union vector-type t/nil-t)
        fallback (t/lit :missing)
        locals {'parts vector-type 'idx (t/prim :int)}]
    (is (= element (infer-type "(get parts 0 :missing)" locals)))
    (is (= #{element fallback}
           (alternatives (infer-type "(get parts 1 :missing)" locals))))
    (is (= fallback (infer-type "(get parts 2 :missing)" locals)))
    (is (= #{element t/nil-t}
           (alternatives (infer-type "(get parts idx)" (assoc locals 'parts optional)))))
    (is (= fallback (infer-type "(get parts idx :missing)" (assoc locals 'idx (t/prim :double)))))))

(deftest string-key-edits-keep-required-fields-and-removals
  (let [open (map-type {"url" (t/prim :string)} {"text" (t/prim :string)})]
    (is (= (t/prim :int) (infer-type "(get (assoc nil \"size\" 42) \"size\")")))
    (is (= (t/prim :int) (infer-type "(get (assoc {\"mode\" \"old\"} \"mode\" 42) \"mode\" :missing)")))
    (is (= (t/lit "kept")
           (infer-type "(get (dissoc {\"text\" \"old\" \"url\" \"kept\"} \"text\") \"url\")")))
    (is (= (t/lit :missing)
           (infer-type "(get (dissoc m \"text\") \"text\" :missing)" {'m open})))
    (is (= t/nil-t (infer-type "(get (assoc (dissoc m \"text\") \"text\" nil) \"text\" :missing)" {'m open})))))

(deftest string-key-edits-specialize-map-returns-across-namespaces
  (let [api (parse/parse-string "prepare.clj"
               "(ns prepare (:require [clojure.spec.alpha :as s]))
(s/fdef resource :args (s/cat :m map?) :ret map?)
(defn resource [m] (-> m (dissoc \"text\") (assoc \"_filename\" \"fixture.json\")))")
        consumer (parse/parse-string "client.clj"
                    "(ns client (:require [prepare :as prep] [clojure.spec.alpha :as s]))
(s/fdef url :args (s/cat) :ret string?)
(defn url [] (get (prep/resource {\"url\" \"urn:fixture\" \"text\" \"old\"}) \"url\"))
(s/fdef filename :args (s/cat) :ret int?)
(defn filename [] (get (prep/resource {\"text\" \"old\"}) \"_filename\"))
(get (prep/resource {\"text\" \"old\"}) \"text\" :missing)")
        result (check/check-files [consumer api] {:strict-nil true})]
    (is (= [["client.clj" 5 "TSP2322"]]
           (mapv (juxt :file (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))
    (is (= ":missing" (main/type-at result "client.clj" 6 1)))))

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

(deftest get-in-uses-computed-keys-from-known-types
  (let [cookie (map-type {:value (t/union t/nil-t (t/prim :string))} {})
        request (map-type {:cookies {:t :map-of :k (t/prim :string) :v cookie}} {})
        expected #{t/nil-t (t/prim :string) (t/lit :missing)}]
    (is (= expected (alternatives (infer-type "(get-in request [:cookies k :value] :missing)"
                                             {'request request 'k (t/prim :string)}))))
    (is (= (t/lit :missing) (infer-type "(get-in request [:cookies k :value] :missing)"
                                      {'request request 'k (t/prim :keyword)}))))
  (is (= #{(t/lit "first") (t/lit "last")}
         (alternatives (infer-type "(get-in [\"first\" :skipped \"last\"] [i] :missing)"
                                   {'i (t/union (t/lit 0) (t/lit 2))}))))
  (is (= #{t/nil-t (t/lit :missing)}
         (alternatives (infer-type "(get-in {:field nil} [k] :missing)"
                                   {'k (t/union (t/lit :field) (t/lit :other))})))))

(deftest get-in-follows-fixed-paths-stored-as-values
  (doseq [[source expected]
          [["(let [path [:a 0 :b]] (get-in {:a [{:b nil}]} path :missing))" t/nil-t]
           ["(let [path [:a 1 :b]] (get-in {:a [{:b nil}]} path :missing))" (t/lit :missing)]
           ["(let [path (if flag [:a :b] [:c])] (get-in {:a {:b 1} :c \"text\"} path :missing))"
            (t/union (t/lit 1) (t/lit "text"))]
           ["(let [path (if flag [:a] nil)] (get-in :root path :missing))"
            (t/union (t/lit :root) (t/lit :missing))]
           ["(let [path []] (get-in :root path :missing))" (t/lit :root)]
           ["(get-in :root path :missing)" t/unknown]]]
    (is (= (alternatives expected)
           (alternatives (infer-type source {'flag (t/prim :boolean) 'path (t/prim :vector)})))
        source)))

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

(deftest typed-map-path-writes-preserve-shape-and-check-the-leaf-contract
  (let [updated (infer-type "(let [path [:routes (if flag :get :post)]]
                              (assoc-in {:routes {:sibling :kept}} path \"handler\"))"
                            {'flag (t/prim :boolean)})
        branches (alternatives updated)]
    (is (= #{#{:sibling :get} #{:sibling :post}}
           (set (map #(set (keys (get-in % [:req :routes :req]))) branches))))
    (is (= #{(t/lit :kept)} (set (map #(get-in % [:req :routes :req :sibling]) branches)))))
  (is (= (t/prim :int) (infer-type "(let [path [:route :count]]
                                    (get-in (update-in {:route {:count 3}} path inc) path))")))
  (let [schema (parse/parse-string "schema.clj" "(ns schema (:require [clojure.spec.alpha :as s]))
(s/def ::handler string?)
(s/def ::path (s/tuple #{:routes} #{::handler}))")
        client (parse/parse-string "client.clj" "(ns client (:require [clojure.spec.alpha :as s] [schema :as schema]))
(s/fdef put-handler :args (s/cat :path ::schema/path) :ret any?)
(defn put-handler [path] (assoc-in {} path 42))")
        diagnostics (:diagnostics (check/check-files [client schema] {}))]
    (is (= [["client.clj" 3 "TSP2322"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics))))

(deftest dynamic-dictionary-paths-track-values-and-possible-absence
  (let [dict {:t :map-of :k (t/prim :string) :v (t/prim :int)}
        typed {'counters dict 'name (t/prim :string)}]
    (is (= (assoc dict :v (t/lit 1)) (infer-type "(assoc-in {} [name] 1)" {'name (t/prim :string)}))
        "An empty map can start a dictionary with a computed key")
    (is (= dict (infer-type "(update-in counters [name] (fn [n] (if (nil? n) (inc 0) (inc n))))" typed)))
    (is (= #{(t/prim :string) (t/lit :new)}
           (alternatives (:k (infer-type "(assoc-in counters [kw] 1)"
                                        {'counters dict 'kw (t/lit :new)}))))))
  (let [file (parse/parse-string "index.clj" "(ns index (:require [clojure.spec.alpha :as s]))
(s/def ::counts (s/map-of string? int?))
(s/def ::indices (s/map-of string? ::counts))
(s/def ::path (s/tuple string? string?))
(s/fdef put :args (s/cat :m ::indices :path ::path) :ret ::indices)
(defn put [m path] (assoc-in m path 42))
(s/fdef put-bad :args (s/cat :m ::indices :path ::path) :ret ::indices)
(defn put-bad [m path] (assoc-in m path \"bad\"))
(s/fdef unsafe-inc :args (s/cat :m ::indices :path ::path) :ret ::indices)
(defn unsafe-inc [m path] (update-in m path inc))")
        diagnostics (:diagnostics (check/check-files [file] {}))]
    (is (= [[8 "TSP2322"] [10 "TSP2531"]]
           (mapv (juxt (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics))))

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

(deftest select-keys-uses-the-shape-of-the-key-sequence
  (let [row (map-type {:id (t/prim :int)} {"name" (t/prim :string)})
        columns (t/tuple [(t/lit :id) (t/lit "name")])
        projected (infer-type "(select-keys row columns)" {'row row 'columns columns})]
    (is (= (:req row) (:req projected)))
    (is (= (:opt row) (:opt projected)))
    (is (true? (:complete-keys? projected))))
  (let [projected (infer-type "(let [columns [(if flag :id :name)]]
                                (select-keys {:id 1 :name \"Ada\"} columns))"
                              {'flag (t/prim :boolean)})]
    (is (= #{{:id (t/lit 1)} {:name (t/lit "Ada")}} (set (map :req (alternatives projected))))))
  (is (= (map-type {} {}) (select-keys (infer-type "(select-keys {:id 1} nil)") [:req :opt :t])))
  (is (= t/unknown (infer-type "(select-keys {:id 1} keys)" {'keys (t/prim :vector)}))))

(deftest symbolic-map-operations-check-contracts-across-namespaces
  (let [api (parse/parse-string "api.clj"
                               "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::counts (s/map-of symbol? int?))
(s/def ::options (s/keys :req-un [::counts]))
(s/fdef accept :args (s/cat :options ::options) :ret int?)
(defn accept [options] 1)
(s/fdef accept-counts :args (s/cat :counts ::counts) :ret int?)
(defn accept-counts [counts] 1)")
        client (parse/parse-string "client.clj"
                                  "(ns client (:require [api :as api]))
(api/accept (assoc-in {:counts {}} [:counts 'fixture/n] 4))
(api/accept (assoc-in {:counts {}} [:counts (quote fixture/n)] \"bad\"))
(inc (get-in (assoc-in nil ['config 'fixture/n] 4) ['config 'fixture/n]))
(inc (get-in (update-in {'config {'fixture/n 4 :n \"keyword\"}} ['config 'fixture/n] inc) ['config 'fixture/n]))
(update-in {'config {'fixture/n \"bad\"}} ['config 'fixture/n] inc)
(update-in {} ['config 'fixture/n] inc)
(assoc-in {'config 1} ['config 'fixture/n] 4)
(api/accept-counts (select-keys {'fixture/n 4 'ignored \"bad\"} ['fixture/n]))
(api/accept-counts (select-keys {'fixture/n \"bad\"} [(quote fixture/n)]))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 3 "TSP2345"] ["client.clj" 6 "TSP2345"] ["client.clj" 7 "TSP2531"]
            ["client.clj" 8 "TSP2339"] ["client.clj" 10 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest computed-symbol-paths-check-values-and-possible-absence
  (let [file (parse/parse-string "index.clj" "(ns index (:require [clojure.spec.alpha :as s]))
(s/def ::counts (s/map-of symbol? (s/map-of symbol? int?)))
(s/def ::path (s/tuple symbol? symbol?))
(s/fdef upsert :args (s/cat :m ::counts :path ::path) :ret ::counts)
(defn upsert [m path] (update-in m path (fn [n] (if (nil? n) 1 (inc n)))))
(s/fdef bump :args (s/cat :m ::counts :path ::path) :ret ::counts)
(defn bump [m path] (update-in m path inc))")
        issues (:diagnostics (check/check-files [file] {}))]
    (is (= [[7 "TSP2531"]] (mapv (juxt (comp :row :pos) :code) issues)) (pr-str issues)))
  (is (= t/nil-t (infer-type "(get-in (assoc-in {} '(config fixture/n) nil) ['config 'fixture/n] :missing)")))
  (is (= (t/lit :kept)
         (infer-type "(get-in (assoc-in {'config {:n :kept}} ['config 'fixture/n] 4) ['config :n])"))))

(deftest text-and-symbol-update-check-contracts-across-namespaces
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::counts (s/map-of string? int?))
(s/fdef accept :args (s/cat :counts ::counts) :ret int?)
(defn accept [counts] 1)
(s/fdef symbol-counts :args (s/cat) :ret (s/map-of symbol? int?))
(defn symbol-counts [] {'fixture/n 4})")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api]))
(api/accept (update {\"n\" 4 \"sibling\" 2} \"n\" inc))
(api/accept (update {\"n\" 4} \"n\" str))
(inc (get (update nil \"n\" (fn [old n] (or old n)) 4) \"n\"))
(let [{:as all} (list :tag :count (seq (api/symbol-counts)))
      {:syms [fixture/n]} (update all (quote fixture/n) (fn [old] (if (nil? old) 0 (inc old))))]
  (inc n))
(update {'fixture/n \"bad\"} 'fixture/n inc)
(update {} \"absent\" inc)")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 3 "TSP2345"] ["client.clj" 8 "TSP2345"] ["client.clj" 9 "TSP2531"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest literal-key-update-checks-homogeneous-maps-and-preserves-siblings
  (let [file (parse/parse-string "counts.clj" "(ns counts (:require [clojure.spec.alpha :as s]))
(s/def ::counts (s/map-of symbol? int?))
(s/fdef upsert :args (s/cat :m ::counts) :ret ::counts)
(defn upsert [m] (update m 'fixture/n (fn [old] (if (nil? old) 0 (inc old)))))
(s/fdef increment :args (s/cat :m ::counts) :ret ::counts)
(defn increment [m] (update m 'fixture/n inc))")
        issues (:diagnostics (check/check-files [file] {}))]
    (is (= [[6 "TSP2531"]] (mapv (juxt (comp :row :pos) :code) issues)) (pr-str issues)))
  (is (= (t/lit :kept)
         (infer-type "(get (update {'n 1 \"n\" :kept} 'n inc) \"n\")"))))

(deftest computed-key-update-checks-spec-contracts-across-namespaces
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::choice #{\"n\" \"m\"})
(s/def ::counts (s/map-of string? int?))
(s/def ::symbol-counts (s/map-of symbol? int?))
(s/fdef accept :args (s/cat :counts ::counts) :ret int?)
(defn accept [counts] 1)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api] [clojure.spec.alpha :as s]))
(s/fdef bump :args (s/cat :key ::api/choice) :ret int?)
(defn bump [key] (api/accept (update {\"n\" 1 \"m\" 2 \"keep\" 3} key inc)))
(s/fdef bad-result :args (s/cat :key ::api/choice) :ret int?)
(defn bad-result [key] (api/accept (update {\"n\" 1 \"m\" 2} key str)))
(s/fdef maybe-missing :args (s/cat :key ::api/choice) :ret ::api/counts)
(defn maybe-missing [key] (update {\"n\" 1} key inc))
(s/fdef upsert :args (s/cat :m ::api/symbol-counts :k symbol?) :ret ::api/symbol-counts)
(defn upsert [m k] (update m k (fn [n] (if (nil? n) 1 (inc n)))))
(s/fdef unsafe :args (s/cat :m ::api/symbol-counts :k symbol?) :ret ::api/symbol-counts)
(defn unsafe [m k] (update m k inc))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 5 "TSP2345"] ["client.clj" 7 "TSP2531"] ["client.clj" 11 "TSP2531"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest computed-key-update-retains-each-possible-shape
  (let [result (infer-type "(update {\"a\" 1 \"b\" 2 \"keep\" :sibling} key inc)"
                           {'key (t/union (t/lit "a") (t/lit "b"))})]
    (is (= #{{"a" (t/prim :int) "b" (t/lit 2) "keep" (t/lit :sibling)}
             {"a" (t/lit 1) "b" (t/prim :int) "keep" (t/lit :sibling)}}
           (set (map :req (alternatives result))))))
  (is (= (t/prim :map)
         (infer-type "(update {} key identity)"
                     {'key (apply t/union (map #(t/lit (str %)) (range 257)))}))))

(deftest computed-get-checks-spec-contracts-after-update-across-namespaces
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::key #{:n \"m\"})
(s/def ::maybe-key #{\"n\" \"absent\"})
(s/fdef accept :args (s/cat :n int?) :ret int?)
(defn accept [n] n)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api] [clojure.spec.alpha :as s]))
(s/fdef bump :args (s/cat :key ::api/key) :ret int?)
(defn bump [key] (api/accept (get (update {:n 1 \"m\" 2} key inc) key \"unused\")))
(s/fdef wrong :args (s/cat :key ::api/key) :ret number?)
(defn wrong [key] (inc (get {:n 1 \"m\" \"bad\"} key)))
(s/fdef defaulted :args (s/cat :key ::api/maybe-key) :ret int?)
(defn defaulted [key] (inc (get {\"n\" 4} key 0)))
(s/fdef stored-nil :args (s/cat :key ::api/maybe-key) :ret int?)
(defn stored-nil [key] (inc (get {\"n\" nil} key 0)))
(let [key :n] (inc (get {:n 2} key)))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 5 "TSP2345"] ["client.clj" 9 "TSP2531"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest computed-get-applies-its-default-only-to-absent-keys
  (let [bindings {'k (t/union (t/lit 'fixture/n) (t/lit "n"))}]
    (is (= #{(t/lit 1) (t/lit 2)}
           (alternatives (infer-type "(get {'fixture/n 1 \"n\" 2} k :missing)" bindings))))
    (is (= #{t/nil-t (t/lit :missing)}
           (alternatives (infer-type "(get {'fixture/n nil} k :missing)" bindings)))))
  (is (= t/unknown
         (infer-type "(get {:a 1} k)" {'k (apply t/union (map #(t/lit (str %)) (range 257)))}))))

(deftest computed-dissoc-checks-spec-contracts-across-namespaces
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::drop-key #{:a :b})
(s/def ::maybe-required #{:a :keep})
(s/def ::gone #{\"gone\"})
(s/def ::keep int?)
(s/def ::options (s/keys :req-un [::keep]))
(s/def ::words (s/map-of string? string?))
(s/fdef accept :args (s/cat :options ::options) :ret int?)
(defn accept [options] (:keep options))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api] [clojure.spec.alpha :as s]))
(s/fdef trim :args (s/cat :key ::api/drop-key) :ret int?)
(defn trim [key] (api/accept (dissoc {:a 1 :b 2 :keep 4} key)))
(s/fdef bad-trim :args (s/cat :key ::api/maybe-required) :ret int?)
(defn bad-trim [key] (api/accept (dissoc {:a 1 :keep 4} key)))
(s/fdef removed :args (s/cat :words ::api/words :key ::api/gone) :ret int?)
(defn removed [words key]
  (let [{:as all} (list :tag :words words)] (inc (get (dissoc all key) \"gone\" 0))))
(s/fdef nil-after :args (s/cat :key ::api/gone) :ret number?)
(defn nil-after [key] (inc (get (dissoc {\"gone\" 4} key) \"gone\")))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 5 "TSP2345"] ["client.clj" 10 "TSP2531"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest computed-dissoc-preserves-shapes-for-multiple-keys
  (let [result (infer-type "(dissoc {:a 1 :b 2 \"x\" 3 \"y\" 4 'n 5 :keep 6} k field 'n)"
                           {'k (t/union (t/lit :a) (t/lit :b))
                            'field (t/union (t/lit "x") (t/lit "y"))})]
    (is (= #{#{:a "x" :keep} #{:a "y" :keep} #{:b "x" :keep} #{:b "y" :keep}}
           (set (map #(set (keys (:req %))) (alternatives result))))))
  (let [keys (apply t/union (map #(t/lit (str %)) (range 17)))]
    (is (= (t/prim :map) (infer-type "(dissoc {:keep 1} first-key second-key)"
                                    {'first-key keys 'second-key keys})))))

(deftest computed-assoc-checks-spec-contracts-across-namespaces
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::keep int?)
(s/def ::limit int?)
(s/def ::options (s/keys :req-un [::keep]))
(s/def ::new-key #{:a :b})
(s/def ::maybe-required #{:keep :extra})
(s/def ::maybe-limited #{::limit :extra})
(s/fdef accept :args (s/cat :m ::options) :ret int?)
(defn accept [m] (:keep m))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api] [clojure.spec.alpha :as s]))
(s/fdef good :args (s/cat :key ::api/new-key) :ret int?)
(defn good [key] (api/accept (assoc {:keep 4} key 2)))
(s/fdef bad :args (s/cat :key ::api/maybe-required) :ret int?)
(defn bad [key] (api/accept (assoc {:keep 4} key \"bad\")))
(s/fdef limits :args (s/cat :key ::api/maybe-limited) :ret map?)
(defn limits [key] (assoc {} key \"bad\"))
(s/fdef read-existing :args (s/cat :key ::api/new-key) :ret int?)
(defn read-existing [key] (inc (get (assoc {\"keep\" 4} key 1) \"keep\")))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 5 "TSP2345"] ["client.clj" 7 "TSP2322"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest computed-assoc-preserves-key-variants-and-write-order
  (let [result (infer-type "(assoc {:a 1 :b 2 \"keep\" :ok} k 3 :a \"final\")"
                           {'k (t/union (t/lit :a) (t/lit :b))})]
    (is (= #{{:a (t/prim :string) :b (t/lit 2) "keep" (t/lit :ok)}
             {:a (t/prim :string) :b (t/prim :int) "keep" (t/lit :ok)}}
           (set (map :req (alternatives result))))))
  (let [result (infer-type "(assoc nil k 4)" {'k (t/union (t/lit 'n) (t/lit "n"))})]
    (is (= #{{'n (t/prim :int)} {"n" (t/prim :int)}} (set (map :req (alternatives result))))))
  (let [keys (apply t/union (map #(t/lit (str %)) (range 17)))]
    (is (= (t/prim :map) (infer-type "(assoc {} first-key 1 second-key 2)"
                                    {'first-key keys 'second-key keys})))))

(deftest computed-contains-narrows-keys-across-namespaces
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::key #{:n \"missing\"})
(s/def ::indexed-key #{\"n\" :missing})
(s/def ::numbers (s/map-of string? int?))
(s/fdef present :args (s/cat :key #{:n} :value int?) :ret int?)
(defn present [key value] value)
(s/fdef missing :args (s/cat :key #{\"missing\"}) :ret int?)
(defn missing [key] 0)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api] [clojure.spec.alpha :as s]))
(s/fdef guarded :args (s/cat :key ::api/key) :ret int?)
(defn guarded [key]
  (let [m {:n 4}]
    (if (contains? m key) (api/present key (get m key)) (api/missing key))))
(s/fdef indexed :args (s/cat :numbers ::api/numbers :key ::api/indexed-key) :ret int?)
(defn indexed [numbers key]
  (let [{:strs [n] :as all} (list :tag :count numbers)]
    (if (contains? all key) (inc n) 0)))
(s/fdef stored-nil :args (s/cat :key ::api/key) :ret int?)
(defn stored-nil [key]
  (let [m {:n nil}]
    (if (contains? m key) (inc (get m key 0)) 0)))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 13 "TSP2531"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest computed-contains-retains-all-reachable-key-choices
  (let [bindings {'k (t/union (t/lit :a) (t/lit :b) (t/lit :c))}]
    (is (= #{(t/lit :a) (t/lit :b) (t/lit :absent)}
           (alternatives (infer-type "(let [m {:a 1 :b 2}] (if (contains? m k) k :absent))" bindings))))
    (is (= #{(t/lit :present) (t/lit :c)}
           (alternatives (infer-type "(let [m {:a 1 :b 2}] (if (contains? m k) :present k))" bindings))))))

(deftest contains-narrows-map-of-contracts-across-namespaces
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::counts (s/map-of string? int?))
(s/def ::words (s/map-of string? string?))
(s/def ::nullable (s/map-of string? (s/nilable int?)))
(s/def ::key #{\"n\" :missing})
(s/fdef present :args (s/cat :key #{\"n\"} :n int?) :ret int?)
(defn present [key n] n)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api] [clojure.spec.alpha :as s]))
(s/fdef count-at :args (s/cat :m ::api/counts) :ret int?)
(defn count-at [m] (if (contains? m \"n\") (inc (get m \"n\")) 0))
(s/fdef computed :args (s/cat :m ::api/counts :key ::api/key) :ret int?)
(defn computed [m key] (if (contains? m key) (api/present key (get m key)) 0))
(s/fdef absent :args (s/cat :m ::api/words) :ret int?)
(defn absent [m] (if (contains? m \"n\") 0 (inc (get m \"n\" 0))))
(s/fdef stored-nil :args (s/cat :m ::api/nullable) :ret int?)
(defn stored-nil [m] (if (contains? m \"n\") (inc (get m \"n\")) 0))
(s/fdef unsafe :args (s/cat :m ::api/counts :key string?) :ret ::api/counts)
(defn unsafe [m key] (if (contains? m \"n\") (update m key inc) m))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 9 "TSP2531"] ["client.clj" 11 "TSP2531"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest contains-preserves-dictionary-types-for-iteration-and-writes
  (let [file (parse/parse-string "counts.clj" "(ns counts (:require [clojure.spec.alpha :as s]))
(s/def ::counts (s/map-of string? int?))
(s/fdef names :args (s/cat :m ::counts) :ret (s/nilable (s/coll-of string? :kind seq?)))
(defn names [m] (when (contains? m \"n\") (keys m)))
(s/fdef values :args (s/cat :m ::counts) :ret (s/nilable (s/coll-of int? :kind seq?)))
(defn values [m] (when (contains? m \"n\") (vals m)))
(s/fdef total :args (s/cat :m ::counts) :ret int?)
(defn total [m] (if (contains? m \"n\") (reduce-kv (fn [acc _ n] (+ acc n)) 0 m) 0))
(s/fdef edit :args (s/cat :m ::counts :key string?) :ret ::counts)
(defn edit [m key] (if (contains? m \"n\") (assoc m key 1) (dissoc m key)))
(s/fdef upsert :args (s/cat :m ::counts :key string?) :ret ::counts)
(defn upsert [m key]
  (if (contains? m \"n\") (update-in m [key] (fn [n] (if (nil? n) 1 (inc n)))) m))")
        issues (:diagnostics (check/check-files [file] {}))]
    (is (empty? issues) (pr-str issues))))

(deftest dictionary-value-guards-narrow-spec-contracts-across-namespaces
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::counts (s/map-of string? (s/nilable int?)))
(s/def ::words (s/map-of string? string?))
(s/def ::nested (s/map-of string? (s/map-of symbol? int?)))
(s/fdef accept :args (s/cat :n int?) :ret int?)
(defn accept [n] n)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api] [clojure.spec.alpha :as s]))
(s/fdef count-at :args (s/cat :m ::api/counts) :ret int?)
(defn count-at [m] (if (some? (get m \"n\")) (inc (get m \"n\")) 0))
(s/fdef by-alias :args (s/cat :m ::api/counts) :ret int?)
(defn by-alias [{:strs [n] :as all}] (if (some? n) (inc (get all \"n\")) 0))
(s/fdef nested :args (s/cat :m ::api/nested) :ret int?)
(defn nested [m]
  (if (some? (get-in m [\"outer\" 'fixture/n]))
    (api/accept (get-in m [\"outer\" 'fixture/n]))
    0))
(s/fdef absent :args (s/cat :m ::api/words) :ret int?)
(defn absent [m] (if (nil? (get m \"n\")) (inc (get m \"n\" 0)) 0))
(s/fdef stored-nil :args (s/cat :m ::api/counts) :ret int?)
(defn stored-nil [m] (if (nil? (get m \"n\")) (inc (get m \"n\" 0)) 0))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 14 "TSP2531"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest dictionary-some-guard-keeps-false-as-a-present-value
  (let [bindings {'m {:t :map-of :k (t/prim :string) :v (t/lit false)}}]
    (is (= #{(t/lit false) (t/lit :absent)}
           (alternatives
            (infer-type "(if (some? (get m \"flag\")) (get m \"flag\" :missing) :absent)" bindings))))))

(deftest stable-local-lookup-keys-refine-map-reads-across-namespaces
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::key #{\"n\"})
(s/def ::maybe-key #{\"n\" :missing})
(s/def ::bucket #{\"outer\"})
(s/def ::field #{:n})
(s/def ::counts (s/map-of string? (s/nilable int?)))
(s/def ::rows (s/map-of string? (s/map-of keyword? int?)))
(s/fdef accept :args (s/cat :n int?) :ret int?)
(defn accept [n] n)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api] [clojure.spec.alpha :as s]))
(s/fdef simple :args (s/cat :m ::api/counts :k ::api/key) :ret int?)
(defn simple [m k] (if (some? (get m k)) (api/accept (get m \"n\")) 0))
(s/fdef guarded :args (s/cat :m ::api/counts :k ::api/maybe-key) :ret int?)
(defn guarded [m k]
  (if (and (contains? m k) (some? (get m k))) (inc (get m \"n\")) 0))
(s/fdef nested :args (s/cat :m ::api/rows :outer ::api/bucket :inner ::api/field) :ret int?)
(defn nested [m outer inner]
  (if (some? (get-in m [outer inner])) (api/accept (get-in m [\"outer\" :n])) 0))
(s/fdef shadow :args (s/cat :m ::api/counts :k ::api/key) :ret int?)
(defn shadow [m k]
  (if (some? (get m k)) (let [k \"other\"] (inc (get m k))) 0))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 12 "TSP2531"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest a-lookup-with-multiple-key-choices-does-not-prove-an-exact-field
  (let [file (parse/parse-string "counts.clj" "(ns counts (:require [clojure.spec.alpha :as s]))
(s/fdef read-n :args (s/cat :m (s/map-of string? int?) :key #{\"n\" \"other\"}) :ret int?)
(defn read-n [m key] (if (some? (get m key)) (inc (get m \"n\")) 0))")
        issues (:diagnostics (check/check-files [file] {}))]
    (is (= [[3 "TSP2531"]] (mapv (juxt (comp :row :pos) :code) issues)) (pr-str issues))))

(deftest find-guards-refine-key-presence-across-namespaces
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::counts (s/map-of string? int?))
(s/def ::words (s/map-of string? string?))
(s/def ::nullable (s/map-of string? (s/nilable int?)))
(s/def ::key #{\"n\" :missing})
(s/fdef accept :args (s/cat :key #{\"n\"} :value int?) :ret int?)
(defn accept [key value] value)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api] [clojure.spec.alpha :as s]))
(s/fdef count-at :args (s/cat :m ::api/counts) :ret int?)
(defn count-at [m] (if-some [entry (find m \"n\")] (+ (val entry) (get m \"n\")) 0))
(s/fdef computed :args (s/cat :m ::api/counts :key ::api/key) :ret int?)
(defn computed [m key]
  (if-let [[found value] (find m key)] (api/accept key (+ value (get m key))) 0))
(s/fdef absent :args (s/cat :m ::api/words) :ret int?)
(defn absent [m] (if (nil? (find m \"n\")) (inc (get m \"n\" 0)) 0))
(s/fdef stored-nil :args (s/cat :m ::api/nullable) :ret int?)
(defn stored-nil [m] (if-some [entry (find m \"n\")] (inc (get m \"n\")) 0))
(s/fdef truthy :args (s/cat :m ::api/counts) :ret (s/nilable int?))
(defn truthy [m] (when (find m \"n\") (inc (get m \"n\"))))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 10 "TSP2531"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest a-shadowed-find-function-does-not-prove-map-key-presence
  (let [file (parse/parse-string "counts.clj" "(ns counts (:require [clojure.spec.alpha :as s]))
(s/fdef read-n :args (s/cat :m (s/map-of string? int?)) :ret int?)
(defn read-n [m]
  (let [find (fn [_m _key] true)]
    (if-some [entry (find m \"n\")] (inc (get m \"n\")) 0)))")
        issues (:diagnostics (check/check-files [file] {}))]
    (is (= [[5 "TSP2531"]] (mapv (juxt (comp :row :pos) :code) issues)) (pr-str issues))))

(deftest saved-find-results-refine-keys-across-namespaces
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/def ::counts (s/map-of string? int?))
(s/def ::words (s/map-of string? string?))
(s/def ::key #{\"n\" :missing})
(s/fdef accept :args (s/cat :key #{\"n\"} :n int?) :ret int?)
(defn accept [key n] n)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api] [clojure.spec.alpha :as s]))
(s/fdef direct :args (s/cat :m ::api/counts) :ret int?)
(defn direct [m]
  (let [entry (find m \"n\")]
    (if entry (+ (val entry) (get m \"n\")) 0)))
(s/fdef computed :args (s/cat :m ::api/counts :key ::api/key) :ret int?)
(defn computed [m key]
  (let [entry (find m key)]
    (if (some? entry) (api/accept key (+ (val entry) (get m key))) 0)))
(s/fdef absent :args (s/cat :m ::api/words) :ret int?)
(defn absent [m]
  (let [entry (find m \"n\")]
    (if (nil? entry) (inc (get m \"n\" 0)) 0)))
(s/fdef rebind :args (s/cat :m ::api/counts) :ret int?)
(defn rebind [m]
  (let [entry (find m \"n\") m {}]
    (if entry (inc (get m \"n\")) 0)))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 17 "TSP2531"]] (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues))))

(deftest saved-find-results-respect-key-rebinding
  (let [file (parse/parse-string "counts.clj" "(ns counts (:require [clojure.spec.alpha :as s]))
(s/fdef read-n :args (s/cat :m (s/map-of string? int?) :key #{\"n\"}) :ret int?)
(defn read-n [m key]
  (let [entry (find m key) key \"other\"]
    (if (some? entry) (+ (val entry) (get m key)) 0)))")
        issues (:diagnostics (check/check-files [file] {}))]
    (is (= [[5 "TSP2531"]] (mapv (juxt (comp :row :pos) :code) issues)) (pr-str issues))))

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
