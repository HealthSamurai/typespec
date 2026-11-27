(ns typespec.externs-regression-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [typespec.main :as main]
            [typespec.externs :as externs]
            [typespec.types :as t]))

(defn project [source extern cfg f]
  (let [root (.toFile (java.nio.file.Files/createTempDirectory "typespec-extern-" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (try
      (.mkdir (io/file root "src"))
      (doseq [[name content] (if (map? source) source {"app.clj" source})]
        (spit (io/file root "src" name) content))
      (spit (io/file root "api.edn") extern)
      (spit (io/file root "other.edn") "{app/f [int? :-> int?]}")
      (spit (io/file root "typespec.edn") (pr-str (merge {:externs ["api.edn"]} cfg)))
      (f (main/run-check (str root) nil {}))
      (finally (doseq [file (reverse (file-seq root))] (.delete file))))))

(defn codes [res] (set (map :code (:diagnostics res))))

(deftest generic-extern-loading-and-equivalence
  (project "(ns app)" "{vendor/id (t/all [a] [a :-> a])}" {}
           (fn [res]
             (is (empty? (:diagnostics res)))
             (is (= :all (get-in res [:env :vars 'vendor/id :type :t])))))
  (let [parse #(t/parse-type {:fail (fn [f] (throw (ex-info "bad type" {:form f})))} %)
        source {:type (parse '(t/all [a] [a :-> a])) :source :sig}
        ext {:type (parse '(t/all [b] [b :-> b])) :source :extern :file "api.edn"}
        [_ ds] (externs/merge-contracts {:vars {'app/f source}} {:vars {'app/f ext}})]
    (is (empty? ds)))
  (project "(ns app)" "{vendor/id string?}" {}
           (fn [res] (is (contains? (codes res) "TSP9003")))))

(deftest third-party-and-keyword-contracts
  (project "(ns app (:require [not.loaded.library :as lib]))\n(lib/f 1)\n{:api/name 42}"
           "{not.loaded.library/f [string? :-> int?] :api/name string?}" {}
           (fn [res]
             (is (contains? (codes res) "TSP2345"))
             (is (seq (:diagnostics res)))
             (is (= :extern (get-in res [:env :vars 'not.loaded.library/f :source])))
             (is (= "api.edn" (get-in res [:env :vars 'not.loaded.library/f :extern/path])))
             (is (nil? (find-ns 'not.loaded.library)))
             (is (re-find #"extern api.edn" (first (main/sig-lines res 'not.loaded.library))))
             (is (string? (json/write-str (mapv main/diag->json (:diagnostics res))))))))

(deftest own-body-is-checked
  (project "(ns app) (defn f [x] 42)" "{app/f [string? :-> string?]}" {}
           (fn [res]
             (is (contains? (codes res) "TSP2322"))
             (is (some? (get-in res [:env :vars 'app/f :parts])))
             (is (some? (get-in res [:env :vars 'app/f :pfile]))))))

(deftest source-conflicts-are-explicit
  (doseq [source ["(ns app) (defn f {:t/sig [string? :-> string?]} [x] x)"
                  "(ns app (:require [clojure.spec.alpha :as s])) (s/fdef f :args (s/cat :x string?) :ret string?) (defn f [x] x)"
                  "(ns app (:require [clojure.spec.alpha :as s])) (s/def :api/name int?)"]]
    (project source "{app/f [int? :-> int?] :api/name string?}" {}
             #(is (contains? (codes %) "TSP2790")))))

(deftest extern-arguments-complement-a-source-return-spec
  (project {"api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/fdef format-number :ret string?)
(defn format-number [n] (str n))
(s/fdef wrong :ret string?)
(defn wrong [n] (+ n 1))
(s/fdef disagree :ret string?)
(defn disagree [n] (str n))"
            "client.clj" "(ns client (:require [api :as a] [clojure.string :as str]))
(str/upper-case (a/format-number 1))
(a/format-number \"wrong\")
(inc (a/disagree 1))"}
           "{api/format-number [int? :-> string?]
             api/wrong [int? :-> t/unknown]
             api/disagree [int? :-> int?]}" {}
           (fn [res]
             (is (= [["api.edn" 1 "TSP2790"] ["src/api.clj" 5 "TSP2322"]
                     ["src/client.clj" 3 "TSP2345"] ["src/client.clj" 4 "TSP2345"]]
                    (mapv (juxt :file (comp :row :pos) :code) (:diagnostics res)))
                 (pr-str (:diagnostics res)))
             (is (= :fdef (get-in res [:env :vars 'api/format-number :source])))
             (is (= "api.edn" (get-in res [:env :vars 'api/format-number :extern/path]))))))

(deftest record-factories-accept-externs-with-the-generated-arity
  (project "(ns app) (defrecord Entry [label]) (->Entry \"ok\") (->Entry 42) (map->Entry {}) (map->Entry)"
           "{app/->Entry [string? :-> map?] app/map->Entry [:-> map?]}" {}
           (fn [res]
             (is (= ["TSP2790" "TSP2345" "TSP2554"] (mapv :code (:diagnostics res)))
                 (pr-str (:diagnostics res)))
             (is (= :extern (get-in res [:env :vars 'app/->Entry :source])))
             (is (= :record (get-in res [:env :vars 'app/map->Entry :source]))))))

(deftest core-policy
  (project "(ns app) (inc 1)" "{clojure.core/inc [string? :-> string?]}" {}
           #(is (contains? (codes %) "TSP9003")))
  (project "(ns app) (inc 1) (clojure.core/inc 1)" "{clojure.core/inc [string? :-> string?]}"
           {:externs-core :override}
           (fn [res]
             (is (= 2 (count (filter #(= "TSP2345" (:code %)) (:diagnostics res)))))
             (is (not (contains? (codes res) "TSP9003"))))))

(deftest inert-readers-and-missing-refs
  (let [called (atom false)]
    (binding [*data-readers* {'evil/tag (fn [_] (reset! called true) {})}]
      (project "(ns app)" "#evil/tag {}" {}
               #(is (contains? (codes %) "TSP9003"))))
    (is (false? @called)))
  (project "(ns app) (defn f [x] x)" "{app/f [:missing/type :-> :missing/type]}" {}
           (fn [res]
             (is (contains? (codes res) "TSP9003"))
             (is (false? (get-in res [:env :vars 'app/f :complete?])))))
  (project "(ns app)" "{app/f [:a/type :-> int?] :a/type :missing/type}" {}
           #(is (contains? (codes %) "TSP9003"))))

(deftest duplicate-extern-conflict
  (project "(ns app)" "{app/f [string? :-> string?]}"
           {:externs ["api.edn" "other.edn"]}
           #(is (contains? (codes %) "TSP9003"))))

(deftest invalid-externs-are-diagnostics
  (doseq [text ["#=(System/exit 1)" "#evil/tag {}" "[]" "{} {}" "{f [int? :-> int?]}"
                "{app/f int?}" "{app/f [bogus? :-> int?]}" "{app/f [int?]}" "{"]]
    (project "(ns app)" text {} #(is (contains? (codes %) "TSP9003") text)))
  (doseq [cfg [{:externs ["missing.edn"]} {:externs "api.edn"} {:externs-core :oops}]]
    (project "(ns app)" "{}" cfg #(is (contains? (codes %) "TSP9003")))))

(deftest valid-extern-calls-and-special-rule-override
  (project "(ns app (:require [not.loaded.library :refer [f]])) (defn own [x] (f x)) (own \"ok\")"
           "{not.loaded.library/f [string? :-> string?] app/own [string? :-> string?]}" {}
           #(is (empty? (:diagnostics %))))
  (project "(ns app) (count 1) (clojure.core/count 1)"
           "{clojure.core/count [string? :-> int?]}" {:externs-core :override}
           #(is (= 2 (count (filter (fn [d] (= "TSP2345" (:code d))) (:diagnostics %)))))))
