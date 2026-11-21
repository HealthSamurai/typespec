(ns typespec.literal-contract-test
  (:require [clojure.test :refer [deftest is]]
            [typespec.check :as check]
            [typespec.main :as main]
            [typespec.parse :as parse]))

(def ^:private declarations
  "(ns literal (:require [clojure.spec.alpha :as s]))
(s/def ::method #{:get :post})
(s/def ::path string?)
(s/def ::route (s/keys :req-un [::method ::path]))
")

(defn- checked [code]
  (check/check-files [(parse/parse-string "literal.clj" (str declarations code))] {:strict-nil true}))

(defn- codes [result] (mapv :code (:diagnostics result)))

(deftest satisfies-returns-the-actual-type-and-is-not-an-escape
  (let [result (checked "(let [x #_(t/satisfies ::route) {:method :get :path \"/\" :cache true}]\n  (:method x))")]
    (is (empty? (codes result)) (pr-str (:diagnostics result)))
    (is (= ":get" (main/type-at result "literal.clj" 6 3)))
    (is (= {:cast 0 :any 0 :ignore 0} (:trust result))))
  (let [result (checked "(let [x #_(t/satisfies ::route) {:method :delete :paht \"/\"}]\n  (:method x))")]
    (is (= ["TSP2741" "TSP2322" "TSP2353"] (codes result)) (pr-str (:diagnostics result)))
    (is (= [33 42 50] (mapv #(get-in % [:pos :col]) (:diagnostics result))))
    (is (= ":delete" (main/type-at result "literal.clj" 6 3))))
  (let [result (checked "(let [x {:method :get :path \"/\" :unrelated 22}]\n  #_(t/satisfies ::route) (:unrelated (do #_(t/satisfies ::route) x)))")]
    (is (= ["TSP2345"] (codes result)) (pr-str (:diagnostics result)))
    (is (= "22" (main/type-at result "literal.clj" 6 26)))))

(deftest const-keeps-nested-literals-and-does-not-cross-into-calls
  (let [result (checked "#_(t/const) {:route {:methods [:get :post] :flags #{true nil}}}\n{:route {:methods [:get :post]}}\n#_(t/const) [(do [:get :post]) [:get :post]]")]
    (is (empty? (codes result)) (pr-str (:diagnostics result)))
    (is (= "{:route (s/keys :req [:methods :flags])}" (main/type-at result "literal.clj" 5 13)))
    (is (= "(s/coll-of #{:get :post} :kind vector?)" (main/type-at result "literal.clj" 5 31)))
    (is (= "(s/coll-of keyword? :kind vector?)" (main/type-at result "literal.clj" 6 19)))
    (is (= "(s/coll-of keyword? :kind vector?)" (main/type-at result "literal.clj" 7 18))))
  (let [result (checked "#_(t/const) #_(t/satisfies (s/coll-of #{:get :post} :kind vector?))\n[:get :post]")]
    (is (empty? (codes result)) (pr-str (:diagnostics result)))
    (is (= "(s/coll-of #{:get :post} :kind vector?)" (main/type-at result "literal.clj" 6 1)))))

(deftest malformed-and-unsupported-claims-are-visible
  (let [result (checked "(s/def ::opaque (s/and string? #(pos? (count %))))
                         #_(t/satisfies ::opaque) \"\"
                         #_(t/satisfies ::absent) 1
                         #_(t/satisfies (s/map-of string?)) {}
                         #_(t/satisfies) 1
                         #_(t/const 1) [:get]
                         #_(t/const) #_(t/const) [:get]
                         #_(t/satisfies string?) ^String \"x\"
                         #_(t/const)")]
    (is (= (concat (repeat 3 "TSP280-01") (repeat 6 "TSP280-02")) (codes result))
        (pr-str (:diagnostics result)))
    (is (= [#{:unsupported-contract} #{:unresolved-type} #{:invalid-contract}]
           (mapv :reasons (take 3 (:diagnostics result)))))))

(deftest evaluated-keys-and-callees-are-checked
  (let [result (checked "{#_(t/satisfies string?) :method :get #_(t/const)}
                         (#_(t/satisfies string?) inc 1)")]
    (is (= ["TSP2345" "TSP280-02" "TSP2345"] (codes result))
        (pr-str (:diagnostics result)))))

(deftest trust-and-unknown-are-not-proof
  (let [result (checked "(defn unchecked [x] #_(t/satisfies string?) x)
                         (let [x #_(t/cast string?) 99] #_(t/satisfies string?) x)
                         (let [x (if (some? nil) \"plain\" #_(t/cast string?) 99)]
                           #_(t/satisfies string?) x)
                         #_(t/satisfies ::route) {:method #_(t/any \"fixture\") 99 :path \"/\"}
                         #_(t/satisfies (s/coll-of string?)) [#_(t/cast string?) 99]")]
    (is (= (repeat 5 "TSP280-01") (codes result)) (pr-str (:diagnostics result)))
    (is (= [#{:unknown} #{:cast} #{:cast} #{:any} #{:cast}] (mapv :reasons (:diagnostics result))))
    (is (= {:cast 3 :any 1 :ignore 0} (:trust result)))))

(deftest approximation-is-visible-in-explicit-contracts
  (let [result (checked "#_(t/satisfies (s/tuple int? string?)) [\"reversed\" 1]
                         #_(t/satisfies (s/coll-of int? :min-count 2)) []
                         #_(t/satisfies (s/or :a (s/and (s/nilable string?) string?) :b int?)) nil
                         #_(t/satisfies pos-int?) -1
                         #_(t/satisfies true?) false
                         (defn assumed? {:t/guard 'string?} [x] (number? x))
                         (defn consumes [x] (when (assumed? x) #_(t/satisfies string?) x))")]
    (is (= (concat ["TSP2345" "TSP330-01"] (repeat 4 "TSP280-01")) (codes result)) (pr-str (:diagnostics result)))
    (is (= (concat [nil nil] (repeat 3 #{:unsupported-contract}) [#{:trusted-guard}])
           (mapv :reasons (:diagnostics result))))))

(deftest nested-platform-values-and-qualified-key-diagnostics
  (let [result (checked "#_(t/satisfies ::route) {:method :get :path (System/getProperty \"home\")}
                         (s/def ::title string?)
                         (s/def ::named (s/keys :req [::title]))
                         #_(t/satisfies ::named) {::title 42}")]
    (is (= ["TSP2322" "TSP2322"] (codes result)) (pr-str (:diagnostics result)))))

(deftest checks-the-same-ast-deterministically
  (let [file (parse/parse-string "literal.clj" (str declarations "#_(t/satisfies ::route) {:method :get}"))
        check #(check/check-files [file] {})]
    (is (= (select-keys (check) [:diagnostics :types :trust])
           (select-keys (check) [:diagnostics :types :trust])))))
