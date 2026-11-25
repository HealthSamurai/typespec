(ns typespec.string-contracts-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [typespec.check :as check]
            [typespec.parse :as parse]
            [typespec.externs-regression-test :refer [project]]))

(defn diagnostics [form]
  (let [out (atom nil)]
    (project (str "(ns app (:require [clojure.string :as s :refer [join replace blank?]]))\n" form)
             "{}" {} #(reset! out (:diagnostics %)))
    @out))

(deftest accepted-contracts
  (doseq [form ["(s/upper-case (StringBuilder. \"ab\"))"
                "(s/lower-case (StringBuilder. \"AB\"))"
                "(s/capitalize (StringBuilder. \"ab\"))"
                "(s/trim (StringBuilder. \" ab \"))"
                "(s/reverse (StringBuilder. \"ab\"))"
                "(blank? nil)" "(s/blank? (StringBuilder.))"
                "(s/split (StringBuilder. \"a,b\") #\",\")"
                "(s/split \"a,b\" #\",\" 2)"
                "(s/split-lines (StringBuilder. \"a\\nb\"))"
                "(s/starts-with? (StringBuilder. \"ab\") \"a\")"
                "(s/ends-with? (StringBuilder. \"ab\") \"b\")"
                "(s/includes? (StringBuilder. \"ab\") (StringBuilder. \"b\"))"
                "(s/upper-case (slurp (java.io.StringReader. \"ab\")))"
                "(s/upper-case (clojure.core/slurp (java.io.File. \"body.txt\") :encoding \"UTF-8\"))"
                "(join nil)" "(join [1 nil :a])" "(join #{1 2})" "(join '(1 2))"
                "(join {:a 1})" "(join (range 2))" "(join \"ab\")"
                "(join (StringBuilder. \"ab\"))"
                "(join (java.util.ArrayList.))" "(join (java.util.HashMap.))"
                "(join 42 [1 2])" "(join nil [1 2])"
                "(join (Object.) [1 2])" "(str (Object.))" "(str (new Object))"
                "(replace \"abc\" \\a \\x)"
                "(replace (StringBuilder. \"abc\") (StringBuilder. \"a\") (StringBuilder. \"x\"))"
                "(replace \"abc\" #\"a\" (StringBuilder. \"x\"))"
                "(replace \"b\" #\"(a)?b\" (fn [groups] (str groups)))"
                "(replace \"a\" #\"a\" (fn [match] \"x\"))"]]
    (let [ds (diagnostics form)]
      (is (empty? ds) (str form " => " (pr-str ds))))))

(deftest rejected-contracts
  (doseq [form ["(s/upper-case nil)" "(s/lower-case 42)" "(s/capitalize false)"
                "(s/trim nil)" "(s/reverse 42)" "(blank? false)"
                "(join 42)" "(join false)" "(join (Object.))" "(join (new Object))"
                "(join (java.lang.Object.))" "(join (new java.lang.Object))"
                "(s/split \"abc\" \",\")" "(s/split nil #\",\")"
                "(s/split \"abc\" #\",\" 1.5)" "(s/split-lines nil)"
                "(s/starts-with? \"abc\" (StringBuilder. \"a\"))"
                "(s/ends-with? \"abc\" nil)" "(s/includes? nil \"a\")"
                "(replace \"abc\" \\a \"x\")" "(replace \"abc\" \"a\" \\x)"
                "(replace \"abc\" nil \"x\")" "(replace \"abc\" #\"a\" 42)"
                "(replace \"abc\" #\"a\" (fn [x] 42))"
                "(replace \"abc\" #\"a\" (fn [x] nil))"
                "(replace \"abc\" #\"a\" (fn [x] (StringBuilder. \"x\")))"
                "(join)" "(join 1 2 3)" "(s/split \"abc\")"
                "(replace \"abc\" \"a\")" "(s/blank? nil nil)"
                "(slurp)" "(spit \"body.txt\")"
                "(inc (slurp (java.io.StringReader. \"ab\")))"]]
    (let [ds (diagnostics form)]
      (is (some #(and (= :error (:severity %))
                      (#{"TSP2345" "TSP2554" "TSP2322"} (:code %))) ds)
          (str form " => " (pr-str ds))))))

(deftest substring-predicates-stringify-only-the-receiver
  (let [api (parse/parse-string
             "source.clj"
             "(ns source (:require [clojure.spec.alpha :as s]))
(s/fdef flag :args (s/cat) :ret boolean?)
(defn flag [] false)")
        client (parse/parse-string
                "client.clj"
                "(ns client (:require [source :as source] [clojure.string :as s]))
(s/starts-with? 42 \"4\")
(s/ends-with? :ready \"dy\")
(s/includes? (source/flag) (StringBuilder. \"als\"))
(s/starts-with? nil \"\")
(s/ends-with? \"abc\" (StringBuilder. \"c\"))
(s/includes? \"abc\" :a)")
        ds (:diagnostics (check/check-files [api client] {:strict-nil true}))]
    (is (= [[5 "TSP2345"] [6 "TSP2345"] [7 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) ds))
        (pr-str ds)))
  (is (str/starts-with? 42 "4"))
  (is (str/ends-with? :ready "dy"))
  (is (str/includes? false (StringBuilder. "als")))
  (is (thrown? NullPointerException (str/starts-with? nil "")))
  (is (thrown? ClassCastException (str/ends-with? "abc" (StringBuilder. "c"))))
  (is (thrown? ClassCastException (str/includes? "abc" :a))))

(deftest spit-returns-nil
  (is (= ["TSP2531"] (mapv :code (diagnostics "(subs (spit (java.io.StringWriter.) \"ab\") 0)"))))
  (let [writer (java.io.StringWriter.)]
    (is (nil? (spit writer "text")))
    (is (= "text" (.toString writer)))))

(deftest core-naming-contracts-accept-named-across-namespaces
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/fdef label :args (s/cat :x clojure.lang.Named) :ret string?)
(defn label [x] (name x))
(s/fdef qualifier :args (s/cat :x clojure.lang.Named) :ret (s/nilable string?))
(defn qualifier [x] (namespace x))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api]))
(defn from-hint [^clojure.lang.Named x] (str (api/qualifier x) (api/label x)))
(name \"plain\")
(name :ns/name)
(namespace 'ns/name)
(name 42)
(namespace \"plain\")
(api/label \"plain\")")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 6 "TSP2345"] ["client.clj" 7 "TSP2345"] ["client.clj" 8 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues)))
  (let [named (reify clojure.lang.Named
                (getName [_] "label")
                (getNamespace [_] "example"))]
    (is (= "label" (name named)))
    (is (= "example" (namespace named))))
  (is (thrown? ClassCastException (name 42)))
  (is (thrown? ClassCastException (namespace "plain"))))

(deftest replace-and-spec-check-non-nil-values-across-namespaces
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s] [clojure.string :as str]))
(s/fdef replace-x :args (s/cat :x some?) :ret string?)
(defn replace-x [x] (str/replace x #\"x\" \"_\"))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api] [clojure.string :as str] [clojure.spec.alpha :as s]))
(api/replace-x :x)
(api/replace-x false)
(api/replace-x {:x 1})
(str/replace 42 \"4\" \"0\")
(api/replace-x nil)
(s/fdef maybe-replace :args (s/cat :x (s/nilable string?)) :ret string?)
(defn maybe-replace [x] (str/replace x #\"x\" \"_\"))")
        issues (:diagnostics (check/check-files [api client] {}))]
    (is (= [["client.clj" 6 "TSP2345"] ["client.clj" 8 "TSP2531"]]
           (mapv (juxt :file (comp :row :pos) :code) issues))
        (pr-str issues)))
  (is (= ":_" (str/replace :x #"x" "_")))
  (is (= "02" (str/replace 42 "4" "0")))
  (is (= "f0lse" (str/replace false "a" "0")))
  (is (thrown? NullPointerException (str/replace nil #"x" "_"))))

(deftest honest-incomplete-boundaries
  ;; Regex groups reach the callback as a string or a vector.
  (is (some #(= "TSP210-01" (:code %))
            (diagnostics "(replace \"abc\" #\"(a)\" (fn [x] (s/upper-case x)))"))))

(deftest replace-reports-arguments-from-the-closest-overload
  (is (= [["TSP2345" "\"_\" is not assignable to java.lang.Character"]]
         (mapv (juxt :code :message) (diagnostics "(replace 42 \\x \"_\")"))))
  (is (= [["TSP2345" "nil? is not assignable to some?"]
          ["TSP2345" "\"_\" is not assignable to java.lang.Character"]]
         (mapv (juxt :code :message) (diagnostics "(replace nil \\x \"_\")")))))

(deftest character-roundtrip
  (doseq [source ["\\a" "\\newline" "\\space" "\\u03bb"]]
    (let [node (first (:forms (parse/parse-string "char.clj" source)))]
      (is (= :char (:op node)))
      (is (= (read-string source) (parse/node->data node)))
      (is (= (read-string source) (read-string (parse/unparse node)))))))

(deftest runtime-probes-1-12-4
  (is (= "1.12.4" (clojure-version)))
  (doseq [[f input expected] [[str/upper-case "ab" "AB"] [str/lower-case "AB" "ab"]
                             [str/capitalize "ab" "Ab"] [str/trim " ab " "ab"]
                             [str/reverse "ab" "ba"] [str/blank? " " true]
                             [str/split-lines "a\nb" ["a" "b"]]]]
    (is (= expected (f (StringBuilder. input)))))
  (is (true? (str/blank? nil)))
  (is (= ["a" "b,c"] (str/split (StringBuilder. "a,b,c") #"," 2)))
  (is (str/starts-with? (StringBuilder. "ab") "a"))
  (is (str/ends-with? (StringBuilder. "ab") "b"))
  (is (str/includes? (StringBuilder. "ab") (StringBuilder. "b")))
  (is (= "a,b" (str/join "," (StringBuilder. "ab"))))
  (is (= "1422" (str/join 42 [1 2])))
  (is (= "" (str/join nil)))
  (is (= "a,b" (str/join "," (java.util.ArrayList. ["a" "b"]))))
  (is (= "xbc" (str/replace "abc" \a \x)))
  (is (= "xbc" (str/replace (StringBuilder. "abc") (StringBuilder. "a") (StringBuilder. "x"))))
  (is (= "xbc" (str/replace "abc" #"a" (StringBuilder. "x"))))
  (is (= "[\"b\" nil]" (str/replace "b" #"(a)?b" pr-str)))
  (is (= "\"a\"" (str/replace "a" #"a" pr-str)))
  (doseq [bad [42 nil (StringBuilder. "x")]]
    (is (thrown? Exception (str/replace "a" #"a" (fn [_] bad)))))
  (is (thrown? Exception (str/replace "a" \a "x")))
  (is (thrown? Exception (str/starts-with? "a" (StringBuilder. "a"))))
  (is (thrown? Exception (str/join 42))))
