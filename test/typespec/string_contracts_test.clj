(ns typespec.string-contracts-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
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
                "(join nil)" "(join [1 nil :a])" "(join #{1 2})"
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
                "(replace \"abc\" \"a\")" "(s/blank? nil nil)"]]
    (let [ds (diagnostics form)]
      (is (some #(and (= :error (:severity %))
                      (#{"TSP2345" "TSP2554" "TSP2322"} (:code %))) ds)
          (str form " => " (pr-str ds))))))

(deftest honest-incomplete-boundaries
  ;; No group-count specialization, and quoted sequence inference is incomplete.
  (doseq [form ["(join '(1 2))"
                "(replace \"abc\" #\"(a)\" (fn [x] (s/upper-case x)))"]]
    (is (some #(= "TSP210-01" (:code %)) (diagnostics form)) form)))

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
