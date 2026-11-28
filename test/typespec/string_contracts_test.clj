(ns typespec.string-contracts-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [typespec.check :as check]
            [typespec.main :as main]
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
                "(blank? nil)" "(blank? false)" "(s/blank? (StringBuilder.))"
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
  (doseq [form ["(s/upper-case nil)" "(s/lower-case nil)" "(s/capitalize nil)"
                "(s/trim nil)" "(s/reverse 42)" "(blank? true)" "(blank? 0)"
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

(deftest case-conversions-stringify-nonnil-values
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/fdef display-flag :args (s/cat) :ret boolean?)
(defn display-flag [] false)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a] [clojure.string :as s]))
(s/upper-case :label)
(s/upper-case (a/display-flag))
(s/lower-case 42)
(s/capitalize :HELLO)
(s/lower-case nil)
(s/reverse 42)")
        diagnostics (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 6 "TSP2345"] ["client.clj" 7 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics)))
  (is (= [":LABEL" "FALSE" "42" ":hello"]
         [(str/upper-case :label) (str/upper-case false) (str/lower-case 42) (str/capitalize :HELLO)]))
  (is (thrown? NullPointerException (str/lower-case nil)))
  (is (thrown? ClassCastException (str/reverse 42))))

(deftest blank-accepts-false-from-another-namespace
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/fdef disabled :args (s/cat) :ret false?)
(defn disabled [] false)
(s/fdef flag :args (s/cat) :ret boolean?)
(defn flag [] true)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a] [clojure.string :as s]))
(s/blank? (a/disabled))
(s/blank? (a/flag))")
        diagnostics (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 3 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics)))
  (is (true? (str/blank? false)))
  (is (thrown? ClassCastException (str/blank? true))))

(deftest join-consumes-primitive-and-reference-arrays
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.string :as str]))
(defn ^String render [^String/1 parts] (str/join \",\" parts))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a] [clojure.string :as str]))
(a/render (into-array String [\"a\" \"b\"]))
(str/join (int-array [1 2]))
(str/join \",\" (long-array [1 2]))
(str/join (into-array Object [:k 2]))
(str/join nil)
(str/join 42)
(str/join \",\" (java.util.Date.))")
        diagnostics (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 7 "TSP2345"] ["client.clj" 8 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics)))
  (is (= ["12" "1,2" ":k2" ""]
         [(str/join (int-array [1 2])) (str/join "," (long-array [1 2]))
          (str/join (into-array Object [:k 2])) (str/join nil)]))
  (is (thrown? IllegalArgumentException (str/join 42))))

(deftest trimming-variants-check-char-sequences-and-return-strings
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s] [clojure.string :as str]))
(s/fdef trim-left :args (s/cat :text java.lang.CharSequence) :ret string?)
(defn trim-left [text] (str/triml text))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a] [clojure.string :as str]))
(str/trim (a/trim-left (StringBuilder. \" a\")))
(str/trim (str/trimr (StringBuilder. \"a \")))
(str/trim (str/trim-newline (StringBuilder. \"a\\n\\r\")))
(str/triml 42)
(str/trimr nil)
(str/trim-newline false)")
        diagnostics (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 5 "TSP2345"] ["client.clj" 6 "TSP2345"] ["client.clj" 7 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics)))
  (is (= ["a" "a" "a"] [(str/triml (StringBuilder. " a")) (str/trimr (StringBuilder. "a "))
                        (str/trim-newline (StringBuilder. "a\n\r"))]))
  (is (thrown? ClassCastException (str/triml 42)))
  (is (thrown? NullPointerException (str/trimr nil))))

(deftest replace-first-checks-replacement-types-by-match-kind
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s] [clojure.string :as str]))
(s/fdef label :args (s/cat :v some?) :ret string?)
(defn label [value] (str/replace-first value \"x\" \"_\"))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a] [clojure.string :as str]))
(str/trim (a/label :x))
(str/replace-first \"abc\" \\a nil)
(str/replace-first \"abc\" \"a\" 42)
(str/replace-first \"abc\" #\"a\" (fn [_] \"x\"))
(str/replace-first \"abc\" \"a\" nil)
(str/replace-first \"abc\" #\"a\" (fn [_] 42))
(str/replace-first \"abc\" 42 \"x\")
(str/replace-first nil \\a \\x)")
        diagnostics (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 6 "TSP2345"] ["client.clj" 7 "TSP2345"]
            ["client.clj" 8 "TSP2345"] ["client.clj" 9 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics)))
  (is (= "bc" (str/replace-first "abc" \a nil)))
  (is (= "42bc" (str/replace-first "abc" "a" 42)))
  (is (= "xbc" (str/replace-first "abc" #"a" (fn [_] "x"))))
  (is (thrown? NullPointerException (str/replace-first "abc" "a" nil)))
  (is (thrown? ClassCastException (str/replace-first "abc" #"a" (fn [_] 42)))))

(deftest string-index-contracts-check-search-inputs-and-nullable-results
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s] [clojure.string :as str]))
(s/fdef needle-index :args (s/cat :text some? :needle char?) :ret (s/nilable int?))
(defn needle-index [text needle] (str/index-of text needle))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a] [clojure.string :as str]))
(when-some [i (a/needle-index :text \\t)] (inc i))
(when-some [i (str/last-index-of (StringBuilder. \"aaa\") \"a\" 1.5)] (inc i))
(inc (str/index-of \"abc\" \"missing\"))
(str/index-of \"abc\" (StringBuilder. \"a\"))
(str/last-index-of \"abc\" \"a\" \"0\")
(str/index-of nil \\a)")
        diagnostics (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 4 "TSP2531"] ["client.clj" 5 "TSP2345"]
            ["client.clj" 6 "TSP2345"] ["client.clj" 7 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics)))
  (is (= 1 (str/index-of :text \t)))
  (is (= 1 (str/last-index-of (StringBuilder. "aaa") "a" 1.5)))
  (is (nil? (str/index-of "abc" "missing")))
  (is (thrown? ClassCastException (str/index-of "abc" (StringBuilder. "a")))))

(deftest quoted-replacement-converts-non-nil-inputs-to-strings
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s] [clojure.string :as str]))
(s/fdef literal-replacement :args (s/cat :value some?))
(defn literal-replacement [value] (str/re-quote-replacement value))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a] [clojure.string :as str]))
(str/trim (a/literal-replacement \"$1\"))
(str/replace \"abc\" #\"b\" (str/re-quote-replacement (StringBuilder. \"$1\")))
(str/re-quote-replacement 42)
(a/literal-replacement nil)
(inc (a/literal-replacement \"$1\"))")
        diagnostics (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 5 "TSP2345"] ["client.clj" 6 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics)))
  (is (= "a$1c" (str/replace "abc" #"b" (str/re-quote-replacement (StringBuilder. "$1")))))
  (is (= "42" (str/re-quote-replacement 42)))
  (is (thrown? NullPointerException (str/re-quote-replacement nil))))

(deftest spit-returns-nil
  (is (= ["TSP2531"] (mapv :code (diagnostics "(subs (spit (java.io.StringWriter.) \"ab\") 0)"))))
  (let [writer (java.io.StringWriter.)]
    (is (nil? (spit writer "text")))
    (is (= "text" (.toString writer)))))

(deftest regex-find-infers-nullable-matches-and-capture-positions
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/fdef head :args (s/cat :source java.lang.CharSequence) :ret (s/nilable string?))
(defn head [source] (re-find #\"[^-]+\" source))
(s/fdef groups :args (s/cat :source java.lang.CharSequence) :ret (s/nilable (s/tuple string? (s/nilable string?))))
(defn groups [source] (re-find #\"(a)?b\" source))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a] [clojure.string :as str]))
(when-some [head (a/head (StringBuilder. \"part-2\"))] (str/trim head))
(when-let [[whole optional] (re-find #\"(a)?b\" \"b\")] (str/trim whole))
(when-let [head (re-find #\"(?:x)+\" \"xx\")] (str/trim head))
(when-let [[whole optional] (re-find #\"(a)?b\" \"b\")] (str/trim optional))
(str/trim (re-find #\"x\" \"empty\"))
(re-find \"x\" \"x\")
(re-find #\"x\" 42)
(re-find #\"x\")
(re-find (re-matcher #\"(x)\" \"x\"))
(when-let [[whole optional] (a/groups \"b\")] (str/trim whole))")
        diagnostics (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 5 "TSP2531"] ["client.clj" 6 "TSP2531"] ["client.clj" 7 "TSP2345"]
            ["client.clj" 8 "TSP2345"] ["client.clj" 9 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics)))
  (is (= "part" (re-find #"[^-]+" (StringBuilder. "part-2"))))
  (is (= ["b" nil] (re-find #"(a)?b" "b")))
  (is (= "xx" (re-find #"(?:x)+" "xx")))
  (is (nil? (re-find #"x" "empty")))
  (is (thrown? ClassCastException (re-find #"x" 42))))

(deftest regex-matcher-carries-its-class-across-namespaces
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/fdef matcher :args (s/cat :source java.lang.CharSequence) :ret java.util.regex.Matcher)
(defn matcher [source] (re-matcher #\"(x)\" source))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as a]))
(.find ^java.util.regex.Matcher (a/matcher (StringBuilder. \"x\")))
(.find (re-matcher #\"x\" \"x\"))
(re-matcher \"x\" \"x\")
(re-matcher #\"x\" 42)
(re-matcher #\"x\" nil)
(re-matcher #\"x\")
(a/matcher 42)")
        diagnostics (:diagnostics (check/check-files [client api] {}))]
    (is (= [["client.clj" 4 "TSP2345"] ["client.clj" 5 "TSP2345"] ["client.clj" 6 "TSP2345"]
            ["client.clj" 7 "TSP2554"] ["client.clj" 8 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) :code) diagnostics))
        (pr-str diagnostics)))
  (is (.find (re-matcher #"x" (StringBuilder. "x"))))
  (is (thrown? ClassCastException (re-matcher #"x" 42)))
  (is (thrown? NullPointerException (re-matcher #"x" nil))))

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

(deftest namespace-uses-known-keyword-and-symbol-qualification
  (let [file (parse/parse-string "names.clj" "(ns names (:require [clojure.string :as str]))
(namespace :space/key)
(namespace 'space/symbol)
(clojure.core/namespace :unqualified)
(str/upper-case (namespace :space/key))
(str/upper-case (namespace 'unqualified))
(namespace true)
(namespace)")
        result (check/check-files [file] {})]
    (is (= ["\"space\"" "\"space\"" "nil?"]
           (mapv #(main/type-at result "names.clj" % 1) [2 3 4])))
    (is (= [[6 "TSP2531"] [7 "TSP2345"] [8 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics result)))))
  (is (= "space" (namespace 'space/symbol)))
  (is (nil? (namespace :unqualified)))
  (is (thrown? NullPointerException (str/upper-case (namespace :unqualified)))))

(deftest name-preserves-the-local-part-of-known-names
  (let [file (parse/parse-string "names.clj" "(ns names)
(name \"as-is\")
(name :space/local)
(name 'space/local)
(name :plain)
(name 42)
(name :a :b)")
        result (check/check-files [file] {})]
    (is (= ["\"as-is\"" "\"local\"" "\"local\"" "\"plain\""]
           (mapv #(main/type-at result "names.clj" % 1) [2 3 4 5])))
    (is (= [[6 "TSP2345"] [7 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics result)))))
  (is (= "local" (name 'space/local)))
  (is (thrown? ClassCastException (name 42))))

(deftest parse-long-keeps-known-integers-and-failed-parses
  (let [file (parse/parse-string "parsing.clj" "(ns parsing)
(parse-long \"+42\")
(parse-long \"-9223372036854775808\")
(parse-long \"9223372036854775808\")
(inc (parse-long \"42\"))
(inc (parse-long \" 42\"))
(parse-long 42)
(parse-long)")
        result (check/check-files [file] {})]
    (is (= ["42" "-9223372036854775808" "nil?"]
           (mapv #(main/type-at result "parsing.clj" % 1) [2 3 4])))
    (is (= [[6 "TSP2531"] [7 "TSP2345"] [8 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics result)))))
  (is (= 42 (parse-long "+42")))
  (is (nil? (parse-long "9223372036854775808")))
  (is (thrown? NullPointerException (inc (parse-long " 42")))))

(deftest parse-double-distinguishes-valid-floating-point-strings
  (let [file (parse/parse-string "parsing.clj" "(ns parsing)
(parse-double \" 0x1.0p2 \")
(parse-double \"NaN\")
(parse-double \"Infinity\")
(parse-double \"broken\")
(+ (parse-double \"3.25\") 1.0)
(+ (parse-double \"broken\") 1.0)
(parse-double :broken)")
        result (check/check-files [file] {})]
    (is (= ["4.0" "double?" "##Inf" "nil?"]
           (mapv #(main/type-at result "parsing.clj" % 1) [2 3 4 5])))
    (is (= [[7 "TSP2531"] [8 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics result)))))
  (is (= 4.0 (parse-double " 0x1.0p2 ")))
  (is (Double/isNaN (parse-double "NaN")))
  (is (thrown? NullPointerException (+ (parse-double "broken") 1.0))))

(deftest parse-boolean-keeps-false-separate-from-invalid-input
  (let [file (parse/parse-string "parsing.clj" "(ns parsing)
(parse-boolean \"true\")
(parse-boolean \"false\")
(parse-boolean \"FALSE\")
(if-some [flag (parse-boolean \"false\")] (if flag :on :off) :invalid)
(parse-boolean 42)
(parse-boolean \"true\" \"false\")")
        result (check/check-files [file] {})]
    (is (= ["true" "false" "nil?" ":off"]
           (mapv #(main/type-at result "parsing.clj" % 1) [2 3 4 5])))
    (is (= [[6 "TSP2345"] [7 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics result)))))
  (is (false? (parse-boolean "false")))
  (is (nil? (parse-boolean "FALSE")))
  (is (thrown? IllegalArgumentException (parse-boolean 42))))

(deftest parse-uuid-distinguishes-valid-input-before-interop
  (let [file (parse/parse-string "parsing.clj" "(ns parsing)
(parse-uuid \"b20c1fd0-5fe9-4ccb-81a1-51d0ad221049\")
(parse-uuid \"1-1-1-1-1\")
(parse-uuid \"invalid\")
(.version (parse-uuid \"b20c1fd0-5fe9-4ccb-81a1-51d0ad221049\"))
(.version (parse-uuid \"invalid\"))
(parse-uuid :bad)")
        result (check/check-files [file] {})]
    (is (= ["uuid?" "uuid?" "nil?"]
           (mapv #(main/type-at result "parsing.clj" % 1) [2 3 4])))
    (is (= [[6 "TSP2531"] [7 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics result)))))
  (is (uuid? (parse-uuid "1-1-1-1-1")))
  (is (nil? (parse-uuid "invalid")))
  (is (thrown? NullPointerException (.version (parse-uuid "invalid")))))

(deftest keyword-qualification-carries-through-specs-guards-and-filter
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/fdef ns-of :args (s/cat :x qualified-keyword?) :ret string?)
(defn ns-of [x] (namespace x))
(s/fdef words :args (s/cat) :ret (s/coll-of keyword? :kind vector?))
(defn words [] [:space/x :plain])")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api] [clojure.spec.alpha :as s] [clojure.string :as str]))
(api/ns-of :space/key)
(api/ns-of :plain)
(s/fdef label :args (s/cat :x keyword?) :ret (s/nilable string?))
(defn label [x] (let [ok (qualified-keyword? x)] (when ok (str/upper-case (namespace x)))))
(mapv #(str/upper-case (namespace %)) (filter qualified-keyword? (api/words)))
(s/fdef mistake :args (s/cat :x keyword?) :ret (s/nilable string?))
(defn mistake [x] (when (simple-keyword? x) (str/upper-case (namespace x))))
(qualified-keyword? :space/key)
(qualified-keyword? :plain)
(simple-keyword? :plain)
(qualified-keyword? :a :b)")
        result (check/check-files [api client] {})]
    (is (= [[3 "TSP2345"] [8 "TSP2531"] [12 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))
    (is (= ["true" "false" "true"]
           (mapv #(main/type-at result "client.clj" % 1) [9 10 11]))))
  (is (= ["SPACE"] (mapv #(str/upper-case (namespace %)) (filter qualified-keyword? [:space/x :plain]))))
  (is (thrown? NullPointerException (str/upper-case (namespace :plain)))))

(deftest symbol-qualification-carries-through-contracts-and-narrowing
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/fdef scoped :args (s/cat) :ret qualified-symbol?)
(defn scoped [] 'scope/value)
(s/fdef source :args (s/cat :x simple-symbol?) :ret symbol?)
(defn source [x] x)")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api] [clojure.spec.alpha :as s] [clojure.string :as str]))
(str/upper-case (namespace (api/scoped)))
(api/source 'plain)
(api/source 'scope/value)
(s/fdef use-ns :args (s/cat :x symbol?) :ret (s/nilable string?))
(defn use-ns [x] (when-not (simple-symbol? x) (str/upper-case (namespace x))))
(qualified-symbol? 'scope/value)
(simple-symbol? 'scope/value)
(simple-symbol? :plain)
(str/upper-case (namespace (api/source 'plain)))")
        result (check/check-files [api client] {})]
    (is (= [[4 "TSP2345"] [10 "TSP2531"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))
    (is (= ["true" "false" "false"]
           (mapv #(main/type-at result "client.clj" % 1) [7 8 9]))))
  (is (qualified-symbol? 'scope/value))
  (is (simple-symbol? 'plain))
  (is (thrown? NullPointerException (str/upper-case (namespace 'plain)))))

(deftest keyword-construction-retains-the-namespace-and-nil-result
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/fdef tag :args (s/cat :text string?) :ret qualified-keyword?)
(defn tag [text] (keyword \"space\" text))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api] [clojure.string :as str]))
(namespace (keyword \"space/label\"))
(namespace (keyword 'space/label))
(namespace (keyword nil \"space/label\"))
(name (keyword nil \"space/label\"))
(namespace (keyword \"\" \"label\"))
(keyword 42)
(keyword nil)
(str/upper-case (namespace (api/tag \"label\")))
(str/upper-case (namespace (keyword nil \"label\")))
(keyword 42 \"label\")
(keyword \"label\" nil)
(keyword)")
        result (check/check-files [api client] {})]
    (is (= ["\"space\"" "\"space\"" "nil?" "\"space/label\"" "\"\"" "nil?" "nil?"]
           (mapv #(main/type-at result "client.clj" % 1) (range 2 9))))
    (is (= [[10 "TSP2531"] [11 "TSP2345"] [12 "TSP2345"] [13 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result))))
  (is (nil? (keyword 42)))
  (is (nil? (namespace (keyword nil "space/label"))))
  (is (= "" (namespace (keyword "" "label"))))
  (is (thrown? NullPointerException (keyword "label" nil))))

(deftest symbol-construction-retains-names-and-supports-vars
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/fdef make-name :args (s/cat :scope string? :text string?) :ret qualified-symbol?)
(defn make-name [scope text] (symbol scope text))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api] [clojure.string :as str]))
(namespace (symbol :space/local))
(name (symbol \"space/local\"))
(namespace (symbol nil \"space/local\"))
(namespace (symbol #'api/make-name))
(str/upper-case (namespace (api/make-name \"scope\" \"name\")))
(str/upper-case (namespace (symbol nil \"plain\")))
(symbol 42)
(symbol)")
        result (check/check-files [api client] {})]
    (is (= ["\"space\"" "\"local\"" "nil?" "string?"]
           (mapv #(main/type-at result "client.clj" % 1) (range 2 6))))
    (is (= [[7 "TSP2531"] [8 "TSP2345"] [9 "TSP2554"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result))))
  (is (= 'clojure.core/inc (symbol #'inc)))
  (is (nil? (namespace (symbol nil "space/local"))))
  (is (thrown? IllegalArgumentException (symbol 42))))

(deftest qualified-ident-covers-keywords-and-symbols-in-specs-and-guards
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/fdef ns-of :args (s/cat :x qualified-ident?) :ret string?)
(defn ns-of [x] (namespace x))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api] [clojure.string :as str]))
(str/upper-case (api/ns-of :space/key))
(str/upper-case (api/ns-of 'space/key))
(api/ns-of :plain)
(api/ns-of 'plain)
(defn indexed [x] (when (qualified-ident? x) (str/upper-case (namespace x))))
(qualified-ident? 42)
(qualified-ident? :space/key)
(qualified-ident? 'space/key)")
        result (check/check-files [api client] {})]
    (is (= [[4 "TSP2345"] [5 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))
    (is (= ["false" "true" "true"]
           (mapv #(main/type-at result "client.clj" % 1) [7 8 9]))))
  (is (every? qualified-ident? [:space/key 'space/key]))
  (is (not-any? qualified-ident? [:plain 'plain 42 nil])))

(deftest simple-ident-proves-the-namespace-absent-for-either-kind
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/fdef simple-ns :args (s/cat :x simple-ident?) :ret nil?)
(defn simple-ns [x] (namespace x))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api] [clojure.string :as str]))
(api/simple-ns :plain)
(api/simple-ns 'plain)
(api/simple-ns :space/key)
(simple-ident? (symbol nil \"space/key\"))
(simple-ident? (keyword \"\" \"key\"))
(mapv name (filter simple-ident? [:space/key 'plain]))
(str/upper-case (api/simple-ns 'plain))")
        result (check/check-files [api client] {})]
    (is (= [[4 "TSP2345"] [8 "TSP2531"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))
    (is (= ["true" "false"]
           (mapv #(main/type-at result "client.clj" % 1) [5 6]))))
  (is (every? simple-ident? [:plain 'plain]))
  (is (not-any? simple-ident? [:space/key 'space/key 42 nil])))

(deftest ident-spec-and-guard-accept-keywords-and-symbols
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s] [clojure.core :as core]))
(s/fdef label :args (s/cat :x core/ident?) :ret string?)
(defn label [x] (name x))")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api] [clojure.string :as str]))
(api/label :label)
(api/label 'label)
(api/label \"label\")
(defn guarded [x] (when (ident? x) (str/upper-case (name x))))
(ident? :x)
(ident? 'x)
(ident? \"x\")")
        result (check/check-files [api client] {})]
    (is (= [[4 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))
    (is (= ["true" "true" "false"]
           (mapv #(main/type-at result "client.clj" % 1) [6 7 8]))))
  (let [shadowed (parse/parse-string "shadowed.clj" "(ns shadowed
  (:refer-clojure :exclude [ident?])
  (:require [clojure.spec.alpha :as s]))
(defn ident? [x] (string? x))
(s/def ::custom ident?)")
        result (check/check-files [shadowed] {})]
    (is (= ["TSP9002"] (mapv :code (:diagnostics result)))))
  (is (every? ident? ['value :key]))
  (is (not-any? ident? ["value" 42 nil])))

(deftest namespace-as-a-callback-keeps-qualification-from-the-input-spec
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/fdef tags :args (s/cat) :ret (s/coll-of qualified-ident? :kind vector?))
(defn tags [] [])
(s/fdef plain :args (s/cat) :ret (s/coll-of simple-ident? :kind vector?))
(defn plain [] [])")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api] [clojure.string :as str]))
(mapv clojure.core/namespace (api/tags))
(mapv namespace (api/plain))
(mapv str/upper-case (mapv namespace (api/tags)))
(mapv str/upper-case (mapv namespace (api/plain)))
(mapv namespace [42])
(let [namespace (fn [_] 42)] (mapv namespace (api/tags)))")
        result (check/check-files [api client] {})]
    (is (= [[5 "TSP2531"] [6 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))
    (is (= ["(s/coll-of string? :kind vector?)" "(s/coll-of nil? :kind vector?)"
            "(s/coll-of int? :kind vector?)"]
           (mapv #(main/type-at result "client.clj" % 1) [2 3 7]))))
  (is (= ["space" "scope"] (mapv namespace [:space/key 'scope/key])))
  (is (thrown? ClassCastException (mapv namespace [42]))))

(deftest namespace-as-a-predicate-tracks-the-qualification-of-identifiers
  (let [file (parse/parse-string "filter.clj" "(ns filtering (:require [clojure.spec.alpha :as s] [clojure.string :as str]))
(s/fdef labels :args (s/cat :xs (s/coll-of ident?)) :ret (s/coll-of string? :kind vector?))
(defn labels [xs] (mapv #(str/upper-case (namespace %)) (filter namespace xs)))
(s/fdef wrong :args (s/cat :xs (s/coll-of ident?)) :ret (s/coll-of string? :kind vector?))
(defn wrong [xs] (mapv #(str/upper-case (namespace %)) (remove namespace xs)))
(s/fdef unknown :args (s/cat :xs (s/coll-of clojure.lang.Named)) :ret (s/coll-of string? :kind vector?))
(defn unknown [xs] (mapv #(str/upper-case (namespace %)) (filter namespace xs)))")
        result (check/check-files [file] {})]
    (is (= [[5 "TSP2531"] [7 "TSP2531"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result))))
  (is (= [:space/key 'scope/key (keyword "" "key")]
         (vec (filter namespace [:space/key :plain 'scope/key 'plain (keyword "" "key")]))))
  (is (thrown? NullPointerException (mapv #(str/upper-case (namespace %)) (remove namespace [:plain])))))

(deftest testing-namespace-refines-immutable-identifiers
  (let [file (parse/parse-string "guard.clj" "(ns guard (:require [clojure.spec.alpha :as s] [clojure.string :as str]))
(s/fdef good :args (s/cat :x ident?) :ret (s/nilable string?))
(defn good [x] (when (namespace x) (str/upper-case (namespace x))))
(s/fdef opposite :args (s/cat :x ident?) :ret (s/nilable string?))
(defn opposite [x] (when-not (namespace x) (str/upper-case (namespace x))))
(s/fdef unknown :args (s/cat :x clojure.lang.Named) :ret (s/nilable string?))
(defn unknown [x] (when (namespace x) (str/upper-case (namespace x))))")
        result (check/check-files [file] {})]
    (is (= [[5 "TSP2531"] [7 "TSP2531"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result))))
  (is (= "SPACE" (when (namespace :space/key) (str/upper-case (namespace :space/key)))))
  (let [names (atom ["space" nil])
        unstable (reify clojure.lang.Named
                   (getName [_] "key")
                   (getNamespace [_] (first (first (swap-vals! names next)))))]
    (is (thrown? NullPointerException
                 (when (namespace unstable) (str/upper-case (namespace unstable)))))))

(deftest nil-tests-on-namespace-refine-the-original-identifier
  (let [file (parse/parse-string "guard.clj" "(ns guard (:require [clojure.spec.alpha :as s] [clojure.string :as str]))
(s/fdef bound :args (s/cat :x ident?) :ret (s/nilable string?))
(defn bound [x] (when-some [ns (namespace x)] (str ns (str/upper-case (namespace x)))))
(s/fdef selected :args (s/cat :x ident?) :ret string?)
(defn selected [x] (if (nil? (namespace x)) \"\" (str/upper-case (namespace x))))
(s/fdef wrong :args (s/cat :x ident?) :ret (s/nilable string?))
(defn wrong [x] (when-not (some? (namespace x)) (str/upper-case (namespace x))))")
        result (check/check-files [file] {})]
    (is (= [[7 "TSP2531"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result))))
  (is (= "spaceSPACE" (when-some [ns (namespace :space/key)] (str ns (str/upper-case (namespace :space/key))))))
  (is (thrown? NullPointerException (when-not (some? (namespace :plain)) (str/upper-case (namespace :plain))))))

(deftest parse-long-as-a-callback-uses-the-literal-element-contract
  (let [api (parse/parse-string "api.clj" "(ns api (:require [clojure.spec.alpha :as s]))
(s/fdef values :args (s/cat) :ret (s/coll-of #{\"0\" \"2\"} :kind vector? :min-count 1))
(defn values [] [\"0\" \"2\"])
(s/fdef mixed :args (s/cat) :ret (s/coll-of #{\"2\" \"bad\"} :kind vector? :min-count 1))
(defn mixed [] [\"2\" \"bad\"])")
        client (parse/parse-string "client.clj" "(ns client (:require [api :as api]))
(mapv parse-long (api/values))
(mapv inc (mapv parse-long (api/values)))
(mapv inc (mapv parse-long (api/mixed)))
(some clojure.core/parse-long (api/values))
(mapv parse-long [42])
(let [parse-long (fn [_] :local)] (mapv parse-long (api/values)))")
        result (check/check-files [api client] {})]
    (is (= [[4 "TSP2531"] [6 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))
    (is (= ["(s/coll-of int? :kind vector? :min-count 1)" "#{0 2}" "(s/coll-of keyword? :kind vector? :min-count 1)"]
           (mapv #(main/type-at result "client.clj" % 1) [2 5 7]))))
  (is (= [0 2] (mapv parse-long ["0" "2"])))
  (is (thrown? NullPointerException (mapv inc (mapv parse-long ["bad"])))))

(deftest string-parser-callbacks-share-the-direct-call-result-types
  (let [file (parse/parse-string "parsing.clj" "(ns parsing (:require [clojure.spec.alpha :as s]))
(s/fdef doubles :args (s/cat :values (s/coll-of #{\"1.25\" \"NaN\"} :kind vector?)) :ret (s/coll-of double? :kind vector?))
(defn doubles [values] (mapv parse-double values))
(s/fdef malformed :args (s/cat :values (s/coll-of #{\"1.25\" \"bad\"} :kind vector?)) :ret (s/coll-of double? :kind vector?))
(defn malformed [values] (mapv parse-double values))
(s/fdef flags :args (s/cat :values (s/coll-of #{\"false\"} :kind vector? :min-count 1)) :ret nil?)
(defn flags [values] (some parse-boolean values))
(s/fdef uuids :args (s/cat :values (s/coll-of #{\"1-1-1-1-1\"} :kind vector?)) :ret (s/coll-of uuid? :kind vector?))
(defn uuids [values] (mapv parse-uuid values))
(s/fdef malformed-uuids :args (s/cat :values (s/coll-of #{\"bad\"} :kind vector?)) :ret (s/coll-of uuid? :kind vector?))
(defn malformed-uuids [values] (mapv parse-uuid values))")
        result (check/check-files [file] {})]
    (is (= [[5 "TSP2322"] [11 "TSP2322"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result))))
  (is (= [1.25 nil] (mapv parse-double ["1.25" "bad"])))
  (is (nil? (some parse-boolean ["false"])))
  (is (every? uuid? (mapv parse-uuid ["1-1-1-1-1"])))
  (is (= [nil] (mapv parse-uuid ["bad"]))))

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
            (diagnostics "(replace \"abc\" #\"(a)\" (fn [x] (s/trim x)))"))))

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
