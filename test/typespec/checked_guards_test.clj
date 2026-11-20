(ns typespec.checked-guards-test
  (:require [clojure.test :refer [deftest is testing]]
            [typespec.check :as check]
            [typespec.main :as main]
            [typespec.parse :as parse]))

(defn- check-source [source]
  (check/check-files
   [(parse/parse-string "guards.clj"
                        (str "(ns guards (:require [clojure.spec.alpha :as s] [clojure.string :as str]))\n" source))]
   {:strict-nil true}))

(defn- codes [result] (mapv :code (:diagnostics result)))

(deftest truthy-and-falsy-returns-are-proved-independently
  (let [result (check-source
                "(defn nonblank? [x] (and (string? x) (not (str/blank? x))))
                 (defn normalize {:t/sig '[t/unknown :-> string?]} [x]
                   (if (nonblank? x) (str/trim x) \"default\"))
                 (defn recover {:t/sig '[string? :-> string?]} [x]
                   (if (nonblank? x) x (str/trim x)))
                 (defn not-string? [x] (not (string? x)))
                 (defn read-string {:t/sig '[(s/nilable string?) :-> string?]} [x]
                   (if (not-string? x) \"default\" (str/trim x)))
                 (defn returns-string [x]
                   (let [same x matches? (string? same)]
                     (if matches? same false)))
                 (defn read-result {:t/sig '[t/unknown :-> string?]} [x]
                   (let [matches? (returns-string x)]
                     (if matches? (str/trim x) \"default\")))")]
    (is (empty? (codes result)) (pr-str (:diagnostics result)))
    (is (= :checked (get-in result [:env :guards 'guards/nonblank? :mode])))
    (is (= "string?" (main/type-at result "guards.clj" 6 52)) (pr-str (:types result))))
  (let [result (check-source
                "(defn string-or-keyword? [x] (or (string? x) (keyword? x)))
                 (defn read-value [x] (when (string-or-keyword? x) (str/trim x)))")]
    (is (= ["TSP2345"] (codes result)) (pr-str (:diagnostics result)))))

(deftest narrowing-of-elements-follows-the-filtered-path
  (let [result (check-source
                "(defn present? [x] (some? x))
                 (defn nonblank? [x] (and (string? x) (not (str/blank? x))))
                 (defn ok? [x] (= x :ok))
                 (defn wants-oks {:t/sig '[(s/coll-of #{:ok}) :-> int?]} [xs] (count xs))
                 (mapv inc (filter some? [1 nil 2]))
                 (mapv inc (filterv #(some? %) [nil 2]))
                 (mapv inc (take-while present? [1 nil 2]))
                 (mapv inc (remove nil? [1 nil 2]))
                 (let [present #(some? %)] (mapv inc (filter present [nil 1])))
                 (wants-oks (filter ok? [:ok :other]))
                 (mapv inc (remove nonblank? [\"\" \"a\" 1]))")]
    (is (= ["TSP2345"] (codes result)) (pr-str (:diagnostics result)))
    (is (= 12 (get-in result [:diagnostics 0 :pos :row]))))
  (let [result (check-source
                "(defn reads-false {:t/sig '[(s/coll-of #{false}) :-> int?]} [xs] (count xs))
                 (reads-false (filter some? (filter #(= % false) [false nil])))")]
    (is (empty? (codes result)) (pr-str (:diagnostics result)))))

(deftest explicit-contracts-and-trust
  (let [result (check-source
                "(s/def ::label string?)
                 (s/def ::entry (s/keys :req-un [::label]))
                 (s/def ::nonblank (s/and string? (complement str/blank?)))
                 (defn checked? {:t/guard '{:type string? :mode :checked :two-sided true}}
                   [x] (string? x))
                 (defn bad-shape? {:t/guard '{:type ::entry :mode :checked}}
                   [x] (map? x))
                 (defn bad-negative? {:t/guard '{:type string? :mode :checked :two-sided true}}
                   [x] (s/valid? ::nonblank x))
                 (defn trusted? {:t/guard '::entry} [x] (map? x))
                 (defn laundering? {:t/guard '{:type ::entry :mode :checked}}
                   [x] (trusted? x))
                 (defn read-entry [x] (when (trusted? x) (str/trim (:label x))))
                 (defn read-string [x] (when (checked? x) (str/trim x)))")
        [shape negative laundering] (:diagnostics result)]
    (is (= ["TSP250-01" "TSP250-02" "TSP250-01"] (codes result)) (pr-str (:diagnostics result)))
    (is (= [#{:truthy} #{:falsy} #{:truthy}] (mapv :reasons [shape negative laundering])))
    (is (= "guards.clj" (:file shape)))
    (is (= 7 (get-in shape [:pos :row])))
    (is (= ["truthy"] (:reasons (main/diag->json shape))))
    (is (= {:cast 0 :any 0 :ignore 0 :guard 1} (:trust result)))))

(deftest unsupported-bodies-cannot-make-checked-claims
  (let [result (check-source
                "(defmacro always [& _] true)
                 (defn recursive? {:t/guard '{:type string? :mode :checked}}
                   [x] (or (string? x) (recursive? x)))
                 (defn cast? {:t/guard '{:type string? :mode :checked}}
                   [x] #_(t/cast boolean?) (string? x))
                 (defn any? {:t/guard '{:type string? :mode :checked}}
                   [x] #_(t/any \"fixture\") (string? x))
                 (defn opaque? {:t/guard '{:type string? :mode :checked}}
                   [x] (always (string? x)))
                 (defn bad-type? {:t/guard '{:type string? :mode :checked}}
                   [x] (and (string? x) (inc x)))")
        claims (filter #(= "TSP250-03" (:code %)) (:diagnostics result))]
    (is (= [#{:recursive} #{:unsupported-body} #{:unsupported-body}
            #{:unsupported-body} #{:body-errors}] (mapv :reasons claims))
        (pr-str (:diagnostics result)))
    (is (= {:cast 1 :any 1 :ignore 0} (:trust result)))))

(deftest locals-retain-identity
  (let [result (check-source
                "(defn snapshot {:t/sig '[:-> t/unknown]} [] (throw (Exception.)))
                 (defn string-value? [x] (string? x))
                 (defn shadow-argument? [x] (let [x \"a\"] (string? x)))
                 (defn shadow-predicate? [x] (let [string? (fn [_] true)] (string? x)))
                 (let [x (snapshot)] (when (string-value? x) (str/trim x)))
                 (when (string-value? (snapshot)) (str/trim (snapshot)))
                 (let [x (snapshot)] (when (shadow-argument? x) (str/trim x)))
                 (let [x (snapshot)] (when (shadow-predicate? x) (str/trim x)))
                 (let [x (snapshot) string-value? (fn [_] true)]
                   (when (string-value? x) (str/trim x)))
                 (with-redefs [string-value? (fn [_] true)]
                   (let [x (snapshot)] (when (string-value? x) (str/trim x))))")]
    (is (= (repeat 5 "TSP210-01") (codes result)) (pr-str (:diagnostics result)))
    (is (= [7 8 9 11 13] (mapv #(get-in % [:pos :row]) (:diagnostics result))))))

(deftest facts-require-stable-definitions-and-snapshots
  (let [result (check-source
                "(defn string-value? [x] (string? x))
                 (defn recover [a] (let [x @a] (when (string-value? x) (str/trim x))))
                 (defn unstable [a] (when (string-value? @a) (str/trim @a)))
                 (defn replacement? {:t/guard '{:type string? :mode :checked}}
                   [x] (string? x))
                 (def replacement? (fn [_] true))")]
    (is (= ["TSP210-01" "TSP250-03"] (codes result)) (pr-str (:diagnostics result)))
    (is (= [4 5] (mapv #(get-in % [:pos :row]) (:diagnostics result))))
    (is (= #{:unstable-var} (get-in result [:diagnostics 1 :reasons])))))

(deftest two-sided-proof-is-relative-to-the-parameter-domain
  (let [result (check-source
                "(defn ok? {:t/sig '[#{:ok} :-> boolean?]
                            :t/guard '{:type keyword? :mode :checked :two-sided true}}
                   [x] (= x :ok))")]
    (is (empty? (codes result)) (pr-str (:diagnostics result)))
    (is (= :checked (get-in result [:env :guards 'guards/ok? :mode])))))

(deftest facts-follow-namespace-aliases-and-are-recomputed
  (let [consumer (parse/parse-string "reader.clj"
                                    "(ns reader (:require [guard-lib :as g] [clojure.string :as str]))
                                     (defn label [x] (when (g/entry? x) (str/trim (:label x))))")
        library (fn [body]
                  (parse/parse-string "guard_lib.clj"
                                      (str "(ns guard-lib (:require [clojure.spec.alpha :as s]))
                                            (s/def ::label string?)
                                            (s/def ::entry (s/keys :req-un [::label]))
                                            (s/fdef entry? :args (s/cat :x any?) :ret boolean?)
                                            (s/fdef check-entry? :args (s/cat :x any?) :ret boolean?)
                                            (declare check-entry?)
                                            (defn entry? [x] (check-entry? x))
                                            (defn check-entry? [x] " body ")")))]
    (testing "The declaration order and the source file order can differ from dependency order"
      (let [result (check/check-files [consumer (library "(s/valid? ::entry x)")] {:strict-nil true})]
        (is (empty? (codes result)) (pr-str (:diagnostics result)))))
    (testing "Changing the helper does not retain an old spec fact"
      (let [result (check/check-files [(library "(map? x)") consumer] {:strict-nil true})]
        (is (some #(= "reader.clj" (:file %)) (:diagnostics result)))))))
