(ns typespec.multimethod-contracts-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [typespec.check :as check]
            [typespec.parse :as parse]
            [typespec.project-contracts :as contracts]))

(defn- locations [result]
  (mapv (juxt :file (comp :row :pos) :code) (:diagnostics result)))

(deftest checks-methods-and-call-sites-against-the-same-spec
  (let [provider (parse/parse-string "display.clj"
                                    "(ns catalog.display (:require [clojure.spec.alpha :as s]))
(s/def ::title string?)
(s/def ::entry (s/keys :req-un [::title]))
(s/fdef render :args (s/cat :format keyword? :entry ::entry) :ret string?)
(defmulti render (fn [format _] format))
(defmethod render :default [_ {:keys [title]}] title)")
        client (parse/parse-string "client.clj"
                                  "(ns client (:require [catalog.display :as api]))
(defmethod api/render :text [_ {:keys [title]}] (subs title 1))
(defmethod api/render :invalid [_ {:keys [title]}] {:title title})
(api/render :text {:title \"ok\"})
(api/render :text 7)")
        extension (parse/parse-string "extension.clj"
                                     "(ns extension (:require [catalog.display :refer [render] :rename {render display}]))
(defmethod display :local [_ {:keys [title]}] title)")
        opts {:track-uses true}
        result (check/check-files [client extension provider] opts)
        index (contracts/index result identity)]
    (is (= [["client.clj" 3 "TSP2322"] ["client.clj" 5 "TSP2345"]]
           (locations result)) (pr-str (:diagnostics result)))
    (is (str/includes? (get-in result [:diagnostics 0 :message])
                       "returns {:title :catalog.display/title}, but the signature says string? (defmethod catalog.display/render :invalid, s/fdef)"))
    (is (= (:diagnostics result)
           (:diagnostics (check/check-files [provider extension client] opts))))
    (is (= [[:implementation "client.clj" 2]
            [:implementation "client.clj" 3]
            [:call "client.clj" 4]
            [:call "client.clj" 5]
            [:implementation "display.clj" 6]
            [:implementation "extension.clj" 2]]
           (mapv (juxt :kind :file (comp :row :pos)) (:uses (first index)))))))

(deftest dispatch-can-route-different-arities-to-different-methods
  (let [file (parse/parse-string "demo.clj"
                                "(ns demo (:require [clojure.spec.alpha :as s]))
(s/fdef render :args (s/cat :text string? :number (s/? int?)) :ret string?)
(defmulti render (fn [& args] (count args)))
(defmethod render 1 [text] (subs text 1))
(defmethod render 2 [text n] (str (inc n) text))
(defmethod render :other
  ([text] text)
  ([text n] n))
(defmethod render 3 [_ _ _] {:outside \"the Spec arities\"})")
        result (check/check-files [file] {})]
    (is (= [["demo.clj" 8 "TSP2322"]] (locations result)) (pr-str (:diagnostics result)))
    (is (str/includes? (get-in result [:diagnostics 0 :message]) "returns int?"))))

(deftest variadic-spec-also-checks-fixed-methods-and-their-rest-alternatives
  (let [file (parse/parse-string "demo.clj"
                                "(ns demo (:require [clojure.spec.alpha :as s]))
(s/fdef render :args (s/cat :format keyword? :numbers (s/* int?)) :ret string?)
(defmulti render (fn [fmt & _] fmt))
(defmethod render :fixed [_ n] n)
(defmethod render :many
  ([_] \"empty\")
  ([_ & [n]] (str (inc n))))
(defmethod render :nilable [_ & [n]] (str (inc n)))
(s/fdef head :args (s/cat :format keyword? :numbers (s/+ int?)) :ret int?)
(defmulti head (fn [fmt & _] fmt))
(defmethod head :first [_ & [n]] (inc n))")
        result (check/check-files [file] {})]
    (is (= [["demo.clj" 4 "TSP2322"] ["demo.clj" 8 "TSP2531"]]
           (locations result)) (pr-str (:diagnostics result)))
    (is (str/includes? (get-in result [:diagnostics 0 :message]) "returns int?"))))

(deftest methods-without-a-contract-still-check-body-and-hints
  (let [file (parse/parse-string "demo.clj"
                                "(ns demo (:require [clojure.spec.alpha :as s]))
(defmulti bare :kind)
(defmethod bare :a [^Long x] (.substring x 1))
(s/fdef render :args (s/cat :x int?) :ret int?)
(defmulti render :kind)
(let [render bare] (defmethod render :b [_] \"a local MultiFn\"))
(defmethod render :a [^String x] 1)")
        result (check/check-files [file] {})]
    (is (= [["demo.clj" 3 "TSP2339"] ["demo.clj" 7 "TSP2322"]]
           (locations result)) (pr-str (:diagnostics result)))
    (is (str/includes? (get-in result [:diagnostics 1 :message]) "contradicts the type"))))

(deftest strict-nil-checks-nested-method-results
  (let [file (parse/parse-string "demo.clj"
                                "(ns demo (:require [clojure.spec.alpha :as s]))
(s/def ::parent #(instance? java.io.File %))
(s/def ::result (s/keys :req-un [::parent]))
(s/fdef render :args (s/cat :kind keyword?) :ret ::result)
(defmulti render identity)
(defmethod render :unguarded [_] {:parent (.getParentFile (java.io.File. \"file\"))})
(defmethod render :guarded [_]
  (if-let [parent (.getParentFile (java.io.File. \"file\"))]
    {:parent parent}
    (throw (IllegalArgumentException.))))")
        strict (check/check-files [file] {:strict-nil true})]
    (is (empty? (:diagnostics (check/check-files [file] {}))))
    (is (= [["demo.clj" 6 "TSP2322"]] (locations strict)) (pr-str (:diagnostics strict)))
    (is (= [:parent] (get-in strict [:diagnostics 0 :type-path])))))
