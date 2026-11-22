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

(def ^:private request-spec
  "
(s/def :render.text/method #{\"text\" \"plain\"})
(s/def :render.text/payload string?)
(s/def :render.count/method #{\"count\"})
(s/def :render.count/payload int?)
(s/def ::text (s/keys :req-un [:render.text/method :render.text/payload]))
(s/def ::count (s/keys :req-un [:render.count/method :render.count/payload]))
(s/def ::request (s/or :text ::text :count ::count))\n")

(deftest string-dispatch-narrows-the-selected-spec-argument
  (let [provider (parse/parse-string "display.clj"
                                    (str "(ns catalog.display (:require [clojure.spec.alpha :as s]))"
                                         request-spec
                                         "(s/fdef render :args (s/cat :context any? :request ::request) :ret string?)
(defmulti render \"Render a request.\" (fn [ctx request] (:method request)))"))
        client (parse/parse-string "client.clj"
                                  "(ns client (:require [catalog.display :as api]))
(defmethod api/render \"text\" [_ {:keys [method payload]}] (if (= method \"text\") (subs payload 1) 42))
(defmethod api/render \"plain\" [_ request] (str (inc (:payload request))))
(defmethod api/render \"count\" [_ {:keys [payload]}] (str (inc payload)))
(defmethod api/render :default [_ {:keys [payload]}] (subs payload 1))
(defmethod api/render \"other\" [_ {:keys [payload]}] (subs payload 1))
(api/render nil {:method \"text\" :payload 7})")
        result (check/check-files [client provider] {})]
    (is (= [["client.clj" 3 "TSP2345"] ["client.clj" 5 "TSP2345"]
            ["client.clj" 6 "TSP2345"] ["client.clj" 7 "TSP2345"]]
           (locations result))
        (pr-str (:diagnostics result)))
    (is (= (:diagnostics result) (:diagnostics (check/check-files [provider client] {}))))))

(deftest string-dispatch-uses-the-shared-contract-when-narrowing-is-uncertain
  (let [source (str "(ns demo (:require [clojure.spec.alpha :as s]))"
                    request-spec
                    "(s/fdef by-key :args (s/cat :request ::request) :ret string?)
(defmulti by-key :method)
(defmethod by-key \"text\" [{:keys [payload]}] (subs payload 1))
(s/fdef by-default :args (s/cat :request ::request) :ret string?)
(defmulti by-default :method :default \"text\")
(defmethod by-default \"text\" [{:keys [payload]}] (subs payload 1))
(defmethod by-default \"count\" [{:keys [payload]}] (str (inc payload)))
(s/fdef by-hierarchy :args (s/cat :request ::request) :ret string?)
(defmulti by-hierarchy :method :hierarchy #'custom)
(defmethod by-hierarchy \"text\" [{:keys [payload]}] (subs payload 1))
(s/fdef by-computation :args (s/cat :request ::request) :ret string?)
(defmulti by-computation (fn [request] (str (:method request))))
(defmethod by-computation \"text\" [{:keys [payload]}] (subs payload 1))
(s/fdef redefined :args (s/cat :request ::request) :ret string?)
(defmulti redefined :method)
(defmulti redefined (fn [request] (str (:method request))))
(defmethod redefined \"text\" [{:keys [payload]}] (subs payload 1))
(s/def ::optional-text (s/keys :req-un [:render.text/payload] :opt-un [:render.text/method]))
(s/def ::optional-request (s/or :text ::optional-text :count ::count))
(s/fdef optional-tag :args (s/cat :request ::optional-request) :ret string?)
(defmulti optional-tag :method)
(defmethod optional-tag \"text\" [{:keys [payload]}] (subs payload 1))")
        result (check/check-files [(parse/parse-string "demo.clj" source)] {})]
    (is (= (mapv #(vector "demo.clj" % "TSP2345") [14 18 21 25 30])
           (locations result))
        (pr-str (:diagnostics result)))))

(deftest class-dispatch-refines-spec-arguments-and-projected-keys
  (let [provider (parse/parse-string "render.clj"
                                    "(ns catalog.render (:require [clojure.spec.alpha :as s]))
(s/def ::body (s/nilable (s/or :text string? :file (partial instance? java.io.File)
                              :stream (partial instance? java.io.ByteArrayInputStream))))
(s/def ::request (s/keys :opt-un [::body]))
(s/fdef render :args (s/cat :request ::request) :ret (s/nilable string?))
(defmulti render (comp class :body))
(s/fdef render-body :args (s/cat :body ::body) :ret (s/nilable string?))
(defmulti render-body class)")
        client (parse/parse-string "extension.clj"
                                  "(ns extension (:require [catalog.render :as api] [clojure.spec.alpha :as s])
  (:import [java.io InputStream]))
(s/fdef consume :args (s/cat :stream (partial instance? InputStream)) :ret string?)
(defn consume [stream] \"read\")
(defmethod api/render String [{:keys [body]}] (subs body 1))
(defmethod api/render InputStream [request] (consume (:body request)))
(defmethod api/render nil [request] (:body request))
(defmethod api/render-body String [body] (subs body 1))
(defmethod api/render-body :default [body] (subs body 1))
(defmethod api/render java.io.File [request] (consume (:body request)))")
        result (check/check-files [client provider] {})]
    (is (= [["extension.clj" 9 "TSP2345"] ["extension.clj" 10 "TSP2345"]]
           (locations result))
        (pr-str (:diagnostics result)))
    (is (= (:diagnostics result) (:diagnostics (check/check-files [provider client] {}))))))

(deftest class-dispatch-keeps-the-contract-for-custom-hierarchies-and-functions
  (let [file (parse/parse-string "demo.clj" "(ns demo (:refer-clojure :exclude [class])
  (:require [clojure.spec.alpha :as s]))
(defn class [_] String)
(s/def ::body (s/or :text string? :file (partial instance? java.io.File)))
(s/fdef shadowed :args (s/cat :body ::body) :ret string?)
(defmulti shadowed class)
(defmethod shadowed String [body] (subs body 1))
(s/fdef custom :args (s/cat :body ::body) :ret string?)
(defmulti custom clojure.core/class :hierarchy #'hierarchy)
(defmethod custom String [body] (subs body 1))
(s/fdef default-nil :args (s/cat :body ::body) :ret string?)
(defmulti default-nil clojure.core/class :default nil)
(defmethod default-nil nil [body] (subs body 1))")
        result (check/check-files [file] {})]
    (is (= [["demo.clj" 7 "TSP2345"] ["demo.clj" 10 "TSP2345"] ["demo.clj" 13 "TSP2345"]]
           (locations result))
        (pr-str (:diagnostics result)))))

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
