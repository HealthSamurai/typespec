(ns typespec.types
  "Type representation, the spec-style type language (read and print), and subtyping."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))

(def any {:t :any})
(def unknown {:t :unknown})
(def never {:t :never})
(def nil-t {:t :nil})
(defn prim [n] {:t :prim :name n})
(defn lit [v] {:t :lit :val v})
(defn ref-t [k] {:t :ref :name k})
;; :generic-failure is a checker note on a result, not part of the type's identity.
(defn strip [t] (dissoc t :platform :as :generic-failure))

(defn union [& ts]
  (let [flat (loop [xs (seq ts) seen #{} out []]
               (if (empty? xs)
                 out
                 (let [x (first xs) more (rest xs)]
                   (cond (= :union (:t x)) (recur (concat (:types x) more) seen out)
                         (= :never (:t x)) (recur more seen out)
                         ;; the same type twice: keep the copy without the failure note
                         (seen (strip x)) (recur more seen
                                                 (if (:generic-failure x) out
                                                   (mapv #(if (= (strip %) (strip x)) (dissoc % :generic-failure) %) out)))
                         :else (recur more (conj seen (strip x)) (conj out x))))))]
    (cond (empty? flat) never
          (some #(= :any (:t %)) flat) any
          (some #(= :unknown (:t %)) flat) unknown
          (= 1 (count flat)) (first flat)
          :else {:t :union :types flat})))

(defn lit-prim [v]
  (prim (cond (string? v) :string (keyword? v) :keyword (integer? v) :int
              (number? v) :double (boolean? v) :boolean :else :symbol)))

;; ---- Java classes ----

(def primitives #{"long" "int" "double" "float" "boolean" "short" "byte" "char"})

(def load-class
  (memoize
   (fn [^String n]
     (case n
       "long" Long/TYPE "int" Integer/TYPE "double" Double/TYPE "float" Float/TYPE
       "boolean" Boolean/TYPE "short" Short/TYPE "byte" Byte/TYPE "char" Character/TYPE "void" Void/TYPE
       (try (Class/forName n false (.getContextClassLoader (Thread/currentThread)))
            (catch Throwable _ nil))))))

(def class->prim
  {"java.lang.String" :string
   "long" :int "int" :int "short" :int "byte" :int
   "java.lang.Long" :int "java.lang.Integer" :int "java.lang.Short" :int "java.lang.Byte" :int
   "double" :double "float" :double "java.lang.Double" :double "java.lang.Float" :double
   "java.lang.Number" :number "boolean" :boolean "java.lang.Boolean" :boolean
   "clojure.lang.Keyword" :keyword "clojure.lang.Symbol" :symbol "java.util.UUID" :uuid})

(def prim->class
  {:string "java.lang.String" :int "java.lang.Long" :double "java.lang.Double" :number "java.lang.Number"
   :boolean "java.lang.Boolean" :keyword "clojure.lang.Keyword" :symbol "clojure.lang.Symbol"
   :uuid "java.util.UUID" :inst "java.util.Date" :map "java.util.Map" :vector "java.util.List"
   :set "java.util.Set" :seq "java.util.List" :coll "java.util.Collection" :fn "clojure.lang.IFn"})

(def prim-supers
  "Classes that every value of a collection predicate is an instance of (close enough for interop checks)."
  {:map ["clojure.lang.APersistentMap"] :vector ["clojure.lang.APersistentVector"]
   :set ["clojure.lang.APersistentSet"] :seq ["clojure.lang.ISeq" "java.util.List"]
   :coll ["clojure.lang.IPersistentCollection" "java.lang.Iterable"]})

(def prim-interface
  "The class that guarantees a collection predicate."
  {:map "clojure.lang.IPersistentMap" :vector "clojure.lang.IPersistentVector" :set "clojure.lang.IPersistentSet"
   :seq "clojure.lang.ISeq" :coll "clojure.lang.IPersistentCollection"})

(defn class-type [n]
  (cond (= n "void") nil-t
        (= n "java.lang.Object") any
        :else (if-let [p (class->prim n)] (prim p) {:t :class :name n})))

(defn platform
  "A type that came from a hint or from Java: nil is not checked. `shown` is how it is printed."
  [t shown]
  (cond (#{:nil :any} (:t t)) t
        (primitives shown) (assoc t :as shown)
        :else (assoc t :platform true :as shown)))

(defn resolve-class
  "Full class name for a symbol in a file with these imports, or nil."
  [imports s]
  (let [s (str s)]
    (or (get imports (symbol s))
        (when (primitives s) s)
        (when (and (str/includes? s ".") (load-class s)) s)
        (when (and (re-matches #"[A-Z]\w*" s) (load-class (str "java.lang." s))) (str "java.lang." s)))))

(defn- class-sub? [a b]
  (let [ca (load-class a) cb (load-class b)]
    (if (and ca cb) (.isAssignableFrom ^Class cb ^Class ca) true)))

(defn- prim-is-class? [p cname]
  (boolean (some #(class-sub? % cname) (or (prim-supers p) [(prim->class p)]))))

(defn- class-is-prim? [cname p]
  (class-sub? cname (or (prim-interface p) (prim->class p))))

;; ---- resolving named types ----

(defn members
  "Flat list of alternatives of a type, with refs resolved."
  ([env t] (members env t #{}))
  ([env t seen]
   (case (:t t)
     :ref (if (seen (:name t))
            [unknown]
            (if-let [d (get-in env [:specs (:name t) :type])]
              (members env d (conj seen (:name t)))
              [unknown]))
     :union (mapcat #(members env % seen) (:types t))
     :merge (let [parts (map #(members env % seen) (:types t))]
              (if (every? #(and (= 1 (count %)) (= :map (:t (first %)))) parts)
                [(reduce (fn [a b] {:t :map :req (merge (:req a) (:req b))
                                    :opt (apply dissoc (merge (:opt a) (:opt b)) (concat (keys (:req a)) (keys (:req b))))})
                         (map first parts))]
                [unknown]))
     [t])))

(defn resolve-t [env t] (apply union (members env t)))

(defn nilable? [env t] (boolean (some #(= :nil (:t %)) (members env t))))

(defn remove-nil [env t]
  (case (:t t)
    :nil never
    :union (apply union (map #(remove-nil env %) (:types t)))
    :ref (if (nilable? env t) (remove-nil env (get-in env [:specs (:name t) :type])) t)
    t))

;; ---- subtyping ----

(declare sub?)
(def ^:dynamic *depth* 0)
(def ^:dynamic *inconclusive* nil)
(def ^:dynamic *strict-shapes*
  "When true, a bare collection predicate (vector?, map?) is not evidence about
  its elements: vector? to (s/coll-of string?) is inconclusive instead of proved.
  The solver binds it; ordinary gradual checks keep the lenient rule."
  false)

(defn- inconclusive! [reason]
  (when *inconclusive* (swap! *inconclusive* conj reason))
  false)

(defn- prim-sub? [a b]
  (or (= a b)
      (contains? (get {:int #{:number} :double #{:number} :vector #{:coll} :set #{:coll}
                       :seq #{:coll} :map #{:coll} :keyword #{:fn}} a) b)))

(defn- map-sub? [env s t]
  (and (every? (fn [[k tv]] (when-let [sv (get (:req s) k)] (sub? env sv tv))) (:req t))
       (every? (fn [[k tv]] (if-let [sv (or (get (:req s) k) (get (:opt s) k))] (sub? env sv tv) true)) (:opt t))))

(defn- arity-sub?
  "Source accepts every call described by target; arguments are contravariant.
  Supports fixed positional parameters and a homogeneous rest parameter only."
  [env s t]
  (let [sp (:params s) tp (:params t)
        sr (:rest s) tr (:rest t)]
    (and (not (:kwargs s)) (not (:kwargs t))
         (vector? sp) (vector? tp) (:ret s) (:ret t)
         (if tr
           (and sr (<= (count sp) (count tp)))
           (if sr (<= (count sp) (count tp)) (= (count sp) (count tp))))
         (every? true? (map-indexed
                       (fn [i p] (sub? env p (if (< i (count sp)) (nth sp i) sr))) tp))
         (or (nil? tr) (sub? env tr sr))
         (sub? env (:ret s) (:ret t)))))

(defn dispatch-arity
  "Clojure chooses a fixed arity before its variadic fallback."
  [arities n]
  (or (some #(when (and (not (:rest %)) (= n (count (:params %)))) %) arities)
      (some #(when (and (:rest %) (<= (count (:params %)) n)) %) arities)))

(defn dispatch-plan
  "Source branches and argument contracts needed to implement one expected arity."
  [source target]
  (let [minimum (count (:params target))]
    (if-not (:rest target)
      (when-let [a (dispatch-arity source minimum)] [[a target]])
      (when-let [rest-arity (first (filter :rest source))]
        (let [rest-minimum (count (:params rest-arity))
              fixed (filter #(not (:rest %)) source)
              fixed-counts (set (map #(count (:params %)) fixed))
              params-at (fn [n] (into (:params target) (repeat (- n minimum) (:rest target))))]
          (when (every? fixed-counts (range minimum rest-minimum))
            (conj (vec (for [a fixed :let [n (count (:params a))] :when (<= minimum n)]
                         [a (assoc target :params (params-at n) :rest nil)]))
                  [rest-arity (assoc target :params (params-at (max rest-minimum minimum)))])))))))

(defn- fn-sub?
  "Every target arity needs a source arity. Mixed fixed/rest dispatch and kwargs
  are deliberately unsupported (except identical types), not guessed compatible."
  [env s t]
  (let [sa (:arities s) ta (:arities t)]
    (cond
      (and (::dispatch (meta s)) (seq sa) (seq ta) (not-any? :kwargs ta))
      (every? (fn [target]
                (when-let [plan (dispatch-plan sa target)]
                  (every? (fn [[a contract]] (arity-sub? env a contract)) plan))) ta)

      (and (seq sa) (seq ta)
             (not-any? :kwargs sa) (not-any? :kwargs ta)
             (or (= 1 (count sa))
                 (and (not-any? :rest sa)
                      (= (count sa) (count (distinct (map #(count (:params %)) sa)))))))
      (every? (fn [target] (some #(arity-sub? env % target) sa)) ta)
      :else (inconclusive! :unsupported-function))))

(defn- sub1? [env s t]
  (let [st (:t s) tt (:t t)]
    (cond
      ;; Reflexivity is valid even for opaque kwargs or unknown components:
      ;; it establishes no conversion between different callable contracts.
      (= (strip s) (strip t)) true
      (#{:any :unknown} tt) true
      (#{:any :never} st) true
      (= :unknown st) (inconclusive! :unknown)
      (= :rigid tt) false
      ;; An arbitrary type parameter fits only itself and top (handled above):
      ;; without a bound this is a definite mismatch, not missing information.
      (= :rigid st) (if (#{:unknown :any} (:t (:bound s)))
                     false
                     (sub? env (:bound s) t))
      (or (#{:var :ivar :all} st) (#{:var :ivar :all} tt))
      (inconclusive! :unresolved-variable)
      (= :nil st) (boolean (:platform t))
      (= :nil tt) false
      (= :lit st) (if (= :lit tt) (= (:val s) (:val t)) (sub1? env (lit-prim (:val s)) t))
      (= :lit tt) false
      (= :prim st) (case tt
                     :prim (prim-sub? (:name s) (:name t))
                     :class (prim-is-class? (:name s) (:name t))
                     ;; A known kind never fits a different :kind (set? is not a vector).
                     :coll (and (contains? #{:vector :set :seq :coll} (:name s))
                                (or (nil? (:kind t)) (= :coll (:name s)) (= (:kind t) (:name s)))
                                (if *strict-shapes* (inconclusive! :unsupported) true))
                     :map-of (and (= :map (:name s))
                                  (if *strict-shapes* (inconclusive! :unsupported) true))
                     false)
      (= :class st) (case tt
                      :class (class-sub? (:name s) (:name t))
                      :prim (class-is-prim? (:name s) (:name t))
                      false)
      (= :map st) (case tt
                    :map (map-sub? env s t)
                    :prim (contains? #{:map :coll} (:name t))
                    :map-of (every? #(sub? env % (:v t)) (concat (vals (:req s)) (vals (:opt s))))
                    :class (prim-is-class? :map (:name t))
                    false)
      (= :map-of st) (case tt
                       :map-of (and (sub? env (:k s) (:k t)) (sub? env (:v s) (:v t)))
                       :prim (contains? #{:map :coll} (:name t))
                       :map (empty? (:req t))
                       :class (prim-is-class? :map (:name t))
                       false)
      (= :coll st) (case tt
                     :coll (and (or (nil? (:kind t)) (= (:kind s) (:kind t))) (sub? env (:of s) (:of t)))
                     :prim (contains? (get {:vector #{:vector :coll} :set #{:set :coll} :seq #{:seq :coll}} (:kind s) #{:coll}) (:name t))
                     :class (prim-is-class? (or (:kind s) :coll) (:name t))
                     false)
      (= :fn st) (case tt
                    :fn (boolean (fn-sub? env s t))
                    :class (let [c (load-class (:name t))]
                             (boolean (and c (.isAssignableFrom ^Class c clojure.lang.IFn))))
                    :prim (= :fn (:name t))
                    false)
      :else false)))

(defn sub?
  "Proven compatibility within the supported subset, with explicit any as a bypass.
  Unknown is a safe top type: every type fits it, but it does not fit a concrete
  type. Exhausting the recursion budget returns false, not evidence of a subtype.
  Callers wanting tolerant checking must handle unknown separately."
  [env s t]
  (if (> *depth* 12)
    (inconclusive! :depth)
    (binding [*depth* (inc *depth*)]
      (let [tt (members env t)]
        (every? (fn [s1] (some #(sub1? env s1 %) tt)) (members env s))))))

(defn sub-result
  "Companion to sub?: {:status :proved|:mismatch|:inconclusive :reasons #{...}}.
  Unknown sources and exhausted recursion cannot prove a concrete mismatch.
  Any remains an explicit bypass, not a runtime guarantee. Reasons are gathered
  conservatively across alternatives; a successful proof takes precedence."
  [env s t]
  (binding [*inconclusive* (atom #{})]
    (let [proved? (sub? env s t)
          reasons @*inconclusive*]
      {:status (cond proved? :proved (seq reasons) :inconclusive :else :mismatch)
       :reasons (if proved? #{} reasons)})))

;; ---- printing ----

(def ^:dynamic *cur-ns* nil)

(defn show-kw [k]
  (if (and *cur-ns* (= (namespace k) (str *cur-ns*))) (str "::" (name k)) (str k)))

(defn show
  "Prints a type in the type language, so it can be pasted back into code."
  [t]
  (or (:as t)
      (some-> t meta ::operator-form pr-str)
    (case (:t t)
      :any "any?" :unknown "t/unknown" :never "t/never" :nil "nil?"
      (:var :ivar :rigid) (str (:name t))
      :all (str "(t/all ["
                (str/join " " (map (fn [{:keys [name bound]}]
                                      (if (= :unknown (:t bound)) (str name)
                                        (str "(" name " :< " (show bound) ")"))) (:binders t)))
                "] " (show (:body t)) ")")
      :prim (str (name (:name t)) "?")
      :lit (pr-str (:val t))
      :ref (show-kw (:name t))
      :class (:name t)
      :union (let [ts (:types t)
                   others (remove #(= :nil (:t %)) ts)
                   core (cond (= 1 (count others)) (show (first others))
                              (every? #(= :lit (:t %)) others) (str "#{" (str/join " " (map show others)) "}")
                              :else (str "(t/or " (str/join " " (map show others)) ")"))]
               (if (some #(= :nil (:t %)) ts) (str "(s/nilable " core ")") core))
      :merge (str "(s/merge " (str/join " " (map show (:types t))) ")")
      :map (if (:fresh t)
             (str "{" (str/join ", " (map (fn [[k v]] (str k " " (show v))) (:req t))) "}")
             (str "(s/keys"
                  (when (seq (:req t)) (str " :req [" (str/join " " (map show-kw (keys (:req t)))) "]"))
                  (when (seq (:opt t)) (str " :opt [" (str/join " " (map show-kw (keys (:opt t)))) "]"))
                  ")"))
      :map-of (str "(s/map-of " (show (:k t)) " " (show (:v t)) ")")
      :coll (str "(s/coll-of " (show (:of t)) (when-let [k (:kind t)] (str " :kind " (name k) "?")) ")")
      :fn (if-let [ars (seq (:arities t))]
            (let [ar (fn [a] (str "[" (str/join " " (concat (map show (:params a))
                                                            (when (:rest a) ["&" (show (:rest a))])
                                                            (when (:kwargs a) ["&" (show (:kwargs a))])
                                                            [":->" (show (:ret a))])) "]"))]
              (if (= 1 (count ars)) (ar (first ars)) (str "(" (str/join " " (map ar ars)) ")")))
            "fn?")
      (pr-str t))))

;; ---- reading the type language ----

(def ^:private pred-prims
  '{string? :string int? :int integer? :int pos-int? :int nat-int? :int neg-int? :int
    double? :double float? :double number? :number pos? :number
    keyword? :keyword simple-keyword? :keyword qualified-keyword? :keyword
    symbol? :symbol boolean? :boolean uuid? :uuid inst? :inst
    map? :map vector? :vector set? :set seq? :seq list? :seq sequential? :coll coll? :coll
    fn? :fn ifn? :fn})

(defn- op [ctx sym]
  (when (symbol? sym)
    (let [n (namespace sym)
          full (when n (str (get (:aliases ctx) (symbol n) n)))]
      (cond (#{"clojure.spec.alpha" "s"} full) (keyword "s" (name sym))
            (= "t" n) (keyword "t" (name sym))
            (nil? n) (keyword (name sym))
            :else nil))))

(declare parse-type)

(defn- flat-keys [x] (cond (keyword? x) [x] (seq? x) (mapcat flat-keys (rest x)) :else []))

(defn- parse-keys [args]
  (let [{:keys [req opt req-un opt-un]} (apply hash-map args)
        un (fn [ks] (map (fn [k] [(keyword (name k)) (ref-t k)]) ks))
        q (fn [ks] (map (fn [k] [k (ref-t k)]) ks))
        plain #(filter keyword? %)
        grouped #(mapcat flat-keys (filter seq? %))
        required (concat (q (plain req)) (un (plain req-un)))
        optional (concat (q (concat opt (grouped req))) (un (concat opt-un (grouped req-un))))
        req-map (into (array-map) required)
        opt-map (into (array-map) optional)
        conflicts (into (set (filter req-map (keys opt-map)))
                        (keep (fn [[k entries]] (when (next (distinct entries)) k)))
                        (group-by first (concat required optional)))]
    (with-meta {:t :map :req req-map :opt opt-map}
      (cond-> {}
        (seq conflicts) (assoc ::shape-conflicts (sort-by pr-str conflicts))
        (some seq? (concat req req-un)) (assoc ::shape-form 's/keys)))))

(defn- parse-operator [ctx operator form]
  (let [[_ source & keys] form
        argc (dec (count form))
        valid-arity? (case operator
                       (t/pick t/omit) (pos? argc)
                       (t/partial t/keys-of) (= 1 argc)
                       t/get (= 2 argc))]
    (if (and valid-arity? (every? keyword? keys))
      (let [failed? (volatile! false)
            input (parse-type (assoc ctx :fail (fn [bad] (vreset! failed? true) ((:fail ctx) bad))) source)]
        (if @failed?
          (with-meta unknown {::invalid-operator true})
          (with-meta {:t :operator :operator operator :input input :keys (vec (sort-by pr-str (distinct keys)))}
            {::operator-form form})))
      (do ((:operator-error ctx) "TSP270-01" form
           (str operator " expects " (case operator
                                        (t/pick t/omit) "a map type followed by zero or more keyword literals"
                                        t/get "a map type and one keyword literal"
                                        "one map type")))
          (with-meta unknown {::invalid-operator true})))))

(defn cat-items
  "Items of (s/cat :a A :b (s/? B) :c (s/* C)), or nil when the form is not an s/cat."
  [ctx form]
  (when (and (seq? form) (= :s/cat (op ctx (first form))) (odd? (count form)))
    (for [[_ f] (partition 2 (rest form))]
      (let [o (when (seq? f) (op ctx (first f)))]
        (case o
          :s/? {:mod :opt :type (parse-type ctx (second f))}
          (:s/* :s/+) {:mod :rest :type (parse-type ctx (second f))}
          {:type (parse-type ctx f)})))))

(defn cat->arities [items ret]
  (let [[plain more] (split-with #(nil? (:mod %)) items)
        opts (take-while #(= :opt (:mod %)) more)
        after (drop (count opts) more)
        rest-t (when (= :rest (:mod (first after))) (:type (first after)))]
    (when (or (empty? after) (and rest-t (= 1 (count after))))
      (let [ars (vec (reductions conj (mapv :type plain) (map :type opts)))]
        (vec (map-indexed (fn [i ps] {:params ps :rest (when (= i (dec (count ars))) rest-t) :ret ret}) ars))))))

(defn parse-sig
  "[A B :-> R], [A & B :-> R], or a list of such vectors."
  [ctx form]
  (let [one (fn [v]
              (let [[ps [arrow ret & extra]] (split-with #(not= :-> %) v)
                    [fixed [_ r]] (split-with #(not= '& %) ps)]
                (if (or (not= :-> arrow) extra (= 1 (count (drop-while #(not= :-> %) v))))
                  (do ((:fail ctx) v) nil)
                  {:params (mapv #(parse-type ctx %) fixed) :rest (when r (parse-type ctx r)) :ret (parse-type ctx ret)})))]
    (cond (vector? form) (if-let [a (one form)] {:t :fn :arities [a]} unknown)
          (and (seq? form) (seq form) (every? vector? form)) {:t :fn :arities (vec (keep one form))}
          :else (do ((:fail ctx) form) unknown))))

(defn- parse-all [ctx form]
  (let [[_ declarations body & extra] form
        failed (atom false)
        bad (fn [f] (reset! failed true) ((:fail ctx) f))
        c (assoc ctx :fail bad :inside-all true)
        binders (when (vector? declarations)
                  (loop [xs (seq declarations) out []]
                    (if-not xs out
                      (let [decl (first xs) bounded? (seq? decl)
                            n (if bounded? (first decl) decl)
                            b (if bounded? (nth decl 2 nil) 't/unknown)]
                        (if (or (not (symbol? n)) (namespace n)
                                (some #(= n (:name %)) out) (nil? b)
                                (and bounded? (or (not= 3 (count decl)) (not= :< (second decl)))))
                          (do (bad declarations) nil)
                          (let [bound (parse-type (assoc c :bound-context true) b)]
                            (if-not (#{:prim :unknown} (:t bound))
                              (do (bad b) nil)
                              (recur (next xs)
                                     (conj out {:id (gensym "binder") :name n :bound bound})))))))))
        vars (into {} (map (fn [{:keys [id name]}] [name {:t :var :id id :name name}]) binders))]
    (if (or (:inside-all ctx) extra (not (seq binders)) (not (vector? body))
            (some #{'&} body) (not= 1 (count (filter #{:->} body))))
      (do (bad form) unknown)
      (let [t (parse-type (assoc c :type-vars vars) body)]
        (if @failed unknown {:t :all :binders binders :body t})))))

(defn alpha-equivalent?
  "Equality modulo names/identities of top-level rank-1 binders. Metadata is retained."
  [a b]
  (let [normalize (fn [ty]
                    (if-not (= :all (:t ty)) ty
                      (let [ids (zipmap (map :id (:binders ty)) (range))]
                        (walk/postwalk
                         (fn [node]
                           (if (and (map? node) (contains? ids (:id node)))
                             (-> node (assoc :id (get ids (:id node))) (dissoc :name)) node)) ty))))]
    (= (normalize a) (normalize b))))

(defn parse-type
  "Reads a type form (plain data). ctx: {:aliases :imports :fail (fn [form])}."
  [ctx form]
  (let [p #(parse-type ctx %)
        fail #(do ((:fail ctx) form) unknown)]
    (cond
      (keyword? form) (if (namespace form) (ref-t form) (fail))
      (symbol? form) (let [nm (if (= "clojure.core" (namespace form)) (symbol (name form)) form)]
                       (cond (get (:type-vars ctx) nm) (get (:type-vars ctx) nm)
                             (pred-prims nm) (prim (pred-prims nm))
                             (#{'any? 'some?} nm) any
                             (= 'nil? nm) nil-t
                             (#{'true? 'false?} nm) (prim :boolean)
                             (= 't/unknown nm) unknown
                             (= 't/never nm) never
                             (:inside-all ctx) (fail)
                             :else (if-let [c (resolve-class (:imports ctx) nm)] (class-type c) (fail))))
      (set? form) (if (and (seq form) (every? #(or (string? %) (keyword? %) (number? %) (boolean? %)) form))
                    (apply union (map lit (sort-by pr-str form)))
                    (fail))
      (vector? form) (if (and (:inside-all ctx) (some #{'&} form)) (fail) (parse-sig ctx form))
      (seq? form)
      (case (op ctx (first form))
        :t/all (parse-all ctx form)
        :quote (p (second form))
        :s/nilable (vary-meta (union (p (second form)) nil-t) assoc ::shape-form 's/nilable)
        :s/or (vary-meta (apply union (map p (take-nth 2 (drop 2 form)))) assoc ::shape-form 's/or)
        :t/or (vary-meta (apply union (map p (rest form))) assoc ::shape-form 't/or)
        (:s/and :t/and :t/exact) (vary-meta (p (second form)) assoc ::shape-form (first form))
        :s/spec (p (second form))
        :s/keys (if (even? (count (rest form))) (parse-keys (rest form)) (fail))
        :s/merge {:t :merge :types (mapv p (rest form))}
        (:s/coll-of :s/every) (let [[_ inner & {:keys [kind]}] form]
                                {:t :coll :of (p inner)
                                 :kind (get '{vector? :vector set? :set list? :seq seq? :seq} kind)})
        :s/map-of {:t :map-of :k (p (nth form 1)) :v (p (nth form 2))}
        :s/tuple {:t :coll :kind :vector :of (apply union (map p (rest form)))}
        :s/fspec (if (:inside-all ctx) (fail) (let [{:keys [args ret]} (apply hash-map (rest form))
                       r (if ret (p ret) unknown)]
                   {:t :fn :arities (some-> (cat-items ctx args) (cat->arities r)) :ret r}))
        :t/fn (if (:inside-all ctx)
                (if (and (= 2 (count form)) (vector? (second form))) (p (second form)) (fail))
                (parse-sig ctx (second form)))
        ;; #(instance? C %) and (partial instance? C): the usual way to say "a Java class" in spec
        (:fn* :fn) (let [body (last form)]
                     (if-let [c (and (seq? body) (= 'instance? (first body))
                                     (resolve-class (:imports ctx) (second body)))]
                       (class-type c)
                       (fail)))
        :partial (if-let [c (and (= 'instance? (second form)) (resolve-class (:imports ctx) (nth form 2 nil)))]
                   (class-type c)
                   (fail))
        :t/class (if-let [c (resolve-class (:imports ctx) (second form))] (class-type c) (fail))
        (:t/pick :t/omit :t/partial :t/keys-of :t/get)
        (if (:operator-error ctx) (parse-operator ctx (first form) form) (fail))
        (fail))
      :else (fail))))
