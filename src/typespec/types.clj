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
(defn assumption [ty] (::assumption (meta ty)))
(defn with-assumption [ty reason]
  (if reason (vary-meta ty assoc ::assumption reason) ty))

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
    (cond-> (with-assumption
              (cond (empty? flat) never
                    (some #(= :any (:t %)) flat) any
                    (some #(= :unknown (:t %)) flat) unknown
                    (= 1 (count flat)) (first flat)
                    :else {:t :union :types flat})
              (some assumption ts))
      (some #(-> % meta ::unchecked-contract) ts)
      (vary-meta assoc ::unchecked-contract true))))

(defn lit-prim [v]
  (prim (cond (string? v) :string (keyword? v) :keyword (integer? v) :int
              (number? v) :double (boolean? v) :boolean :else :symbol)))

(defn tuple
  ([items] (tuple items (apply union items)))
  ([items element] {:t :coll :kind :vector :items items :of element}))

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
(def ^:dynamic ^:private *contract-subtyping* false)

(defn- inconclusive! [reason]
  (when *inconclusive* (swap! *inconclusive* conj reason))
  false)

(defn- prim-sub? [a b]
  (or (= a b)
      (contains? (get {:int #{:number} :double #{:number} :vector #{:coll} :set #{:coll}
                       :seq #{:coll} :map #{:coll} :keyword #{:fn}} a) b)))

(defn- map-sub? [env s t]
  (let [required (fn [[k tv]] (when-let [sv (get (:req s) k)] (sub? env sv tv)))
        optional (fn [[k tv]]
                   (if-let [sv (or (get (:req s) k) (get (:opt s) k))]
                     (sub? env sv tv)
                     (or (not *contract-subtyping*) (sub? env any tv))))]
    (if *contract-subtyping*
      ;; Isolate the reasons from each field. An incompatible field is decisive
      ;; even if the shape also contains an opaque sibling.
      (let [proof (fn [check field]
                    (binding [*inconclusive* (atom #{})]
                      (if (check field) #{} (not-empty @*inconclusive*))))
            results (concat (map #(proof required %) (:req t))
                            (map #(proof optional %) (:opt t)))]
        (when (every? some? results)
          (let [reasons (into #{} cat results)]
            (run! inconclusive! reasons)
            (empty? reasons))))
      (and (every? required (:req t)) (every? optional (:opt t))))))

(defn arity-minimum [arity]
  (+ (count (:params arity)) (get arity :rest-min 0)))

(defn arity-fits? [arity n]
  (if (or (:rest arity) (:kwargs arity))
    (<= (arity-minimum arity) n)
    (= (arity-minimum arity) n)))

(defn- arity-sub?
  "Source accepts every call described by target; arguments are contravariant.
  Supports fixed positional parameters and a homogeneous rest parameter only."
  [env s t]
  (let [sp (:params s) tp (:params t)
        sr (:rest s) tr (:rest t)]
    (and (not (:kwargs s)) (not (:kwargs t))
         (vector? sp) (vector? tp) (:ret s) (:ret t)
         (arity-fits? s (arity-minimum t))
         (or (nil? tr) sr)
         (every? true? (map-indexed
                       (fn [i p] (sub? env p (if (< i (count sp)) (nth sp i) sr))) tp))
         (or (nil? tr)
             (and (every? #(sub? env tr %) (drop (count tp) sp))
                  (sub? env tr sr)))
         (sub? env (:ret s) (:ret t)))))

(defn dispatch-arity
  "Clojure chooses a fixed arity before its variadic fallback."
  [arities n]
  (or (some #(when (and (not (:rest %)) (arity-fits? % n)) %) arities)
      (some #(when (and (:rest %) (arity-fits? % n)) %) arities)))

(defn dispatch-plan
  "Source branches and argument contracts needed to implement one expected arity."
  [source target]
  (let [minimum (arity-minimum target)]
    (if-not (:rest target)
      (when-let [a (dispatch-arity source minimum)] [[a target]])
      (when-let [rest-arity (first (filter :rest source))]
        (let [rest-minimum (arity-minimum rest-arity)
              fixed (filter #(not (:rest %)) source)
              fixed-counts (set (map #(count (:params %)) fixed))
              params-at (fn [n] (into (:params target) (repeat (- n (count (:params target))) (:rest target))))]
          (when (every? fixed-counts (range minimum rest-minimum))
            (let [target (dissoc target :rest-min)]
              (conj (vec (for [a fixed :let [n (count (:params a))] :when (<= minimum n)]
                           [a (assoc target :params (params-at n) :rest nil)]))
                    [rest-arity (assoc target :params (params-at (max rest-minimum minimum)))]))))))))

(defn- fn-sub?
  "Every target arity needs a source arity. Where overloads share a return type,
  any matching source arity can cover a target. Other overlaps need dispatch."
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
                 (apply = (map :ret sa))
                 (and (not-any? :rest sa)
                      (= (count sa) (count (distinct (map #(count (:params %)) sa)))))))
      (every? (fn [target] (some #(arity-sub? env % target) sa)) ta)
      :else (inconclusive! :unsupported-function))))

(defn- sub1? [env s t]
  (let [st (:t s) tt (:t t)]
    (cond
      ;; In ordinary checking, equality can establish reflexivity even for
      ;; opaque components. Comparing revisions must account for those gaps.
      (and *contract-subtyping* (or (= :unknown st) (= :unknown tt)))
      (inconclusive! (or (:contract-reason s) (:contract-reason t) :unknown))
      (and (= (strip s) (strip t))
           (or (not *contract-subtyping*) (not (#{:map :coll} st)))) true
      (#{:any :unknown} tt) true
      (= :never st) true
      (= :any st) (not *contract-subtyping*)
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
                                (cond (:items t) (inconclusive! :tuple-length)
                                      *strict-shapes* (inconclusive! :unsupported)
                                      :else true))
                     :map-of (and (= :map (:name s))
                                  (if *strict-shapes* (inconclusive! :unsupported) true))
                     false)
      (= :class st) (case tt
                      :class (class-sub? (:name s) (:name t))
                      :prim (class-is-prim? (:name s) (:name t))
                      :coll (when (and (:items t) (class-is-prim? (:name s) :vector))
                              (inconclusive! :tuple-length))
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
                     :coll (if (:items t)
                             (cond (:items s) (and (= (:kind s) (:kind t))
                                                   (= (count (:items s)) (count (:items t)))
                                                   (every? true? (map #(sub? env %1 %2) (:items s) (:items t))))
                                   (#{nil :vector} (:kind s)) (inconclusive! :tuple-length)
                                   :else false)
                             (and (or (nil? (:kind t)) (= (:kind s) (:kind t)))
                                  (sub? env (:of s) (:of t))))
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
        (every? (fn [s1]
                  (or (some #(sub1? env s1 %) tt)
                      ;; A structural value can be covered by several alternatives
                      ;; even when it is not contained in any one of them.
                      (when (and *contract-subtyping* (#{:map :coll} (:t s1))
                                 (< 1 (count (filter #(= (:t s1) (:t %)) tt))))
                        (inconclusive! :structural-union))))
                (members env s))))))

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

(defn contract-sub-result
  "Directional containment for public contract shapes. any? admits every input,
  and undeclared keys in an open map have no value constraint."
  [source target]
  (binding [*strict-shapes* true *contract-subtyping* true]
    (sub-result {} source target)))

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
      :coll (if (and (:items t) (::tuple-contract (meta t)))
              (str "(s/tuple" (apply str (map #(str " " (show %)) (:items t))) ")")
              (str "(s/coll-of " (show (:of t)) (when-let [k (:kind t)] (str " :kind " (name k) "?")) ")"))
      :fn (if-let [ars (seq (:arities t))]
            (let [ar (fn [a] (str "[" (str/join " " (concat (map show (:params a))
                                                            (repeat (get a :rest-min 0) (some-> (:rest a) show))
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

(defn type-form-op [ctx sym]
  (when (symbol? sym)
    (let [sym (get (:refers ctx) sym sym)
          n (namespace sym)
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

(def ^:private spec-regex-ops #{:s/cat :s/alt :s/? :s/* :s/+ :s/& :s/keys*})

(defn- regex-op [ctx form]
  (when (seq? form)
    (spec-regex-ops (type-form-op ctx (first form)))))

(defn- bounded-arities [arities]
  (let [limit 64
        branches (vec (take (inc limit) (distinct arities)))]
    (when (<= (count branches) limit) branches)))

(defn- regex-branches [ctx form]
  (let [empty-branch {:params [] :rest nil}
        operator (regex-op ctx form)
        operands (when operator (rest form))
        inner (first operands)]
    (case operator
      (:s/cat :s/alt)
      (when (and (even? (count operands))
                 (every? keyword? (take-nth 2 operands))
                 (or (= :s/cat operator) (seq operands)))
        (let [branches (mapv #(regex-branches ctx %) (take-nth 2 (next operands)))]
          (when (every? some? branches)
            (if (= :s/alt operator)
              (when (not-any? :rest (mapcat identity branches))
                (bounded-arities (mapcat identity branches)))
              (reduce (fn [prefix suffix]
                        (if (and (some :rest prefix) (some #(or (:rest %) (seq (:params %))) suffix))
                          (reduced nil)
                          (or (bounded-arities
                               (for [p prefix s suffix]
                                 (merge p (if (:rest p) (dissoc s :rest) s)
                                        {:params (into (:params p) (:params s))})))
                              (reduced nil))))
                      [empty-branch] branches)))))

      :s/? (when (= 1 (count operands))
             (when-let [branches (regex-branches ctx inner)]
               (when-not (some :rest branches)
                 (bounded-arities (cons empty-branch branches)))))
      (:s/* :s/+) (when (and (= 1 (count operands)) (not (regex-op ctx inner)))
                    [(cond-> (assoc empty-branch :rest (parse-type ctx inner))
                       (= :s/+ operator) (assoc :rest-min 1))])
      (:s/& :s/keys*) nil
      [(assoc empty-branch :params [(parse-type ctx form)])])))

(defn spec-args->arities [ctx args ret]
  (when (some? args)
    (if-let [branches (when (regex-op ctx args) (regex-branches ctx args))]
      (mapv #(assoc % :ret ret) branches)
      (do ((:fail ctx) args) nil))))

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
  "Equality modulo rank-1 binder names and equivalent rest minima."
  [a b]
  (let [normalize (fn [ty]
                    (let [ids (zipmap (map :id (:binders ty)) (range))]
                      (walk/postwalk
                       (fn [node]
                         (cond
                           (and (map? node) (contains? ids (:id node)))
                           (-> node (assoc :id (get ids (:id node))) (dissoc :name))
                           (and (map? node) (pos? (get node :rest-min 0)))
                           (-> node
                               (update :params into (repeat (:rest-min node) (:rest node)))
                               (dissoc :rest-min))
                           :else node))
                       ty)))]
    (or (= a b) (= (normalize a) (normalize b)))))

(defn parse-type
  "Reads a type form (plain data). ctx: {:aliases :imports :fail (fn [form])}."
  [ctx form]
  (let [p #(parse-type ctx %)
        fail #(do ((:fail ctx) form) unknown)]
    (cond
      (keyword? form) (if (namespace form) (ref-t form) (fail))
      (symbol? form) (let [nm (if (= "clojure.core" (namespace form)) (symbol (name form)) form)]
                       (cond (get (:type-vars ctx) nm) (get (:type-vars ctx) nm)
                             (pred-prims nm) (cond-> (prim (pred-prims nm))
                                               (#{'pos-int? 'nat-int? 'neg-int? 'pos?
                                                  'simple-keyword? 'qualified-keyword? 'list? 'sequential? 'fn?} nm)
                                               (with-meta {::unchecked-contract true}))
                             (= 'any? nm) any
                             (= 'some? nm) (with-meta any {::unchecked-contract true})
                             (= 'nil? nm) nil-t
                             (#{'true? 'false?} nm) (with-meta (prim :boolean) {::unchecked-contract true})
                             (= 't/unknown nm) unknown
                             (= 't/never nm) never
                             (:inside-all ctx) (fail)
                             :else (if-let [c (resolve-class (:imports ctx) nm)] (class-type c) (fail))))
      (set? form) (if (and (seq form) (every? #(or (string? %) (keyword? %) (number? %) (boolean? %)) form))
                    (apply union (map lit (sort-by pr-str form)))
                    (fail))
      (vector? form) (if (and (:inside-all ctx) (some #{'&} form)) (fail) (parse-sig ctx form))
      (seq? form)
      (case (type-form-op ctx (first form))
        :t/all (parse-all ctx form)
        :quote (p (second form))
        :s/nilable (vary-meta (union (p (second form)) nil-t) assoc ::shape-form 's/nilable)
        :s/or (vary-meta (apply union (map p (take-nth 2 (drop 2 form))))
                         assoc ::shape-form 's/or ::spec-tags (vec (take-nth 2 (rest form))))
        :t/or (vary-meta (apply union (map p (rest form))) assoc ::shape-form 't/or)
        (:s/and :t/and :t/exact) (vary-meta (p (second form)) assoc ::shape-form (first form) ::unchecked-contract true)
        :s/spec (p (second form))
        :s/keys (if (even? (count (rest form))) (parse-keys (rest form)) (fail))
        :s/merge {:t :merge :types (mapv p (rest form))}
        (:s/coll-of :s/every) (let [[_ inner & {:keys [kind] :as opts}] form
                                    kinds '{vector? :vector set? :set list? :seq seq? :seq}]
                                (cond-> {:t :coll :of (p inner) :kind (get kinds kind)}
                                  (or (some #(not= :kind %) (keys opts))
                                      (and (contains? opts :kind) (not (contains? (dissoc kinds 'list?) kind)))
                                      (= :s/every (type-form-op ctx (first form))))
                                  (with-meta {::unchecked-contract true})))
        :s/map-of {:t :map-of :k (p (nth form 1)) :v (p (nth form 2))}
        :s/tuple (with-meta (tuple (mapv p (rest form))) {::tuple-contract true})
        :s/fspec (if (:inside-all ctx) (fail) (let [{:keys [args ret]} (apply hash-map (rest form))
                       r (if ret (p ret) unknown)]
                   {:t :fn :arities (spec-args->arities ctx args r) :ret r}))
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
