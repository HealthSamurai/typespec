(ns typespec.types
  "Type representation, the spec-style type language (read and print), and subtyping."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [typespec.length :as length]
            [typespec.recursive-types :as recursive]))

(def any {:t :any})
(def unknown {:t :unknown})
(def never {:t :never})
(def nil-t {:t :nil})
(def some-t {:t :some})
(defn prim [n] {:t :prim :name n})
(defn lit [v] {:t :lit :val v})
(defn ref-t [k] {:t :ref :name k})
(defn assumption [ty] (::assumption (meta ty)))
(defn with-assumption [ty reason]
  (if reason (vary-meta ty assoc ::assumption reason) ty))

(defn may-be-nil-leaf? [ty]
  (and (#{:nil :unknown :any :rigid :var :ivar :all} (:t ty))
       (not (:non-nil? ty))))

(defn non-nil-leaf [ty]
  (case (:t ty)
    :nil never
    :unknown (assoc ty :non-nil? true)
    ty))

(defn literal-number-class [ty]
  (when (and (= :lit (:t ty)) (number? (:val ty)) (not (:ambiguous-number-box? ty)))
    (class (:val ty))))

;; These facts do not change the nominal type.
(defn strip [t] (dissoc t :platform :as :generic-failure :exact-array? :array-length :ambiguous-number-box?))

(defn union [& ts]
  (let [[flat nullable?]
        (loop [xs (seq ts) seen {} out [] nullable? false]
          (if (seq xs)
            (let [x (first xs)
                  more (rest xs)
                  shape (strip x)]
              (case (:t x)
                :union (recur (concat (:types x) more) seen out (or nullable? (:platform x)))
                :never (recur more seen out nullable?)
                (if-let [i (get seen shape)]
                  ;; Preserve platform nil risk and retain array facts shared by both paths.
                  (recur more seen
                         (update out i
                                 #(cond-> %
                                    (:platform x) (assoc :platform true)
                                    (not (:generic-failure x)) (dissoc :generic-failure)
                                    (not (:exact-array? x)) (dissoc :exact-array?)
                                    (not= (:array-length %) (:array-length x)) (dissoc :array-length)
                                    (not= (literal-number-class %) (literal-number-class x))
                                    (assoc :ambiguous-number-box? true)))
                         nullable?)
                  (recur more (assoc seen shape (count out)) (conj out x) nullable?))))
            [out nullable?]))
        result (cond (empty? flat) never
                     (some #(= :any (:t %)) flat) any
                     (some #(= :unknown (:t %)) flat)
                     (cond-> unknown
                       (and (not nullable?) (every? :non-nil? flat)) (assoc :non-nil? true))
                     (= 1 (count flat)) (first flat)
                     :else {:t :union :types flat})]
    (cond-> (with-assumption result (some assumption ts))
      (and nullable? (not (#{:any :unknown :nil :never} (:t result))))
      (assoc :platform true)
      (some #(-> % meta ::unchecked-contract) ts)
      (vary-meta assoc ::unchecked-contract true))))

(defn allow-platform-nil [ty]
  (if (:platform ty)
    (union (dissoc ty :platform) (with-meta nil-t {::platform-origin ty}))
    ty))

(defn effective-type [env ty]
  (if (:strict-nil env) (allow-platform-nil ty) ty))

(defn lit-prim [v]
  (prim (cond (string? v) :string (keyword? v) :keyword (integer? v) :int
              (number? v) :double (boolean? v) :boolean :else :symbol)))

(defn name-qualification [ty]
  (case (:t ty)
    :lit (when (ident? (:val ty)) (some? (namespace (:val ty))))
    :prim (:qualified? ty)
    nil))

(defn qualification-target [predicate]
  (case predicate
    qualified-keyword? (assoc (prim :keyword) :qualified? true)
    simple-keyword? (assoc (prim :keyword) :qualified? false)
    qualified-symbol? (assoc (prim :symbol) :qualified? true)
    simple-symbol? (assoc (prim :symbol) :qualified? false)
    qualified-ident? (union (assoc (prim :keyword) :qualified? true)
                            (assoc (prim :symbol) :qualified? true))
    simple-ident? (union (assoc (prim :keyword) :qualified? false)
                         (assoc (prim :symbol) :qualified? false))
    ident? (union (prim :keyword) (prim :symbol))
    nil))

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
  "JVM class name for a symbol (including Clojure array class symbols), or nil."
  [imports s]
  (let [s (str s)]
    (or (get imports (symbol s))
        (when (primitives s) s)
        (when-let [[_ component rank] (re-matches #"([^/]+)/([1-9])" s)]
          (when-let [cls (some-> (resolve-class imports component) load-class)]
            (.getName ^Class (nth (iterate #(.arrayType ^Class %) cls) (parse-long rank)))))
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

(defn- bounded-branches [branches]
  (let [limit 64
        result (vec (take (inc limit) (distinct branches)))]
    (when (<= (count result) limit) result)))

(declare merge-spec-maps)

(defn- merge-spec-alternatives [env seen parts]
  (reduce (fn [prefix suffix]
            (or (bounded-branches (for [a prefix b suffix] (merge-spec-maps env seen a b)))
                (reduced nil)))
          (first parts) (rest parts)))

(defn members
  "Flat list of alternatives of a type, with refs resolved."
  ([env t] (members env t #{}))
  ([env t seen]
   (let [t (effective-type env t)]
     (case (:t t)
       :ref (if (or (seen (:name t)) (get-in env [:recursive-types :invalid (:name t)]))
              [unknown]
              (if-let [d (get-in env [:specs (:name t) :type])]
                (recur env d (conj seen (:name t)))
                [unknown]))
       :union (mapcat #(members env % seen) (:types t))
       :merge (let [parts (mapv #(bounded-branches (members env % seen)) (:types t))]
                (or (when (and (seq parts) (every? seq parts)
                               (every? #(= :map (:t %)) (mapcat identity parts)))
                      (seq (merge-spec-alternatives env seen parts)))
                    [unknown]))
       [t]))))

(defn resolve-t [env t] (apply union (members env t)))

(defn nilable? [env t] (boolean (some #(= :nil (:t %)) (members env t))))

(defn remove-nil [env t]
  (letfn [(without-nil [ty seen]
            (let [ty (if (:strict-nil env) (dissoc ty :platform) ty)]
              (case (:t ty)
                :nil never
                :union (apply union (map #(without-nil % seen) (:types ty)))
                :ref (cond
                       (contains? seen (:name ty)) unknown
                       (nilable? env ty) (recur (get-in env [:specs (:name ty) :type]) (conj seen (:name ty)))
                       :else ty)
                ty)))]
    (without-nil t #{})))

;; ---- subtyping ----

(declare sub? sub-result)
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

(defn map-of-entry [env ty k]
  (when-not (= :mismatch (:status (sub-result env (lit k) (:k ty))))
    (:v ty)))

(defn- map-fields-fit? [env fields target]
  (every? (fn [[k v]]
            (or (= :never (:t v))
                (and (sub? env (lit k) (:k target)) (sub? env v (:v target)))))
          fields))

(defn- map-index-sub? [env source target]
  (if-let [index (:map-index target)]
    (and (map-fields-fit? env (remove (fn [[k]] (or (contains? (:req target) k)
                                                     (contains? (:opt target) k)))
                                     (concat (:req source) (:opt source)))
                          index)
         (if-let [source-index (:map-index source)]
           (sub? env source-index index)
           (or (:complete-keys? source) (inconclusive! :unsupported))))
    true))

(defn- map-sub? [env s t]
  (let [required (fn [[k tv]] (when-let [sv (get (:req s) k)] (sub? env sv tv)))
        optional (fn [[k tv]]
                   (if-let [sv (or (get (:req s) k) (get (:opt s) k)
                                   (when-let [index (:map-index s)]
                                     (or (map-of-entry env index k) never)))]
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
            (and (empty? reasons) (map-index-sub? env s t)))))
      (and (every? required (:req t)) (every? optional (:opt t)) (map-index-sub? env s t)))))

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
  "Source branches and argument contracts needed to implement one expected arity.
  With partial coverage, intersect the arities implemented by a single method."
  ([source target] (dispatch-plan source target :total))
  ([source target coverage]
   (let [minimum (arity-minimum target)]
     (if-not (:rest target)
       (when-let [a (dispatch-arity source minimum)] [[a target]])
       (let [rest-arity (first (filter :rest source))
             fixed (remove :rest source)
             fixed-counts (into #{} (map arity-minimum) fixed)
             rest-minimum (when rest-arity
                            (let [n (arity-minimum rest-arity)]
                              (if (fixed-counts n) (inc n) n)))
             params-at (fn [n] (into (:params target) (repeat (- n (count (:params target))) (:rest target))))]
         (when (or (= :partial coverage)
                   (and rest-minimum (every? fixed-counts (range minimum rest-minimum))))
           (let [target (dissoc target :rest-min)]
             (cond-> (vec (for [a fixed :let [n (arity-minimum a)] :when (<= minimum n)]
                            [a (assoc target :params (params-at n) :rest nil)]))
               rest-arity (conj [rest-arity (assoc target :params (params-at (max rest-minimum minimum)))])))))))))

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

(defn- atom-content-shape [env content]
  (let [budget (volatile! 1024)
        opaque? (volatile! false)]
    (letfn [(opaque [] (vreset! opaque? true) nil)
            (shape [node active]
              (cond
                (neg? (vswap! budget dec)) (opaque)
                (or (assumption node) (::unchecked-contract (meta node))) (opaque)
                (= :unknown (:t node)) (opaque)
                (= :ref (:t node))
                (let [name (:name node)]
                  (if-let [target (when-not (active name) (get-in env [:specs name :type]))]
                    (shape target (conj active name))
                    (opaque)))
                (= :union (:t node)) (update node :types #(into #{} (map (fn [part] (shape part active))) %))
                :else (walk/walk #(shape % active) identity
                                 (if (keyword? (:t node)) (dissoc node :as :fresh) node))))]
      (let [result (shape content #{})]
        (when-not @opaque? result)))))

(defn- atom-sub? [env actual expected]
  (let [a (atom-content-shape env (:of actual))
        b (atom-content-shape env (:of expected))]
    (if (and a b) (= a b) (inconclusive! :atom-content))))

(defn- sub1? [env s t]
  (let [st (:t s) tt (:t t)]
    (cond
      ;; In ordinary checking, equality can establish reflexivity even for
      ;; opaque components. Comparing revisions must account for those gaps.
      (and *contract-subtyping* (or (= :unknown st) (= :unknown tt)))
      (inconclusive! (or (:contract-reason s) (:contract-reason t) :unknown))
      (= :atom st tt) (atom-sub? env s t)
      (and (= (strip s) (strip t))
           (or (not *contract-subtyping*) (not (#{:map :coll} st)))) true
      (#{:any :unknown} tt) (or (not (:non-nil? t)) (sub? env s some-t))
      (= :never st) true
      (= :any st) (not *contract-subtyping*)
      (= :unknown st) (cond
                        (and (:non-nil? s) (= :some tt)) true
                        (and (:non-nil? s) (= :nil tt)) false
                        :else (inconclusive! :unknown))
      (= :rigid tt) false
      ;; An arbitrary type parameter fits only itself and top (handled above):
      ;; without a bound this is a definite mismatch, not missing information.
      (= :rigid st) (if (#{:unknown :any} (:t (:bound s)))
                     false
                     (sub? env (:bound s) t))
      (or (#{:var :ivar :all} st) (#{:var :ivar :all} tt))
      (inconclusive! :unresolved-variable)
      (= :nil st) (boolean (:platform t))
      (= :some tt) true
      (= :nil tt) false
      (= :lit st) (if (= :lit tt)
                    (= (:val s) (:val t))
                    (sub1? env (cond-> (lit-prim (:val s))
                                 (contains? t :qualified?) (assoc :qualified? (name-qualification s)))
                           t))
      (= :lit tt) false
      (= :prim st) (case tt
                     :prim (and (prim-sub? (:name s) (:name t))
                                (or (not (contains? t :qualified?))
                                    (= (:qualified? s) (:qualified? t))))
                     :class (prim-is-class? (:name s) (:name t))
                     ;; A known kind never fits a different :kind (set? is not a vector).
                     :coll (and (contains? #{:vector :set :seq :coll} (:name s))
                                (or (nil? (:kind t)) (= :coll (:name s)) (= (:kind t) (:name s)))
                                (cond (:items t) (inconclusive! :tuple-length)
                                      (length/constrained? t) (inconclusive! :collection-length)
                                      *strict-shapes* (inconclusive! :unsupported)
                                      :else true))
                     :map-of (and (= :map (:name s))
                                  (if *strict-shapes* (inconclusive! :unsupported) true))
                     false)
      (= :class st) (case tt
                      :class (class-sub? (:name s) (:name t))
                      :atom (and (class-sub? (:name s) "clojure.lang.Atom")
                                 (inconclusive! :atom-content))
                      :prim (class-is-prim? (:name s) (:name t))
                      :coll (cond
                              (and (:items t) (class-is-prim? (:name s) :vector))
                              (inconclusive! :tuple-length)
                              (and (length/constrained? t)
                                   (class-is-prim? (:name s) (or (:kind t) :coll)))
                              (inconclusive! :collection-length))
                      false)
      (= :map st) (case tt
                    :map (map-sub? env s t)
                    :prim (contains? #{:map :coll} (:name t))
                    :map-of (and (map-fields-fit? env (concat (:req s) (:opt s)) t)
                                 (if-let [index (:map-index s)]
                                   (sub? env index t)
                                   (or (not *strict-shapes*) (:complete-keys? s)
                                       (inconclusive! :unsupported))))
                    :class (prim-is-class? :map (:name t))
                    false)
      (= :map-of st) (case tt
                       :map-of (and (sub? env (:k s) (:k t)) (sub? env (:v s) (:v t)))
                       :prim (contains? #{:map :coll} (:name t))
                       :map (if (:map-index t)
                              (map-sub? env {:map-index s} t)
                              (empty? (:req t)))
                       :class (prim-is-class? :map (:name t))
                       false)
      (= :atom st) (and (= :class tt) (class-sub? "clojure.lang.Atom" (:name t)))
      (= :coll st) (case tt
                     :coll (if-let [positions (:items t)]
                             (cond (:items s) (and (= (:kind s) (:kind t))
                                                   (length/subset? s t)
                                                   (every? true? (map #(sub? env %1 %2) (:items s) positions)))
                                   (and (length/constrained? s) (not (length/subset? s t))) false
                                   (and (length/subset? s t) (= (:kind s) (:kind t)))
                                   (every? #(sub? env (:of s) %) positions)
                                   (#{nil :vector} (:kind s)) (inconclusive! :tuple-length)
                                   :else false)
                             (and (or (nil? (:kind t)) (= (:kind s) (:kind t)))
                                  (length/subset? s t)
                                  (if-let [items (:items s)]
                                    (every? #(sub? env % (:of t)) items)
                                    (or (= 0 (:max-count s)) (sub? env (:of s) (:of t))))))
                     :prim (contains? (get {:vector #{:vector :coll} :set #{:set :coll} :seq #{:seq :coll}} (:kind s) #{:coll}) (:name t))
                     :class (if-let [c (:jvm-class s)]
                              (class-sub? c (:name t))
                              (prim-is-class? (or (:kind s) :coll) (:name t)))
                     false)
      (= :fn st) (case tt
                    :fn (boolean (fn-sub? env s t))
                    :class (let [c (load-class (:name t))]
                             (boolean (and c (.isAssignableFrom ^Class c clojure.lang.IFn))))
                    :prim (= :fn (:name t))
                    false)
      :else false)))

(defn- structural-result [env s t]
  (recursive/compare-types
   env s t
   (fn [a b]
     (binding [*inconclusive* (atom #{})]
       (cond
         (sub1? env a b) {:status :proved}
         (seq @*inconclusive*) {:status :inconclusive :reasons @*inconclusive*}
         :else {:status :mismatch :actual a :expected b})))
   *contract-subtyping*
   #(effective-type env %)))

(defn- recursive-result [env s t]
  (when (and (<= *depth* 12) (recursive/candidate? env s t))
    (binding [*strict-shapes* true]
      (structural-result env s t))))

(defn nested-platform-mismatch
  "Locate a platform nil after a failed structural comparison."
  [env actual expected original-result]
  (when (:strict-nil env)
    (let [result (if (seq (:path original-result))
                   original-result
                   (when (recursive/contains-structural? :platform actual)
                     (structural-result env actual expected)))]
      (when (and (= :mismatch (:status result)) (seq (:path result)))
        (when-let [origin (some-> (:actual result) meta ::platform-origin)]
          (assoc result :platform-origin origin))))))

(defn- ordinary-sub? [env s t]
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

(defn sub?
  "Proven compatibility within the supported subset, with explicit any as a bypass.
  Unknown is a safe top type: every type fits it, but it does not fit a concrete
  type. Exhausting the recursion budget returns false, not evidence of a subtype.
  Callers wanting tolerant checking must handle unknown separately."
  [env s t]
  (if-let [result (recursive-result env s t)]
    (do (run! inconclusive! (:reasons result))
        (= :proved (:status result)))
    (ordinary-sub? env s t)))

(defn sub-result
  "Companion to sub?: {:status :proved|:mismatch|:inconclusive :reasons #{...}}.
  Unknown sources and exhausted recursion cannot prove a concrete mismatch.
  Any remains an explicit bypass, not a runtime guarantee. Reasons are gathered
  conservatively across alternatives; a successful proof takes precedence."
  [env s t]
  (or (some-> (recursive-result env s t) (update :reasons #(or % #{})))
      (binding [*inconclusive* (atom #{})]
        (let [proved? (ordinary-sub? env s t)
              reasons @*inconclusive*]
          {:status (cond proved? :proved (seq reasons) :inconclusive :else :mismatch)
           :reasons (if proved? #{} reasons)}))))

(defn contract-sub-result
  "Directional containment for public contract shapes. any? admits every input,
  and undeclared keys in an open map have no value constraint."
  [source target]
  (binding [*strict-shapes* true *contract-subtyping* true]
    (sub-result {} source target)))

(defn- scalar-contract [env seen type]
  (let [parts (members env type seen)]
    (when (every? #(#{:prim :class :lit :some :nil :never :any} (:t %)) parts)
      (apply union parts))))

(defn- scalar-category [{:keys [t name val]}]
  (case t
    :prim (cond (#{:int :double :number} name) :number
                (#{:string :keyword :symbol :boolean :uuid} name) name)
    :lit (cond (number? val) :number
               (string? val) :string
               (keyword? val) :keyword
               (symbol? val) :symbol
               (boolean? val) :boolean
               (instance? java.util.UUID val) :uuid)
    nil))

(defn- disjoint-scalar-types? [left right]
  (let [lt (:t left) rt (:t right)
        nonnil? #{:lit :prim :class :some}]
    (cond
      (or (:platform left) (:platform right)) false
      (and (= :lit lt) (= :lit rt)) (not= (:val left) (:val right))
      (= :nil lt) (contains? nonnil? rt)
      (= :nil rt) (contains? nonnil? lt)
      :else (let [a (scalar-category left) b (scalar-category right)]
              (and a b (not= a b))))))

(defn- scalar-intersection [left right]
  (let [ls (members {} left)
        rs (members {} right)]
    (when (<= (* (count ls) (count rs)) 256)
      (let [intersections (for [l ls r rs]
                            (cond
                              (= :proved (:status (contract-sub-result l r))) l
                              (= :proved (:status (contract-sub-result r l))) r
                              (disjoint-scalar-types? l r) never))]
        (when (every? some? intersections)
          (apply union intersections))))))

(defn- narrower-scalar-contract [env seen a b]
  (when (and a b (not= a b))
    (let [left (scalar-contract env seen a)
          right (scalar-contract env seen b)]
      (when (and left right)
        (cond
          (= :any (:t left)) b
          (= :any (:t right)) a
          (= :proved (:status (contract-sub-result left right))) a
          (= :proved (:status (contract-sub-result right left))) b
          :else (scalar-intersection left right))))))

(defn- merge-spec-maps [env seen a b]
  (let [required (merge (:req a) (:req b))
        optional (apply dissoc (merge (:opt a) (:opt b)) (keys required))
        left (merge (:opt a) (:req a))
        right (merge (:opt b) (:req b))
        refine (fn [row]
                 (reduce-kv (fn [result k _]
                              (if-let [field (narrower-scalar-contract env seen (get left k) (get right k))]
                                (assoc result k field)
                                result))
                            row row))]
    {:t :map :req (refine required) :opt (refine optional)}))

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
      :any "any?" :unknown (if (:non-nil? t) "some?" "t/unknown") :never "t/never" :nil "nil?" :some "some?"
      (:var :ivar :rigid) (str (:name t))
      :all (str "(t/all ["
                (str/join " " (map (fn [{:keys [name bound]}]
                                      (if (= :unknown (:t bound)) (str name)
                                        (str "(" name " :< " (show bound) ")"))) (:binders t)))
                "] " (show (:body t)) ")")
      :prim (str (when (contains? t :qualified?)
                   (if (:qualified? t) "qualified-" "simple-"))
                 (name (:name t)) "?")
      :lit (pr-str (:val t))
      :ref (show-kw (:name t))
      :class (:name t)
      :atom (str "(t/atom " (show (:of t)) ")")
      :union (let [ts (:types t)
                   others (remove #(= :nil (:t %)) ts)
                   core (cond (= 1 (count others)) (show (first others))
                              (every? #(= :lit (:t %)) others) (str "#{" (str/join " " (map show others)) "}")
                              :else (str "(t/or " (str/join " " (map show others)) ")"))]
               (if (some #(= :nil (:t %)) ts) (str "(s/nilable " core ")") core))
      :merge (str "(s/merge " (str/join " " (map show (:types t))) ")")
      :map (cond
             (:fresh t)
             (str "{" (str/join ", " (map (fn [[k v]] (str (pr-str k) " " (show v))) (:req t))) "}")
             (some (complement keyword?) (concat (keys (:req t)) (keys (:opt t))))
             "map?"
             :else
             (str "(s/keys"
                  (when (seq (:req t)) (str " :req [" (str/join " " (map show-kw (keys (:req t)))) "]"))
                  (when (seq (:opt t)) (str " :opt [" (str/join " " (map show-kw (keys (:opt t)))) "]"))
                  ")"))
      :map-of (str "(s/map-of " (show (:k t)) " " (show (:v t)) ")")
      :coll (if (and (:items t) (::tuple-contract (meta t)))
              (str "(s/tuple" (apply str (map #(str " " (show %)) (:items t))) ")")
              (str "(s/coll-of " (show (:of t)) (when-let [k (:kind t)] (str " :kind " (name k) "?"))
                   (when-let [lo (:min-count t)] (str " :min-count " lo))
                   (when-let [hi (:max-count t)] (str " :max-count " hi))
                   ")"))
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
    keyword? :keyword
    symbol? :symbol boolean? :boolean uuid? :uuid inst? :inst
    map? :map vector? :vector set? :set seq? :seq sequential? :coll coll? :coll
    fn? :fn ifn? :fn})

(defn- builtin-predicate-type [predicate]
  (if-let [primitive (pred-prims predicate)]
    (cond-> (prim primitive)
      (#{'pos-int? 'nat-int? 'neg-int? 'pos? 'sequential? 'fn?} predicate)
      (with-meta {::unchecked-contract true}))
    (case predicate
      any? any
      some? some-t
      nil? nil-t
      list? (class-type "clojure.lang.IPersistentList")
      char? (class-type "java.lang.Character")
      bytes? (class-type "[B")
      class? (class-type "java.lang.Class")
      true? (lit true)
      false? (lit false)
      nil)))

(defn type-form-op [ctx sym]
  (when (symbol? sym)
    (let [sym (get (:refers ctx) sym sym)
          n (namespace sym)
          full (when n (str (get (:aliases ctx) (symbol n) n)))]
      (cond (#{"clojure.spec.alpha" "s"} full) (keyword "s" (name sym))
            (= "t" n) (keyword "t" (name sym))
            (and (= "clojure.core" full) (#{"fn" "partial"} (name sym))) (keyword (name sym))
            (nil? n) (if (= sym (get-in ctx [:refer-clojure :rename 'partial])) :partial (keyword (name sym)))
            :else nil))))

(declare parse-type)

(defn core-name
  "Unqualified name of a core symbol in a source context, or nil."
  [ctx head]
  (when (and (symbol? head)
             (not (contains? (:locals ctx) head))
             (not (contains? (:defined-symbols ctx) head)))
    (let [sym (get (:refers ctx) head head)
          n (namespace sym)
          {:keys [only exclude rename] :as refers} (:refer-clojure ctx)
          original (when-not n
                     (or (some (fn [[from to]] (when (= sym to) from)) rename)
                         (when-not (contains? rename sym) sym)))]
      (cond
        (and n (= 'clojure.core (get (:aliases ctx) (symbol n) (symbol n))))
        (symbol (name sym))

        (and original (not (some #{original} exclude))
             (or (not (contains? refers :only)) (some #{original} only)))
        original))))

(defn- literal-set-member? [value]
  (or (nil? value) (string? value) (keyword? value) (number? value) (boolean? value)))

(defn- literal-set-type [values]
  (apply union (map #(if (nil? %) nil-t (lit %)) (sort-by pr-str values))))

(defn- instance-predicate-type [ctx class-form]
  (when (and (symbol? class-form) (not (primitives (str class-form)))
             (not (contains? (:locals ctx) class-form))
             (not (contains? (:refers ctx) class-form))
             (not (contains? (:defined-symbols ctx) class-form)))
    (when-let [class-name (resolve-class (:imports ctx) class-form)]
      (if (= "java.lang.Object" class-name) some-t (class-type class-name)))))

(defn- partial-instance-type [ctx form]
  (let [[head predicate class-form & extra] form]
    (when (and (= 'partial (core-name ctx head))
               (= 'instance? (core-name ctx predicate)) (empty? extra))
      (instance-predicate-type ctx class-form))))

(defn- refine-predicate-type [left right]
  (cond
    (and (= some-t right) (nilable? {} left)) (remove-nil {} left)
    (and (= some-t left) (nilable? {} right)) (remove-nil {} right)
    :else (or (narrower-scalar-contract {} #{} left right) left)))

(defn- predicate-expression-type [ctx parameter? expression]
  (when (seq? expression)
    (let [[head arg value & more] expression
          op (core-name ctx head)]
      (case op
        contains? (when (and (set? arg) (every? literal-set-member? arg)
                             (parameter? value) (empty? more))
                    (literal-set-type arg))
        instance? (when (and (parameter? value) (empty? more))
                    (instance-predicate-type ctx arg))
        and (let [checks (mapv (fn [clause] [clause (predicate-expression-type ctx parameter? clause)])
                               (rest expression))]
              (when-let [known (seq (keep second checks))]
                (when-let [opaque (:fail ctx)]
                  (doseq [[clause type] checks :when (nil? type)]
                    (opaque clause)))
                (vary-meta (reduce refine-predicate-type known)
                           assoc ::unchecked-contract true)))
        or (let [branches (mapv #(predicate-expression-type ctx parameter? %) (rest expression))]
             (when (and (seq branches) (every? some? branches))
               (apply union branches)))
        (when (and (= 2 (count expression)) (parameter? arg)
                   (contains? '#{list? vector? map? set? string? keyword? symbol? boolean?
                                 true? false? nil? some? any? char? bytes? class? int? number?} op))
          (parse-type (dissoc ctx :type-vars) (symbol "clojure.core" (name op))))))))

(defn predicate-spec-type
  "Type of a recognized unary Spec predicate, or nil for an opaque expression."
  [ctx form]
  (let [[head params body & more] form
        param (first (when (vector? params) params))
        reader-fn? (:typespec.parse/reader-fn (meta form))]
    (when (and (or reader-fn? (= 'fn* head) (= 'fn (core-name ctx head)))
               (simple-symbol? param) (= 1 (count params)) (empty? more)
               (not-any? (comp :typespec.parse/source-meta meta) [param params]))
      (predicate-expression-type (update ctx :defined-symbols (fnil conj #{}) param)
                                 (fn [argument] (or (= param argument) (and reader-fn? (= '%1 argument))))
                                 body))))

(defn- named-spec-input [ctx name seen read-input]
  (when (and (not (seen name)) (< (count seen) 64))
    (let [definitions (:spec-definitions ctx)]
      (when-let [{:keys [spec-form spec-context]} (definitions name)]
        (let [source (assoc spec-context :spec-definitions definitions :fail (constantly nil))]
          (when-let [type (read-input source spec-form (conj seen name))]
            (vary-meta type assoc ::referenced-specs #{name})))))))

(defn- transparent-predicate-type [ctx form seen]
  (cond
    (qualified-keyword? form) (named-spec-input ctx form seen transparent-predicate-type)
    (symbol? form) (let [predicate (core-name ctx form)]
                     (or (qualification-target predicate) (builtin-predicate-type predicate)))
    (and (set? form) (every? literal-set-member? form)) (literal-set-type (filter identity form))
    (seq? form) (if (and (= :s/nilable (type-form-op ctx (first form))) (= 2 (count form)))
                  (when-let [type (transparent-predicate-type ctx (second form) seen)]
                    (vary-meta (union type nil-t) merge (select-keys (meta type) [::referenced-specs])))
                  (or (partial-instance-type ctx form) (predicate-spec-type ctx form)))))

(defn spec-option-map
  "Reads keyword argument pairs with an optional trailing map."
  [options]
  (if (even? (count options))
    (apply hash-map options)
    (when (map? (last options))
      (into (apply hash-map (butlast options)) (last options)))))

(defn- conjunction-input-shape [ctx form seen]
  (cond
    (qualified-keyword? form) (named-spec-input ctx form seen conjunction-input-shape)
    (seq? form)
    (case (type-form-op ctx (first form))
      (:s/keys :s/tuple) (parse-type ctx form)
      :s/map-of (let [{:keys [kind]} (spec-option-map (drop 3 form))]
                  (when (or (nil? kind) (= 'map? (core-name ctx kind)))
                    (parse-type ctx form)))
      :s/coll-of (let [{:keys [kind]} (spec-option-map (drop 2 form))]
                   (when (or (nil? kind) (#{'vector? 'set? 'list? 'seq?} (core-name ctx kind)))
                     (parse-type ctx form)))
      nil)))

(defn- spec-conjunction-type [ctx forms]
  ;; The first transforming spec can still constrain the original input
  ;; when all earlier predicates preserve it.
  (let [prefix (seq (take-while some? (map #(transparent-predicate-type ctx % #{}) forms)))]
    (if prefix
      (let [leading (first forms)
            refined (reduce refine-predicate-type prefix)
            shape (conjunction-input-shape ctx (nth forms (count prefix) nil) #{})
            refined (if (and shape (= :proved (:status (contract-sub-result shape refined))))
                      shape
                      refined)
            type (if (and (qualified-keyword? leading) (= refined (first prefix)))
                   (ref-t leading)
                   refined)
            references (into #{} (mapcat #(-> % meta ::referenced-specs)) (cons refined prefix))]
        (cond-> type (seq references) (vary-meta assoc ::referenced-specs references)))
      (parse-type ctx (first forms)))))

(defn- flat-keys [x] (cond (keyword? x) [x] (seq? x) (mapcat flat-keys (rest x)) :else []))

(defn- conjunctive-keys [groups]
  (reduce (fn [prefix suffix]
            (or (bounded-branches (for [a prefix b suffix] (into a b)))
                (reduced nil)))
          [#{}] groups))

(defn- key-group-branches [key-name form]
  (cond
    (keyword? form) [#{(key-name form)}]
    (seq? form)
    (let [[op & children] form
          branches (mapv #(key-group-branches key-name %) children)]
      (when (and (seq children) (every? some? branches))
        (case op
          and (conjunctive-keys branches)
          or (bounded-branches (mapcat identity branches))
          nil)))))

(defn- spec-keys-options [args]
  (when-let [{:keys [req opt req-un opt-un] :as options} (spec-option-map args)]
    (when (every? #(or (nil? %) (sequential? %)) [req opt req-un opt-un])
      options)))

(defn- parse-keys [{:keys [req opt req-un opt-un]}]
  (let [short-key (comp keyword name)
        un (fn [ks] (map (fn [k] [(short-key k) (ref-t k)]) ks))
        q (fn [ks] (map (fn [k] [k (ref-t k)]) ks))
        plain #(filter keyword? %)
        [qualified-groups unqualified-groups] (map #(filter seq? %) [req req-un])
        required (concat (q (plain req)) (un (plain req-un)))
        optional (concat (q (concat opt (mapcat flat-keys qualified-groups)))
                         (un (concat opt-un (mapcat flat-keys unqualified-groups))))
        req-map (into (array-map) required)
        opt-map (into (array-map) optional)
        conflicts (into (set (filter req-map (keys opt-map)))
                        (keep (fn [[k entries]] (when (next (distinct entries)) k)))
                        (group-by first (concat required optional)))
        grouped? (or (seq qualified-groups) (seq unqualified-groups))
        groups (concat (map #(key-group-branches identity %) qualified-groups)
                       (map #(key-group-branches short-key %) unqualified-groups))
        row {:t :map :req req-map :opt opt-map}
        result (if grouped?
                 (if-let [branches (when (every? some? groups) (conjunctive-keys groups))]
                   (apply union (for [ks branches]
                                  (let [selected (select-keys opt-map (sort-by pr-str ks))]
                                    {:t :map :req (merge selected req-map)
                                     :opt (apply dissoc opt-map ks)})))
                   (vary-meta row assoc ::unchecked-contract true))
                 row)]
    (vary-meta result merge
               (cond-> {}
                 (seq conflicts) (assoc ::shape-conflicts (sort-by pr-str conflicts))
                 grouped? (assoc ::shape-form 's/keys)))))

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

(defn- resolve-regex [ctx form]
  (loop [ctx ctx form form]
    (if (regex-op ctx form)
      {:context ctx :form form}
      (when (qualified-keyword? form)
        (when-let [{:keys [spec-form spec-context]} (get-in ctx [:spec-definitions form])]
          (if (or (contains? (:active-regex-specs ctx) form)
                  (neg? (vswap! (:regex-expansion-budget ctx) dec)))
            {:opaque? true}
            (recur (assoc spec-context
                          :fail (:fail ctx)
                          :spec-definitions (:spec-definitions ctx)
                          :regex-expansion-budget (:regex-expansion-budget ctx)
                          :active-regex-specs (conj (:active-regex-specs ctx) form))
                   spec-form)))))))

(declare regex-branches)

(defn- regex-tail? [branch]
  (or (:rest branch) (:kwargs branch)))

(defn- regex-form-branches [ctx form]
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
              (bounded-branches (mapcat identity branches))
              (reduce (fn [prefix suffix]
                        (if (and (some regex-tail? prefix) (some #(or (regex-tail? %) (seq (:params %))) suffix))
                          (reduced nil)
                          (or (bounded-branches
                               (for [p prefix s suffix]
                                 (merge p (if (regex-tail? p) (dissoc s :rest) s)
                                        {:params (into (:params p) (:params s))})))
                              (reduced nil))))
                      [empty-branch] branches)))))

      :s/? (when (= 1 (count operands))
             (when-let [branches (regex-branches ctx inner)]
               (bounded-branches (cons empty-branch branches))))
      (:s/* :s/+) (when (and (= 1 (count operands)) (not (resolve-regex ctx inner)))
                    [(cond-> (assoc empty-branch :rest (parse-type ctx inner))
                       (= :s/+ operator) (assoc :rest-min 1))])
      :s/& (when (seq operands)
             (when-let [branches (regex-branches ctx inner)]
               ;; The remaining predicates inspect the conformed arguments.
               ;; They can reject a match without enlarging the argument shape.
               (run! (:fail ctx) (rest operands))
               branches))
      :s/keys* (when-let [options (spec-keys-options operands)]
                 [(assoc empty-branch :kwargs (parse-keys options) :keyword-regex? true)]))))

(defn- regex-branches [ctx form]
  (if-let [{source :context regex :form} (resolve-regex ctx form)]
    (when regex (regex-form-branches source regex))
    [{:params [(parse-type ctx form)] :rest nil}]))

(defn spec-args->arities [ctx args ret]
  (when (some? args)
    (let [ctx (assoc ctx :active-regex-specs #{} :regex-expansion-budget (volatile! 256))
          {source :context regex :form} (resolve-regex ctx args)]
      (if-let [branches (when regex (regex-form-branches source regex))]
        (mapv #(assoc % :ret ret) branches)
        (do ((:fail ctx) args) nil)))))

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

(defn- collection-type [ctx [_ inner :as form] {:keys [kind min-count max-count count] :as opts}]
  (let [kinds '{coll? nil vector? :vector set? :set list? :seq seq? :seq}
        kind-name (if kind (core-name ctx kind) 'coll?)
        kind-type (get kinds kind-name)
        ;; Spec uses bounded-count for :count. An uncounted seq can satisfy
        ;; it with additional elements, so only counted kinds get an upper bound.
        exact? (and (some? count) (#{:vector :set} kind-type))
        declared (remove nil? [min-count max-count count])]
    (if (and (contains? kinds kind-name)
             (every? #(and (integer? %) (<= 0 % Integer/MAX_VALUE)) declared))
      (let [lower (max (or min-count 0) (or count 0))
            upper (if exact? (min count (or max-count count)) max-count)]
        (if (and upper (> lower upper))
          (do ((:fail ctx) form) unknown)
          (cond-> (length/with-bounds {:t :coll :of (parse-type ctx inner) :kind kind-type} lower upper)
            (or (some #(not (#{:kind :count :min-count :max-count} %)) (keys opts))
                (= 'list? kind-name)
                (and (some? count) (not exact?))
                (= :s/every (type-form-op ctx (first form))))
            (with-meta {::unchecked-contract true}))))
      (do ((:fail ctx) form) unknown))))

(defn parse-type
  "Reads a type form (plain data). ctx: {:aliases :imports :fail (fn [form])}."
  [ctx form]
  (let [p #(parse-type ctx %)
        fail #(do ((:fail ctx) form) unknown)]
    (cond
      (keyword? form) (if (namespace form) (ref-t form) (fail))
      (symbol? form) (let [nm (if (= "clojure.core" (namespace form)) (symbol (name form)) form)
                           core (core-name ctx form)
                           predicate (if (:spec? ctx) core nm)
                           qualification (qualification-target core)]
                       (or (get (:type-vars ctx) nm) qualification (builtin-predicate-type predicate)
                           (case nm
                             t/unknown unknown
                             t/never never
                             (if-let [c (when-not (:inside-all ctx) (resolve-class (:imports ctx) nm))]
                               (class-type c)
                               (fail)))))
      (set? form) (if (:spec? ctx)
                    ;; A set used as a predicate returns the member itself.
                    (if (every? literal-set-member? form)
                      (literal-set-type (filter identity form))
                      (fail))
                    (if (and (seq form) (not (contains? form nil)) (every? literal-set-member? form))
                      (literal-set-type form)
                      (fail)))
      (vector? form) (if (and (:inside-all ctx) (some #{'&} form)) (fail) (parse-sig ctx form))
      (seq? form)
      (case (type-form-op ctx (first form))
        :t/all (parse-all ctx form)
        :t/atom (if (= 2 (count form)) {:t :atom :of (p (second form))} (fail))
        :quote (p (second form))
        :s/nilable (vary-meta (union (p (second form)) nil-t) assoc ::shape-form 's/nilable)
        :s/or (vary-meta (apply union (map p (take-nth 2 (drop 2 form))))
                         assoc ::shape-form 's/or ::spec-tags (vec (take-nth 2 (rest form))))
        :t/or (vary-meta (apply union (map p (rest form))) assoc ::shape-form 't/or)
        :s/and (vary-meta (if (:spec? ctx) (spec-conjunction-type ctx (rest form)) (p (second form)))
                          assoc ::shape-form (first form) ::unchecked-contract true)
        (:t/and :t/exact) (vary-meta (p (second form)) assoc ::shape-form (first form) ::unchecked-contract true)
        :s/spec (p (second form))
        :s/nonconforming (if (= 2 (count form)) (p (second form)) (fail))
        :s/with-gen (if (= 3 (count form)) (p (second form)) (fail))
        :s/keys (if-let [options (spec-keys-options (rest form))] (parse-keys options) (fail))
        :s/merge {:t :merge :types (mapv p (rest form))}
        (:s/coll-of :s/every) (if-let [options (when (next form) (spec-option-map (drop 2 form)))]
                               (collection-type ctx form options)
                               (fail))
        :s/map-of (if (and (nnext form) (spec-option-map (drop 3 form)))
                    {:t :map-of :k (p (second form)) :v (p (nth form 2))}
                    (fail))
        :s/tuple (with-meta (tuple (mapv p (rest form))) {::tuple-contract true})
        :s/fspec (if-let [{:keys [args ret]} (when-not (:inside-all ctx) (spec-option-map (rest form)))]
                   (let [r (if ret (p ret) unknown)]
                     {:t :fn :arities (spec-args->arities ctx args r) :ret r})
                   (fail))
        :t/fn (if (:inside-all ctx)
                (if (and (= 2 (count form)) (vector? (second form))) (p (second form)) (fail))
                (parse-sig ctx (second form)))
        (:fn* :fn) (or (predicate-spec-type ctx form) (fail))
        :partial (or (partial-instance-type ctx form) (fail))
        :t/class (if-let [c (resolve-class (:imports ctx) (second form))] (class-type c) (fail))
        (:t/pick :t/omit :t/partial :t/keys-of :t/get)
        (if (:operator-error ctx) (parse-operator ctx (first form) form) (fail))
        (fail))
      :else (fail))))
