(ns typespec.check
  "The checker: walks forms, infers types, reports diagnostics."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [typespec.parse :as parse]
            [typespec.types :as t]
            [typespec.literals :as literals]
            [typespec.type-operators :as operators]
            [typespec.solver :as solver]
            [typespec.env :as env]
            [typespec.java :as java]
            [typespec.core-types :as core]
            [typespec.guards :as guards]
            [typespec.macro-contracts :as macros]))

(def ^:dynamic *diags* nil)
(def ^:dynamic *types* nil)
(def ^:dynamic *ignores* nil)
(def ^:dynamic *trust* nil)
(def ^:dynamic *rets* nil)

(declare infer infer-body bind-pattern apply-fn-node why-of var-type infer-fn narrow record! force-callback)

(defn- report! [ctx node code msg & {:as extra}]
  (when-not (:quiet ctx)
    (swap! *diags* conj (merge {:code code :severity :error :file (:file (:file ctx)) :pos (:pos node) :message msg} extra)))
  nil)

(defn- eff
  "With :strict-nil, a platform type (from a hint or from Java) may be nil."
  [ctx ty]
  (if (and (:strict-nil (:opts ctx)) (:platform ty)) (t/union (dissoc ty :platform) t/nil-t) ty))

(defn- show-ref [ctx ty]
  (let [s (t/show ty)]
    (if (= :ref (:t ty))
      (let [r (t/resolve-t (:env ctx) ty)]
        (if (#{:map :merge :unknown} (:t r)) s (str s " (" (t/show r) ")")))
      s)))

;; ---- did you mean ----

(defn- edit-distance [^String a ^String b]
  (let [prev (int-array (range (inc (count b))))]
    (dotimes [i (count a)]
      (let [cur (int-array (inc (count b)))]
        (aset cur 0 (inc i))
        (dotimes [j (count b)]
          (aset cur (inc j) (int (min (inc (aget cur j)) (inc (aget prev (inc j)))
                                      (+ (aget prev j) (if (= (.charAt a i) (.charAt b j)) 0 1))))))
        (System/arraycopy cur 0 prev 0 (alength cur))))
    (aget prev (count b))))

(defn- similar [x candidates]
  (let [norm #(str/replace (str/lower-case (str %)) #"[-_]" "")
        sx (str x)]
    (->> candidates
         (remove #(= (str %) sx))
         (map (fn [c] [(if (= (norm c) (norm sx)) 0 (edit-distance sx (str c))) c]))
         (filter (fn [[d c]] (<= d (max 1 (min 2 (quot (count (name c)) 2))))))
         (sort-by first) first second)))

(defn- hint-suffix [x candidates]
  (if-let [c (similar x candidates)] (str ". Did you mean " c "?") ""))

;; ---- maps and keys ----

(defn- map-members [ctx ty]
  (filter #(= :map (:t %)) (t/members (:env ctx) ty)))

(defn- lookup-key [ctx ty k & [path?]]
  (let [env (:env ctx)
        ms (remove #(= :nil (:t %)) (t/members env ty))
        spec (when (and (qualified-keyword? k) (get-in env [:specs k])) (t/ref-t k))
        loose [:unknown (or spec t/unknown)]
        one (fn [m]
              (case (:t m)
                :map (cond (contains? (:req m) k) [:req (get (:req m) k)]
                           (contains? (:opt m) k) [:opt (get (:opt m) k)]
                           (:complete-keys? m) [:absent nil]
                           spec [:opt spec]
                           ;; a map whose type was inferred (a literal, a pattern) may hold more keys than we saw
                           (:inferred m) loose
                           :else [:missing nil])
                :map-of [:opt (:v m)]
                :coll (cond (and path? (= :set (:kind m))) [:opt (t/lit k)]
                            (and path? (nil? (:kind m))) loose
                            :else [:not-map nil])
                (:unknown :any :class :var :rigid :ivar) loose
                :prim (if (or (= :map (:name m)) (and path? (= :coll (:name m))))
                        loose
                        (if (and path? (= :set (:name m))) [:opt (t/lit k)] [:not-map nil]))
                [:not-map nil]))
        rs (map one ms)
        ty-of #(t/with-assumption (apply t/union (keep second rs)) (t/assumption ty))]
    (cond (= :never (:t ty)) {:status :req :type t/never}
          (empty? ms) (if path? {:status :absent} {:status :nil :type t/nil-t})
          (or (every? #(= :absent (first %)) rs)
              (and path? (every? #(#{:absent :not-map} (first %)) rs))) {:status :absent}
          (and (not path?) (some #(= :not-map (first %)) rs)) {:status :not-map}
          (some #(= :missing (first %)) rs) {:status :missing}
          (every? #(= :req (first %)) rs) {:status :req :type (ty-of)}
          (every? #(= :unknown (first %)) rs) {:status :unknown :type (ty-of)}
          :else {:status :opt :type (ty-of)})))

(defn- key-info
  "{:type :optional? :declared?} for reading key k from a value of type ty; reports unknown keys at node."
  [ctx ty k node & [path?]]
  (let [env (:env ctx)
        {:keys [status type]} (lookup-key ctx ty k path?)
        spec-keys (keys (:specs env))]
    (case status
      :req {:type type :optional? (t/nilable? env ty) :declared? true :status :req}
      :opt {:type type :optional? true :declared? true}
      :nil {:type t/nil-t :optional? true :declared? true}
      :absent {:type t/never :optional? true :declared? true}
      :unknown (do (when (and (qualified-keyword? k) (not (get-in env [:specs k]))
                              (contains? (:key-nss env) (namespace k))
                              (or (:strict (:opts ctx)) (similar k spec-keys)))
                     (report! ctx node "TSP2304" (str "no spec for " k (hint-suffix k spec-keys))))
                   ;; nothing is known about the map: nil is not checked, as for other platform types
                   {:type (if (#{:unknown :any} (:t type)) type (assoc type :platform true))
                    :optional? (boolean path?) :declared? false})
      :missing (do (report! ctx node "TSP2339"
                            (str k " is not a key of " (t/show (t/remove-nil env ty))
                                 (hint-suffix k (concat (mapcat #(concat (keys (:req %)) (keys (:opt %))) (map-members ctx ty))
                                                        (when (qualified-keyword? k) spec-keys)))))
                   {:type t/unknown :optional? false :declared? false})
      :not-map (do (report! ctx node "TSP2339" (str "cannot read " k " from " (t/show ty)))
                   {:type t/unknown :optional? false :declared? false}))))

(defn- read-key [ctx ty k node]
  (let [{:keys [type optional?]} (key-info ctx ty k node)]
    (if optional? (t/union type t/nil-t) type)))

(defn- read-key-default [ctx ty k node default-type]
  (let [{:keys [type optional?]} (key-info ctx ty k node)]
    ;; A default replaces absence, never a nil value stored at the key.
    (if optional?
      (if (every? #(= :nil (:t %)) (t/members (:env ctx) ty))
        default-type
        (t/union type default-type))
      type)))

;; ---- assignability ----

(defn- incomplete-problem [ctx actual expected result]
  ["TSP210-01"
   (str "compatibility not checked: " (show-ref ctx actual) " to " (show-ref ctx expected)
        " (" (str/join ", " (sort (map name (:reasons result)))) ")")
   {:severity (if (:strict (:opts ctx)) :error :warning)
    :reasons (:reasons result)}])

(defn- fresh-problems [ctx actual expected]
  (let [env (:env ctx)
        ms (remove #(= :nil (:t %)) (t/members env expected))]
    (when (and (:fresh actual) (= 1 (count ms)) (= :map (:t (first ms))))
      (let [m (first ms)
            known (concat (keys (:req m)) (keys (:opt m)))]
        (concat
         (for [k (keys (:req m)) :when (not (contains? (:req actual) k))]
           ["TSP2741" (str "required key " k " of " (t/show expected) " is missing")])
         (for [[k v] (:req actual)
               :let [exp (or (get (:req m) k) (get (:opt m) k))
                     result (when exp (t/sub-result env v exp))]
               :when (and exp (not= :proved (:status result)))]
           (if (= :inconclusive (:status result))
             (conj (incomplete-problem ctx v exp result) [:value k])
             ["TSP2322" (str "value of " k ": " (t/show v) " is not assignable to " (show-ref ctx exp)) nil [:value k]]))
         (for [k (keys (:req actual))
               :when (and (not (some #{k} known))
                          (not (and (qualified-keyword? k) (get-in env [:specs k])))
                          ;; a qualified key with no spec is already reported as TSP2304
                          (not (and (qualified-keyword? k) (contains? (:key-nss env) (namespace k))))
                          (or (qualified-keyword? k) (similar k known)))]
           ["TSP2353" (str "unknown key " k " in a literal of " (t/show expected) (hint-suffix k known)) nil [:key k]]))))))

(defn- missing-req
  "[key optional-there?] when `actual` is a map type that lacks a required key of `expected`."
  [ctx actual expected]
  (let [env (:env ctx)
        am (remove #(= :nil (:t %)) (t/members env actual))
        em (remove #(= :nil (:t %)) (t/members env expected))]
    (when (and (= 1 (count am) (count em)) (= :map (:t (first am)) (:t (first em))) (not (t/nilable? env actual)))
      (when-let [k (first (remove #(contains? (:req (first am)) %) (keys (:req (first em)))))]
        [k (contains? (:opt (first am)) k)]))))

(defn- problems
  "Why a value of type `actual` (written as `node`) cannot be used as `expected`. Empty when it can."
  [ctx node actual expected]
  (let [env (:env ctx) actual (eff ctx actual)
        result (t/sub-result env actual expected)]
    (cond
      (:generic-failure actual) []
      (= :proved (:status result)) (if (:fresh actual) (filter #(= "TSP2353" (first %)) (fresh-problems ctx actual expected)) [])
      (seq (fresh-problems ctx actual expected)) (fresh-problems ctx actual expected)
      (= :inconclusive (:status result)) [(incomplete-problem ctx actual expected result)]
      (and (not (#{:nil :map} (:op node))) (t/nilable? env actual) (not (t/nilable? env expected))
           (t/sub? env (t/remove-nil env actual) expected))
      [["TSP2531" (str (or (:display node) (parse/unparse node)) " may be nil"
                       (when-let [w (if (= :sym (:op node))
                                      (get-in ctx [:why (:val node)])
                                      (why-of ctx node actual))]
                         (str ": " w)))]]
      :else
      (if-let [[k opt?] (missing-req ctx actual expected)]
        [["TSP2741" (str "required key " k " is missing in " (t/show actual) (when opt? " (it is optional there)"))]]
        [["TSP2345" (str (show-ref ctx actual) " is not assignable to " (show-ref ctx expected))]]))))

(defn- assignable! [ctx node actual expected]
  (doseq [[code msg extra] (problems ctx node actual expected)]
    (apply report! ctx node code msg (mapcat identity extra))))

;; ---- locals and narrowing ----

(defn- bind [ctx sym ty & {:keys [why hinted param]}]
  (-> ctx
      (assoc-in [:locals sym] ty)
      (update :why #(if why (assoc % sym why) (dissoc % sym)))
      (update :hinted #(if hinted (conj (or % #{}) sym) (disj (or % #{}) sym)))
      (update :params #(if param (conj (or % #{}) sym) (disj (or % #{}) sym)))
      (assoc-in [:binding-ids sym] (Object.))
      (update :projections dissoc sym)
      (update :conditions dissoc sym)
      (update :contextual-params dissoc sym)
      (update :missing-of dissoc sym)))

(defn- plain-local [ctx node]
  (when (and (= :sym (:op node)) (empty? (:directives node))
             (contains? (:locals ctx) (:val node)))
    (:val node)))

(defn- builtin-head [ctx node]
  (let [head (env/head (:file ctx) node)
        raw (:val (first (:children node)))
        ns-sym (:ns (:file ctx))
        core-sym (when (and head (not (namespace head))) (symbol "clojure.core" (name head)))]
    (when (and head (empty? (:directives node)) (not (env/tag-node node))
               (not (contains? (:locals ctx) raw))
               (not (and core-sym (contains? (get-in ctx [:env :ns-defs ns-sym]) head)))
               (not= :extern (get-in ctx [:env :vars core-sym :source])))
      head)))

(defn- guard-symbol [ctx node]
  (when (and (= :sym (:op node)) (not (contains? (:locals ctx) (:val node))))
    (let [file (:file ctx)
          resolved (env/resolve-sym file (:val node))
          local (symbol (str (:ns file)) (name resolved))
          sym (cond (namespace resolved) resolved
                    (or (get-in ctx [:env :vars local])
                        (contains? (get-in ctx [:env :ns-defs (:ns file)]) resolved)) local
                    :else (symbol "clojure.core" (name resolved)))]
      (when-not (and (or (= "clojure.core" (namespace sym)) (= 'clojure.spec.alpha/valid? sym))
                     (= :extern (get-in ctx [:env :vars sym :source])))
        sym))))

(defn- guard-call [ctx node]
  (when (and (not (:guards-disabled? ctx))
             (= :list (:op node)) (= 2 (count (:children node)))
             (empty? (:directives node)) (not (env/tag-node node)))
    (get-in ctx [:env :guards (guard-symbol ctx (first (:children node)))])))

(defn- guard-compiler [ctx]
  {:resolve-head #(when (= :list (:op %)) (guard-symbol ctx (first (:children %))))
   :class-name #(when-let [name (t/resolve-class (:imports (:file ctx)) %)]
                  (when-let [class (t/load-class name)]
                    (when-not (.isPrimitive ^Class class) name)))
   :macro? #(and % (or (contains? (get-in ctx [:env :macros]) %)
                       (contains? (get-in ctx [:env :macro-contracts]) %)
                       (core/stdlib-macro? %)))
   :callable? #(contains? (get-in ctx [:env :vars]) %)})

(defn- remember-projection [ctx sym origin path]
  (assoc-in ctx [:projections sym]
            {:origin origin :path path :type (get-in ctx [:locals sym])}))

(defn- stable-path [ctx node]
  (when (and (empty? (:directives node)) (not (env/tag-node node)))
    (if-let [sym (plain-local ctx node)]
      (let [ty (get-in ctx [:locals sym])
            {:keys [origin path type]} (get-in ctx [:projections sym])]
        (if (and origin (= type ty))
          {:origin origin :path path :type (get-in ctx [:origins origin])}
          {:local sym :path [] :type ty}))
      (when (= :list (:op node))
        (let [[head a b & more] (:children node)
              h (builtin-head ctx node)
              [base path] (cond
                            (and (= :kw (:op head)) a (nil? b)) [a [(:val head)]]
                            (and (= 'get h) (= :kw (:op b)) (empty? more)) [a [(:val b)]]
                            (and (= 'get-in h) (= :vector (:op b)) (seq (:children b))
                                 (every? #(= :kw (:op %)) (:children b)) (empty? more))
                            [a (mapv :val (:children b))])]
          (when-let [p (and base (stable-path ctx base))]
            (update p :path into path)))))))

(defn- track-path [ctx {:keys [local origin path type]}]
  (if origin
    [ctx origin path]
    (let [origin (Object.)
          ctx (assoc-in ctx [:origins origin] type)]
      [(cond-> ctx local (remember-projection local origin [])) origin path])))

(defn- projected-type [ctx ty path]
  (reduce (fn [current key]
            (let [{:keys [type status]} (lookup-key ctx current key)]
              (case status
                :req (if (t/nilable? (:env ctx) current) (t/union type t/nil-t) type)
                :opt (t/union type t/nil-t)
                :nil t/nil-t
                (reduced nil))))
          ty path))

(defn- refine-origin [ctx {:keys [local origin]} ty]
  (when-not (= :never (:t ty))
    (if origin
      (reduce-kv (fn [c sym {:keys [path type] :as p}]
                   (if (and (= origin (:origin p)) (= type (get-in ctx [:locals sym])))
                     (if-let [narrowed (projected-type c ty path)]
                       (-> c (assoc-in [:locals sym] narrowed) (remember-projection sym origin path))
                       c)
                     c))
                 (assoc-in ctx [:origins origin] ty) (:projections ctx))
      (assoc-in ctx [:locals local] ty))))

(defn- path-alternatives
  "Pairs of [leaf type, rebuild]. Each rebuild retains its enclosing map alternatives."
  ([ctx ty path] (path-alternatives ctx ty path false))
  ([ctx ty path presence?]
   (let [budget (volatile! 256)
         env (:env ctx)]
     (letfn [(walk [ty path]
               (let [members (vec (take (inc @budget) (t/members env (eff ctx ty))))]
                 (when (and (seq members) (not (neg? (vswap! budget - (count members)))))
                   (if-let [[key & more] (seq path)]
                     (let [required (fn [m field]
                                      (when-let [children (walk field more)]
                                        (for [[leaf rebuild] children]
                                          [leaf #(assoc-in m [:req key] (rebuild %))])))
                           paths (for [m members]
                                   (cond
                                     (get-in m [:req key]) (required m (get-in m [:req key]))
                                     (and presence? (get-in m [:opt key]))
                                     (let [field (get-in m [:opt key])
                                           absent [t/nil-t (constantly (assoc-in m [:opt key] t/never))]
                                           present (required (update m :opt dissoc key) field)]
                                       (when present (cons absent present)))
                                     (and presence? (= :nil (:t m))) [[t/nil-t (constantly m)]]))]
                       (when (every? some? paths)
                         (mapcat identity paths)))
                     (map (fn [member] [member identity]) members)))))]
       (walk ty path)))))

(def ^:private primitive-predicates
  {'string? [:string string?] 'int? [:int int?] 'number? [:number number?]
   'keyword? [:keyword keyword?] 'map? [:map map?] 'vector? [:vector vector?]
   'boolean? [:boolean boolean?]})

(defn- truth-part [env ty truthy?]
  (if (and truthy?
           (not-any? #(or (= % (t/lit false)) (= % (t/prim :boolean))
                          (#{:unknown :any} (:t %))) (t/members env ty)))
    (t/remove-nil env ty)
    (apply t/union
         (for [m (t/members env ty)
               :let [falsy? (or (= :nil (:t m)) (= m (t/lit false)))
                     boolean? (= m (t/prim :boolean))]
               :when (or (#{:unknown :any} (:t m)) boolean? (= truthy? (not falsy?)))]
           (if boolean? (t/lit truthy?) m)))))

(defn- nil-part [env ty]
  (if (some #(#{:nil :unknown :any} (:t %)) (t/members env ty)) t/nil-t t/never))

(defn- refine-alternatives [ctx path alternatives select]
  (refine-origin ctx path
                 (apply t/union (keep (fn [[leaf rebuild]]
                                        (when-let [narrowed (select leaf)]
                                          (rebuild narrowed)))
                                      alternatives))))

(defn- literal-node? [node]
  (and (#{:str :kw :num :bool :nil} (:op node)) (empty? (:directives node))
       (not (env/tag-node node))))

(defn- literal-type [v]
  (if (nil? v) t/nil-t (t/lit v)))

(defn- finite-value? [ty]
  (#{:lit :nil} (:t ty)))

(defn- boolean-type? [ty]
  (= (t/prim :boolean) (t/strip ty)))

(defn- expand-boolean [ty]
  (if (boolean-type? ty) [(t/lit false) (t/lit true)] [ty]))

(defn- refine-path [ctx node split]
  (let [subject (stable-path ctx node)
        alternatives (when subject (path-alternatives ctx (:type subject) (:path subject) true))
        local (when-not alternatives (plain-local ctx node))
        subject (if local {:local local} subject)
        alternatives (or alternatives (when local (path-alternatives ctx (get-in ctx [:locals local]) [])))]
    (when alternatives
      (let [parts (map (fn [[leaf rebuild]] [(split leaf) rebuild]) alternatives)
            branch (fn [i]
                     (refine-alternatives ctx subject parts #(nth % i)))]
        [(branch 0) (branch 1)]))))

(defn- equality-parts [ctx value leaf]
  (let [literal (literal-type value)]
    (cond
      (finite-value? leaf) (if (= value (:val leaf)) [leaf nil] [nil leaf])
      (= :never (:t leaf)) [nil nil]
      (boolean-type? leaf)
      (if (boolean? value) [literal (t/lit (not value))] [nil leaf])
      (and (= :prim (:t leaf)) (not (t/sub? (:env ctx) literal leaf))) [nil leaf]
      :else [literal leaf])))

(defn- equality-contexts [ctx expr value]
  (refine-path ctx expr #(equality-parts ctx value %)))

(defn- presence-parts [key leaf]
  (case (:t leaf)
    :nil [nil leaf]
    :never [nil nil]
    :map (cond
           (contains? (:req leaf) key) [leaf nil]
           (contains? (:opt leaf) key)
           (let [field (get-in leaf [:opt key])
                 absent (assoc-in leaf [:opt key] t/never)]
             [(when-not (= :never (:t field))
                (-> leaf (assoc-in [:req key] field) (update :opt dissoc key)))
              absent])
           :else [leaf leaf])
    [leaf leaf]))

(def ^:private scalar-primitives #{:int :double :number :string :keyword :boolean :symbol})

(defn- predicate-parts [ctx head target leaf]
  (let [tag (:t leaf)
        env (:env ctx)
        pred (second (primitive-predicates head))]
    (cond
      (= :never tag) [nil nil]
      (#{:unknown :any} tag) [target leaf]
      ;; Specs compare numeric literals by value; int? and instance? can
      ;; distinguish representations of the same number.
      (and (= 'int? head)
           (or (and (= :lit tag) (integer? (:val leaf))) (= :int (:name leaf))))
      [leaf leaf]
      (and (finite-value? leaf) pred)
      (if (pred (:val leaf)) [leaf nil] [nil leaf])
      (= :nil tag) [nil leaf]
      (t/sub? env leaf target) [leaf nil]
      (t/sub? env target leaf) [target leaf]
      (and (= :prim tag) (= :prim (:t target))
           (or (scalar-primitives (:name leaf)) (scalar-primitives (:name target))))
      [nil leaf]
      :else [target leaf])))

(defn- instance-parts [ctx class-name leaf]
  (if (= "java.lang.Object" class-name)
    [(when-not (#{:nil :never} (:t leaf)) leaf)
     (when (#{:nil :unknown :any} (:t leaf)) t/nil-t)]
    (let [target (t/class-type class-name)
          [positive negative] (predicate-parts ctx 'instance? target leaf)]
      (if (#{:int :double} (:name target))
        [positive (when-not (= :never (:t leaf)) leaf)]
        [positive negative]))))

(defn- join-contexts [base paths]
  (let [paths (vec (remove nil? paths))]
    (case (count paths)
      0 nil
      1 (first paths)
      (let [locals (apply merge-with t/union (map :locals paths))]
        (reduce-kv (fn [c sym {:keys [origin path]}]
                     (if (every? #(= (get-in % [:locals sym]) (get-in % [:projections sym :type])) paths)
                       (remember-projection c sym origin path)
                       c))
                   (assoc base :locals locals :origins (apply merge-with t/union (map :origins paths)))
                   (:projections base))))))

(defn- logical-contexts [ctx args and?]
  (let [proceed (if and? first second)
        stop (if and? second first)
        [last-path stopped] (reduce (fn [[active stopped] arg]
                                      (if active
                                        (let [branches (narrow active arg)]
                                          [(proceed branches) (conj stopped (stop branches))])
                                        (reduced [nil stopped])))
                                    [ctx []] args)
        stopped (join-contexts ctx stopped)]
    (if and? [last-path stopped] [stopped last-path])))

(defn- condition-capture [ctx node]
  (let [head (builtin-head ctx node)]
    (when (or (primitive-predicates head)
              (guard-call ctx node)
              (#{'some? 'nil? 'not 'and 'or '= 'not= 'contains? 'instance? 'clojure.spec.alpha/valid?} head))
      {:test node
       :bindings (into {} (for [n (tree-seq (comp seq :children) :children node) :when (= :sym (:op n))]
                            [(:val n) (get-in ctx [:binding-ids (:val n)])]))})))

(defn- truth-contexts [ctx node]
  (refine-path ctx node (fn [leaf]
                         (for [truthy? [true false]
                               :let [ty (truth-part (:env ctx) leaf truthy?)]]
                           (when-not (= :never (:t ty)) ty)))))

(defn- narrow
  "Truth and falsity contexts. A nil context denotes an unreachable path."
  [ctx test]
  (let [env (:env ctx)
        h (builtin-head ctx test)
        [a b :as args] (vec (rest (:children test)))
        local (plain-local ctx test)
        {captured-test :test bindings :bindings} (get-in ctx [:conditions local])]
    (cond
      (and captured-test (every? (fn [[sym id]] (= id (get-in ctx [:binding-ids sym]))) bindings))
      (narrow ctx captured-test)

      (literal-node? test)
      (if (:val test) [ctx nil] [nil ctx])

      local
      (let [ty (eff ctx (get-in ctx [:locals local]))
            positive (truth-part env ty true)
            negative (truth-part env ty false)
            update-type (fn [ty] (when-not (= :never (:t ty)) (assoc-in ctx [:locals local] ty)))
            fc (update-type negative)]
        (or (when (seq (get-in ctx [:projections local :path])) (truth-contexts ctx test))
            [(update-type positive)
             (if (= t/nil-t negative)
               (reduce (fn [c s] (update-in c [:locals s] #(t/remove-nil env %))) fc (get-in ctx [:missing-of local]))
               fc)]))

      (#{'some? 'nil?} h)
      (or (when (= 1 (count args))
            (refine-path ctx a (fn [leaf]
                                 (let [nil? (= :nil (:t leaf))
                                       vague? (#{:unknown :any} (:t leaf))
                                       present (when-not (or nil? (= :never (:t leaf))) leaf)
                                       absent (when (or nil? vague?) t/nil-t)]
                                   (if (= 'some? h) [present absent] [absent present])))))
          [ctx ctx])

      (and (= 'not h) (= 1 (count args))) (vec (reverse (narrow ctx a)))
      (= 'and h) (logical-contexts ctx args true)
      (= 'or h) (logical-contexts ctx args false)

      (and (#{'= 'not=} h) (= 2 (count args)))
      (let [branches (or (cond (and (literal-node? a) (literal-node? b))
                               (if (= (:val a) (:val b)) [ctx nil] [nil ctx])
                               (literal-node? b) (equality-contexts ctx a (:val b))
                               (literal-node? a) (equality-contexts ctx b (:val a)))
                         [ctx ctx])]
        (if (= '= h) branches (vec (reverse branches))))

      (and (= 'contains? h) (= 2 (count args)) (= :kw (:op b)) (literal-node? b))
      (or (refine-path ctx a #(presence-parts (:val b) %)) [ctx ctx])

      (and (= 'instance? h) (= 2 (count args)) (= :sym (:op a)))
      (or (when-let [c (t/resolve-class (:imports (:file ctx)) (:val a))]
            (refine-path ctx b #(instance-parts ctx c %)))
          [ctx ctx])

      (and (= 'clojure.spec.alpha/valid? h) (= 2 (count args)) (= :kw (:op a)) (plain-local ctx b))
      [(assoc-in ctx [:locals (plain-local ctx b)] (t/ref-t (:val a))) ctx]

      (and (primitive-predicates h) (= 1 (count args)))
      (let [target (t/prim (first (primitive-predicates h)))]
        (or (refine-path ctx a #(predicate-parts ctx h target %)) [ctx ctx]))

      :else (or (when-let [summary (guard-call ctx test)]
                  (refine-path ctx a #(mapv (fn [ty] (when-not (= t/never ty) ty))
                                            (guards/apply-summary env summary %))))
                (truth-contexts ctx test) [ctx ctx]))))

(defn- condition-contexts [ctx test]
  (let [ty (eff ctx (infer ctx test))
        [tc fc] (narrow ctx test)]
    [(when-not (= :never (:t (truth-part (:env ctx) ty true))) tc)
     (when-not (= :never (:t (truth-part (:env ctx) ty false))) fc)]))

;; ---- destructuring ----

(defn- keys-directive [k]
  (when (and (= :kw (:op k)) (#{"keys" "keys!"} (name (:val k))))
    {:ns (namespace (:val k)) :bang? (= "keys!" (name (:val k)))}))

(defn- has-bang? [pattern]
  (and (= :map (:op pattern))
       (some (fn [[k _]] (or (:bang? (keys-directive k)) (has-bang? k))) (partition 2 (:children pattern)))))

(defn- destructure-map [ctx pattern ty outer-why projection]
  (let [env (:env ctx)
        pairs (partition 2 (:children pattern))
        special (into {} (for [[k v] pairs :when (and (= :kw (:op k)) (nil? (namespace (:val k))))] [(:val k) v]))
        defaults (into {} (for [[k v] (partition 2 (:children (:or special)))] [(:val k) v]))
        missing-sym (some-> (:missing special) :val)
        tname (t/show (t/remove-nil env ty))
        ;; Destructuring evaluates its input once. Its whole value and plain
        ;; field bindings share this identity, even without :as.
        [ctx origin path] (track-path ctx (or projection {:path [] :type ty}))
        ctx (cond-> ctx missing-sym (bind missing-sym (t/union (t/prim :map) t/nil-t)))
        ctx (reduce
             (fn [ctx [k v]]
               (let [kd (keys-directive k)]
                 (cond
                   (and kd (= :vector (:op v)))
                   (reduce
                    (fn [ctx s]
                      (if-not (#{:sym :kw} (:op s))
                        ctx
                        (let [key (keyword (or (:ns kd) (namespace (:val s))) (name (:val s)))
                              sym (symbol (name (:val s)))
                              {:keys [type optional? declared? status]} (key-info ctx ty key s)
                              default (get defaults sym)]
                          (cond
                            (and (:bang? kd) missing-sym)
                            (-> (bind ctx sym (t/union type t/nil-t) :why (str key " is missing when " missing-sym " is not nil"))
                                (update-in [:missing-of missing-sym] (fnil conj []) sym))
                            (:bang? kd)
                            (do (when (and optional? declared?)
                                  (report! ctx s "TSP2741" (str ":keys! requires " key ", but it is optional in " tname)))
                                (bind ctx sym type))
                            default
                            (bind ctx sym (if optional? (t/union type (infer ctx default)) type)
                                  :why (when (t/nilable? env type)
                                         (str key " is " (t/show (t/resolve-t env type))
                                              ", and :or applies only when the key is absent")))
                            optional?
                            (-> (bind ctx sym (t/union type t/nil-t)
                                      :why (cond (= :req status) (or outer-why "the destructured value may be nil")
                                                 declared? (str key " is optional in " tname)
                                                 :else (str key " may be absent")))
                                (remember-projection sym origin (conj path key)))
                            :else (-> (bind ctx sym type)
                                      (remember-projection sym origin (conj path key)))))))
                    ctx (:children v))

                   (and (= :kw (:op k)) (= :as (:val k)))
                   (-> (bind ctx (:val v) ty)
                       (remember-projection (:val v) origin path))
                   (and (= :kw (:op k)) (= :missing (:val k))) ctx
                   (and (= :kw (:op k)) (#{:select :excess :all} (:val k))) (bind ctx (:val v) t/unknown)
                   (and (= :kw (:op k)) (#{:strs :syms :strs! :syms!} (:val k)))
                   (reduce #(bind %1 (:val %2) t/unknown) ctx (filter #(= :sym (:op %)) (:children v)))

                   (= :kw (:op v))
                   (let [{:keys [type optional? declared?]} (key-info ctx ty (:val v) v)
                         bang? (has-bang? k)]
                     (when (and bang? declared? (or optional? (t/nilable? env type)))
                       (report! ctx v "TSP2741" (str ":keys! below requires " (:val v) ", but it is optional in " tname)))
                     (bind-pattern ctx k (cond bang? (t/remove-nil env type)
                                               optional? (t/union type t/nil-t)
                                               :else type)
                                   (when (and optional? declared?) (str (:val v) " is optional in " tname))
                                   (when (and (not bang?) (not (contains? defaults (:val k))))
                                     {:origin origin :path (conj path (:val v))})))
                   :else ctx)))
             ctx pairs)]
    ctx))

(defn- elem-type [ctx ty]
  (t/with-assumption
   (apply t/union
         (for [m (t/members (:env ctx) ty)]
           (case (:t m)
             :coll (:of m)
             :nil t/never
             :map-of (t/prim :vector)
             (:prim :lit) (if (or (= :string (:name m)) (string? (:val m)))
                            (t/class-type "java.lang.Character") t/unknown)
             t/unknown)))
   (t/assumption ty)))

(defn- indexed
  "Element access is not checked for emptiness (as in TypeScript by default); :strict-nil makes it T | nil."
  [el]
  (if (#{:unknown :any :never :nil} (:t el)) el (assoc el :platform true)))

(defn- check-hint!
  "A hint that cannot be true for the value it is put on."
  [ctx node actual what]
  (when-let [h (env/hint-type (:file ctx) node)]
    (let [env (:env ctx) a (t/remove-nil env (eff ctx actual))]
      (when (and (not (#{:never :unknown :any} (:t (t/resolve-t env a))))
                 (not (t/sub? env a h)) (not (t/sub? env h a)))
        (report! ctx (env/tag-node node) "TSP2322"
                 (str "hint " (t/show h) " contradicts the " what ": " (t/show (dissoc actual :platform))))))
    h))

(defn bind-pattern
  ([ctx pattern ty] (bind-pattern ctx pattern ty nil))
  ([ctx pattern ty why] (bind-pattern ctx pattern ty why nil))
  ([ctx pattern ty why projection]
   (case (:op pattern)
     :sym (let [h (check-hint! ctx pattern ty "type")
                env (:env ctx)
                ;; the compiler believes the hint; keep the inferred type only when it agrees and says more
                use-hint? (and h (or (#{:unknown :any} (:t (t/resolve-t env ty)))
                                     (not (t/sub? env (t/remove-nil env (eff ctx ty)) h))))
                [c origin path] (if (and projection (not use-hint?)) (track-path ctx projection) [ctx])]
            (cond-> (bind c (:val pattern) (if use-hint? h ty) :why (when-not use-hint? why) :hinted (boolean h))
              origin (remember-projection (:val pattern) origin path)))
     :map (destructure-map ctx pattern ty why projection)
     :vector (let [el (elem-type ctx ty)]
               (loop [ctx ctx [p & more] (:children pattern)]
                 (cond (nil? p) ctx
                       (and (= :sym (:op p)) (= '& (:val p))) (bind-pattern ctx (first more) {:t :coll :of el})
                       (and (= :kw (:op p)) (= :as (:val p))) (bind-pattern ctx (first more) ty)
                       :else (recur (bind-pattern ctx p (if (#{:unknown :any :never :nil} (:t el)) el (assoc el :platform true))) more))))
     ctx)))

;; ---- function calls ----

(defn- arity-fits? [a n]
  (let [k (count (:params a))]
    (or (= n k) (and (> n k) (or (:rest a) (:kwargs a))))))

(defn- matching-arities [ftype n]
  (let [arities (:arities ftype)]
    (if (::t/dispatch (meta ftype))
      (when-let [arity (t/dispatch-arity arities n)] [arity])
      (filter #(arity-fits? % n) arities))))

(defn- keyword-parameter [m k]
  (some (fn [[declared ty]] (when (= (name declared) (name k)) ty))
        (concat (:req m) (:opt m))))

(defn- arg-problems [ctx a arg-nodes arg-types call-node]
  (let [check-arg (fn [n at pt]
                    (when at
                      (if-let [[expected actual] (::callback-arity-error (meta at))]
                        [[n ["TSP2554" (str "callback expects " expected " parameter(s) in this call, got " actual)]]]
                        (map #(vector n %) (problems ctx n at pt)))))
        k (count (:params a))
        fixed (mapcat check-arg
                      arg-nodes arg-types (:params a))
        extra (drop k (map vector arg-nodes arg-types))]
    (concat
     fixed
     (when (:rest a) (mapcat (fn [[n at]] (check-arg n at (:rest a))) extra))
     (when-let [kw (:kwargs a)]
       (let [given (into {} (for [[[kn _] [vn vt]] (partition 2 extra) :when (= :kw (:op kn))] [(:val kn) [vn vt]]))
             m (first (map-members ctx kw))]
         (concat
          (for [rk (keys (:req m)) :when (not (contains? given (keyword (name rk))))]
            [call-node ["TSP2741" (str "required key " (keyword (name rk)) " is missing")]])
          (for [[gk [vn vt]] given
                :let [exp (keyword-parameter m gk)]
                :when (and exp vt)
                p (problems ctx vn vt exp)]
            [vn p])))))))

(defn- unresolved-type?
  "Contains unknown/any or an unsolved variable. A rigid type parameter is a
  known type: its (possibly unknown) bound is not part of the value's type."
  [ty]
  (boolean (some #(and (map? %) (#{:ivar :var :unknown :any :all} (:t %)))
                 (tree-seq #(and (coll? %) (not (and (map? %) (= :rigid (:t %))))) seq ty))))

(defn- trusted-substitution
  "Candidates that may be used as types: not the ones the solver marks uncertain
  (an unknown lower bound or evidence it could not decompose)."
  [solved]
  (apply dissoc (:substitution solved) (:uncertain solved)))

(defn- generic-result [ctx node label state]
  (let [{:keys [status issues] :as solved} (solver/solve (:env ctx) state)
        issue (or (first (filter #(= :mismatch (:status %)) issues)) (first issues))
        ret (solver/substitute (:ret (first (:arities (:type state)))) (trusted-substitution solved))
        ;; Only the solver snapshot keeps unsolved ivars. Do not leak them, or an
        ;; incomplete candidate, into ordinary inference after the diagnostic.
        ret (solver/substitute ret (into {} (for [v (:vars state)] [(:id v) t/unknown])))]
    ;; A solve snapshot is pure. Only this final solve emits, once per call.
    (when issue
      (if (= :mismatch status)
        (report! ctx node "TSP2345"
                 (str label ": " (t/show (:left issue)) " is not assignable to " (t/show (:right issue))))
        (report! ctx node "TSP210-01"
                 (str label ": generic call not checked ("
                      (str/join ", " (sort (map name (into #{} (mapcat :reasons issues))))) ")")
                 :severity (if (:strict (:opts ctx)) :error :warning)
                 :reasons (into #{} (mapcat :reasons issues)))))
    ;; The flag silences follow-up checks of a result that means nothing: after
    ;; a mismatch, or when the result itself is not known. A fully known result
    ;; of a merely unchecked call is still checked where it is used.
    (cond-> (or ret t/unknown)
      (or (= :mismatch status) (and (not= :proved status) (unresolved-type? ret)))
      (assoc :generic-failure true))))

(defn- ivar-ids [ty]
  (into #{} (keep #(when (and (map? %) (= :ivar (:t %))) (:id %))) (tree-seq coll? seq ty)))

(defn- callback-substitution
  "The partial solution as callbacks may see it. A variable that some callback
  returns can still grow from that callback's result, so its seed is only a
  start: a literal seed 0 means int?, and an empty collection says nothing."
  [partial callback-types]
  (let [growing (into #{} (mapcat #(ivar-ids (:ret (first (:arities %)))) callback-types))
        widen-all (fn [ty] (walk/postwalk
                            (fn [x] (cond (and (map? x) (= :lit (:t x))) (t/lit-prim (:val x))
                                          (and (map? x) (= :union (:t x))) (apply t/union (:types x))
                                          :else x)) ty))
        empty-seed? (fn [ty] (some #(and (map? %) (= :never (:t %))) (tree-seq coll? seq ty)))]
    (reduce (fn [sub id]
              (cond (not (contains? sub id)) sub
                    (empty-seed? (get sub id)) (dissoc sub id)
                    :else (update sub id widen-all)))
            partial growing)))

(defn- callback-context
  "Parameter types a callback may assume, or nil when they are not fully known."
  [expected partial]
  (let [context (solver/substitute expected partial)
        ca (first (:arities context))]
    (when (and (= :fn (:t context)) (= 1 (count (:arities context)))
               (not (:rest ca)) (not (:kwargs ca))
               (not-any? unresolved-type? (:params ca)))
      (:params ca))))

(defn- fn-lit-parameters [node]
  (let [syms (keep #(when (and (= :sym (:op %)) (simple-symbol? (:val %))) (name (:val %)))
                   (tree-seq :children :children node))
        n (reduce max 0 (keep #(cond (= "%" %) 1
                                    (re-matches #"%[1-9][0-9]*" %) (or (parse-long (subs % 1)) 21)) syms))]
    (when (<= n 20)
      {:fixed (mapv #(hash-map :op :sym :val (symbol (str "%" %))) (range 1 (inc n)))
       :rest (when (some #{"%&"} syms) {:op :sym :val '%&})})))

(defn- inline-callback? [ctx node]
  (or (= :fn-lit (:op node))
      (#{'fn 'fn*} (builtin-head ctx (dissoc node :directives :meta)))))

(defn- callback-signature [ctx expected]
  (let [members (remove #(= :nil (:t %)) (t/members (:env ctx) expected))
        ty (first members)]
    (when (and (= 1 (count members)) (= :fn (:t ty)) (seq (:arities ty))
               (not-any? :kwargs (:arities ty)))
      ty)))

(defn- deferred-callback [ctx node]
  (when-let [sym (plain-local ctx node)]
    (::deferred-callback (meta (get-in ctx [:locals sym])))))

(defn- callback-node? [ctx node]
  (or (inline-callback? ctx node) (deferred-callback ctx node)))

(defn- argument-type [ctx ar args i]
  (let [fixed (count (:params ar))]
    (cond
      (< i fixed) (nth (:params ar) i)
      (:rest ar) (:rest ar)
      (and (:kwargs ar) (odd? (- i fixed)))
      (let [key (nth args (dec i))]
        (when (= :kw (:op key))
          (keyword-parameter (first (map-members ctx (:kwargs ar))) (:val key)))))))

(defn- common-callback-signature [ctx fits args i]
  (let [signatures (mapv #(callback-signature ctx (argument-type ctx % args i)) fits)
        inputs (fn [signature] (mapv #(select-keys % [:params :rest]) (:arities signature)))]
    (when (and (seq signatures) (every? some? signatures)
               (apply = (map inputs signatures)))
      {:t :fn :arities (apply mapv (fn [& ars] (assoc (first ars) :ret (apply t/union (map :ret ars))))
                               (map :arities signatures))})))

(defn- infer-call-arguments [ctx call-node arg-nodes fits]
  (let [pending? (fn [i arg]
                   (and (callback-node? ctx arg)
                        (some #(callback-signature ctx (argument-type ctx % arg-nodes i)) fits)))
        types (mapv (fn [i arg] (when-not (pending? i arg) (infer ctx arg))) (range) arg-nodes)
        candidates (when (some nil? types)
                     (let [possible (filter #(every? (fn [[_ [code]]] (= "TSP210-01" code))
                                                    (arg-problems ctx % arg-nodes types call-node)) fits)]
                       (or (seq possible) (when (= 1 (count fits)) fits))))]
    (mapv (fn [i arg ty]
            (or ty (if-let [expected (common-callback-signature ctx candidates arg-nodes i)]
                     (infer ctx arg expected)
                     (infer ctx arg))))
          (range) arg-nodes types)))

(defn- call-generic
  "Rank-1, single fixed arity: infer ordinary arguments before contextual callbacks.
  Typed arguments are used by apply-type; node arguments are visited at most once."
  [ctx node label ftype arg-nodes typed-args]
  (let [state (solver/instantiate ftype)
        ars (:arities (:type state)) ar (first ars)
        n (count (or typed-args arg-nodes))
        inline? #(callback-node? ctx %)]
    (cond
      (or (not= 1 (count ars)) (:rest ar) (:kwargs ar))
      (do (when-not typed-args (run! #(infer ctx %) arg-nodes))
          (report! ctx node "TSP210-01" "generic overloads and variadic contracts are unsupported"
                   :severity (if (:strict (:opts ctx)) :error :warning))
          (assoc t/unknown :generic-failure true))
      (not= n (count (:params ar)))
      (do (when-not typed-args (run! #(infer ctx %) arg-nodes))
          (report! ctx node "TSP2554" (str label " expects " (count (:params ar)) " argument(s), got " n))
          (assoc t/unknown :generic-failure true))
      :else
      (let [entries (mapv (fn [i pt] {:i i :expected pt :node (nth arg-nodes i nil)})
                          (range n) (:params ar))
            callbacks (if typed-args [] (filter #(inline? (:node %)) entries))
            ordinary (if typed-args entries (remove #(inline? (:node %)) entries))
            state (reduce (fn [st {:keys [i expected node]}]
                            (solver/constrain st (eff ctx (if typed-args (nth typed-args i) (infer ctx node)))
                                              expected {:argument i})) state ordinary)
            partial (callback-substitution (trusted-substitution (solver/solve (:env ctx) state))
                                           (map :expected callbacks))
            state (reduce
                   (fn [st {:keys [i expected node]}]
                     (let [params (callback-context expected partial)
                           actual (if params
                                    (infer ctx node {:t :fn :arities [{:params params :ret t/unknown}]})
                                    (do (infer ctx node) t/unknown))]
                       ;; Keep the original expected return ivar: the callback's result solves it.
                       (solver/constrain st actual expected {:argument i :callback true})))
                   state callbacks)]
        (generic-result ctx node label state)))))

(defn- call-fn [ctx node fname ftype arg-nodes]
  (if (= :all (:t ftype))
    (call-generic ctx node fname ftype arg-nodes nil)
    (let [n (count arg-nodes)
        ars (:arities ftype)
          fits (matching-arities ftype n)
        arg-types (infer-call-arguments ctx node arg-nodes fits)]
    (cond
      (empty? ars) (or (:ret ftype) t/unknown)
      (empty? fits) (do (report! ctx node "TSP2554"
                                 (str fname " expects " (str/join " or " (distinct (map #(str (count (:params %)) (when (or (:rest %) (:kwargs %)) "+")) ars)))
                                      " argument(s), got " n))
                        (apply t/union (map :ret ars)))
      :else (let [all (map #(arg-problems ctx % arg-nodes arg-types node) fits)
                  passing (keep-indexed (fn [i ps] (when (empty? ps) i)) all)
                  vague? (some #(#{:unknown :any} (:t (t/resolve-t (:env ctx) %))) arg-types)
                  ;; Incomplete evidence is not a mismatch. Prefer a candidate
                  ;; whose known arguments fit, but still emit its uncertainty.
                  incomplete (keep-indexed
                               (fn [i ps]
                                 (when (and (seq ps)
                                            (every? #(= "TSP210-01" (first (second %))) ps)) i)) all)
                  ;; overloads go from narrow to general: with an unknown argument only the general one is safe
                  ;; nothing fits: if the only trouble is a possible nil, keep the overload that fits otherwise
                  non-nil (mapv #(t/remove-nil (:env ctx) (eff ctx %)) arg-types)
                  non-nil-problems (map #(arg-problems (assoc ctx :quiet true) % arg-nodes non-nil node) fits)
                  nil-only (first (keep-indexed (fn [i ps] (when (empty? ps) i)) non-nil-problems))
                  ;; Return evidence must include every candidate that could fit
                  ;; after removing nil, even if another argument is still unknown.
                  nil-compatible (keep-indexed
                                   (fn [i ps]
                                     (when (every? #(= "TSP210-01" (first (second %))) ps) i))
                                   non-nil-problems)
                  unchecked? (and (empty? passing)
                                  (some #(= "TSP210-01" (first (second %))) (mapcat identity all)))
                  possible (distinct (concat incomplete nil-compatible))
                  returns (distinct (map :ret (if (seq possible) (map #(nth fits %) possible) fits)))
                  i (or (if vague? (last passing) (first passing)) (first incomplete) nil-only (dec (count all)))]
              (doseq [[pn [code msg extra]] (nth all i)]
                (apply report! ctx pn code msg (mapcat identity extra)))
              ;; A merely incomplete candidate must not hide a possible nil
              ;; boundary on another viable overload. Keep these diagnostics
              ;; separate from both overload selection and return evidence.
              (when unchecked?
                (doseq [[pn [code msg extra]]
                        (distinct (for [j nil-compatible
                                        problem (nth all j)
                                        :when (and (= "TSP2531" (first (second problem)))
                                                   (not (some #{problem} (nth all i))))]
                                    problem))]
                  (apply report! ctx pn code msg (mapcat identity extra))))
              (cond
                unchecked? (if (next returns) t/unknown (first returns))
                (and vague? (next (distinct (map #(:ret (nth fits %)) passing)))) t/unknown
                :else (:ret (nth fits i))))))))

;; ---- inference ----

(defn- args-of [node] (vec (rest (:children node))))

(defn- record! [ctx node ty]
  (when (and *types* (:pos node) (not (:synthetic node)))
    (let [parameter (when (= :sym (:op node)) (get-in ctx [:contextual-params (:val node)]))]
      (swap! *types* conj (cond-> {:file (:file (:file ctx)) :pos (:pos node) :type ty :ns (:ns (:file ctx))}
                           parameter (assoc :context parameter)))))
  ty)

(defn- known-ns? [ctx ns-sym]
  (or (contains? (:nss (:env ctx)) ns-sym) (some? (core/stdlib-publics ns-sym))))

(defn- resolve-var
  "Resolve a Var name without reading its root."
  [ctx sym node]
  (let [file (:file ctx) env (:env ctx)
        r (env/resolve-sym file sym)]
    (if-let [n (some-> (namespace r) symbol)]
      (let [ty (or (var-type ctx r) (get core/table r))
            names (or (core/stdlib-publics n) (get-in env [:ns-defs n]))]
        (when (and names (not ty) (not (contains? names (symbol (name r)))))
          (report! ctx node "TSP2305" (str n " has no var " (name r) (hint-suffix (symbol (name r)) names))))
        {:sym r :type ty :known-ns? (boolean names)})
      (let [local (symbol (str (:ns file)) (name r))]
        (if (or (contains? (:var-declarations env) local) (get-in env [:vars local]) (contains? (:macros env) local))
          {:sym local :type (var-type ctx local)}
          (let [c (symbol "clojure.core" (name r))]
            {:sym c :type (or (var-type ctx c) (get core/table c))}))))))

(defn- root-initialized? [ctx sym initializers at]
  (or (contains? (:initialized-vars ctx) sym)
      (if (and (:load-time? ctx) (= (namespace sym) (str (get-in ctx [:file :ns]))))
        (some (fn [{:keys [file pos]}]
                (let [end ((juxt :end-row :end-col) pos)
                      start ((juxt :row :col) (:pos at))]
                  (and (= file (get-in ctx [:file :file]))
                       (every? number? (concat end start))
                       (not (pos? (compare end start))))))
              initializers)
        (seq initializers))))

(defn- read-var [ctx sym node]
  (let [{full :sym :as resolved} (resolve-var ctx sym node)
        {:keys [tracks-root? initializers]} (get-in ctx [:env :var-declarations full])]
    (if (and tracks-root? (not (root-initialized? ctx full initializers node)))
      (do (when-not (contains? (:possible-bindings ctx) sym)
            (report! ctx node "TSP721-01" (str "Var " full " may be unbound at this read")
                     :severity (if (:strict (:opts ctx)) :error :warning)
                     :reasons #{:possibly-unbound-var}))
          (assoc resolved :type t/unknown))
      resolved)))

(defn- java-static?
  "Class name when `Foo/bar` refers to a Java class and not to a Clojure namespace."
  [ctx sym]
  (when-let [n (some-> (namespace sym) symbol)]
    (when-not (or (contains? (:aliases (:file ctx)) n) (known-ns? ctx n))
      (t/resolve-class (:imports (:file ctx)) n))))

(defn- probe [ctx f]
  (binding [*types* nil *diags* (atom []) *trust* (atom {:cast 0 :any 0 :ignore 0})
            *ignores* (atom []) *rets* (atom (when *rets* @*rets*))]
    (f (assoc ctx :quiet true))))

(defn- quietly [ctx node]
  (probe ctx #(infer % node)))

(defn- widen [ty]
  (case (:t ty)
    :lit (t/with-assumption (t/lit-prim (:val ty)) (t/assumption ty))
    :union (apply t/union (map widen (:types ty)))
    ty))

(defn var-type
  "Type of a var. A function with no declared return type gets it inferred from its body, once."
  [ctx sym]
  (when-let [v (get-in (:env ctx) [:vars sym])]
    (cond
      (not (and (= :hints (:source v)) (some #(= :unknown (:t (:ret %))) (:arities (:type v))))) (:type v)
      (contains? @*rets* sym) (get @*rets* sym)
      :else
      (do (swap! *rets* assoc sym (:type v)) ; a recursive call sees the declared part only
          (let [fctx {:env (:env ctx) :file (:pfile v) :opts (:opts ctx) :locals {}}
                ars (mapv (fn [a {:keys [params body]}]
                            (if (not= :unknown (:t (:ret a)))
                              a
                              (let [{:keys [fixed rest]} (env/split-params params)
                                    c (reduce (fn [c [p ty]] (bind-pattern c p ty)) (assoc fctx :quiet true)
                                              (map vector fixed (:params a)))
                                    c (if rest (bind-pattern c rest (or (:kwargs a) t/unknown)) c)
                                    ret (binding [*types* nil *trust* (atom {:cast 0 :any 0 :ignore 0}) *ignores* (atom [])]
                                          (infer-body c body))]
                                (assoc a :ret (widen (dissoc ret :fresh)) :inferred true))))
                          (:arities (:type v)) (:arities (:parts v)))
                ty (assoc (:type v) :arities ars)]
            (swap! *rets* assoc sym ty)
            ty)))))

(defn- why-of [ctx init ty]
  (cond
    (= :sym (:op init)) (get-in ctx [:why (:val init)])
    (and (= :list (:op init)) (= :kw (:op (first (:children init)))) (second (:children init))
         (t/nilable? (:env ctx) ty))
    (let [k (:val (first (:children init)))
          mty (quietly ctx (second (:children init)))]
      (when (= :opt (:status (lookup-key ctx mty k)))
        (str k " is optional in " (t/show (t/remove-nil (:env ctx) mty)))))
    (and (= :list (:op init)) (= :sym (:op (first (:children init)))) (t/nilable? (:env ctx) ty))
    (str (parse/unparse (first (:children init))) " returns " (t/show ty))))

(def ^:private compiler-tagged-fns '#{str subs name format})

(defn- compiler-knows?
  "Does the Clojure compiler know the class of this expression (so the call is not reflective)?"
  [ctx node]
  (boolean
   (or (env/tag-node node)
       (#{:str :num :bool} (:op node))
       (and (= :sym (:op node)) (contains? (:hinted ctx) (:val node)))
       (when (= :list (:op node))
         (let [h (first (:children node))]
           (when (= :sym (:op h))
             (let [n (name (:val h))]
               (or (and (nil? (namespace (:val h)))
                        (or (str/starts-with? n ".") (str/ends-with? n ".") (= "new" n)
                            (compiler-tagged-fns (:val h))))
                   (java-static? ctx (:val h))
                   (and (not (contains? (:locals ctx) (:val h)))
                        (let [full (:sym (resolve-var (assoc ctx :quiet true) (:val h) h))]
                          (or (core/stdlib-tagged? full)
                              (get-in (:env ctx) [:vars full :tagged?]))))))))))))

(defn- bind-pairs [ctx bindings]
  (reduce (fn [ctx [p init]]
            (let [sym (when (= :sym (:op p)) (:val p))
                  pending (:pending-callbacks ctx)
                  defer? (and sym pending (not (env/tag-node p)) (not (env/tag-node init))
                              (empty? (:directives init)))
                  callback (when defer?
                             (or (deferred-callback ctx init)
                                 (when (inline-callback? ctx init)
                                   (let [callback {:ctx (dissoc ctx :pending-callbacks)
                                                   :node init :type (atom nil)}]
                                     (vswap! pending conj callback)
                                     callback))))
                  ty (if callback
                       (with-meta {:t :fn} {::deferred-callback callback})
                       (dissoc (infer ctx init) :fresh))
                  why (why-of ctx init ty)
                  captured (when (and sym (not (env/tag-node p))) (condition-capture ctx init))
                  ctx' (bind-pattern ctx p ty why (stable-path ctx init))]
              (when-not callback (record! ctx p (get-in ctx' [:locals (:val p)] ty)))
              (cond-> ctx'
                captured (assoc-in [:conditions sym] captured)
                (and sym (compiler-knows? ctx init)) (update :hinted (fnil conj #{}) sym))))
          ctx (partition 2 (:children bindings))))

(defn- bind-seq-pairs [ctx bindings]
  (reduce (fn [ctx [p init]]
            (if (= :kw (:op p))
              (case (:val p)
                :let (bind-pairs ctx init)
                (:when :while) (or (first (condition-contexts ctx init)) ctx)
                ctx)
              (bind-pattern ctx p (elem-type ctx (infer ctx init)))))
          ctx (partition 2 (:children bindings))))

(defn- body-initializers [ctx form]
  (case (builtin-head ctx form)
    (def defonce) (when-let [name (env/initialized-var (:file ctx) form)] [name])
    do (mapcat #(body-initializers ctx %) (args-of form))
    time (let [[expr & more] (args-of form)]
           (when (and expr (empty? more)) (body-initializers ctx expr)))
    nil))

(defn infer-body [ctx forms]
  (loop [[form & more] forms, ctx ctx, result t/nil-t]
    (if form
      (let [result (infer ctx form)
            initialized (seq (body-initializers ctx form))]
        (recur more (cond-> ctx initialized (update :initialized-vars (fnil into #{}) initialized)) result))
      result)))

(defn- fn-bodies [node]
  (if (= :fn-lit (:op node))
    (when-let [params (fn-lit-parameters node)]
      [(assoc params :body [{:op :list :children (:children node) :pos (:pos node) :synthetic true}]
                     :reader? true)])
    (let [more (rest (:children node))
          more (if (= :sym (:op (first more))) (rest more) more)
          bodies (if (= :vector (:op (first more))) [more]
                     (for [l more :when (= :list (:op l))] (:children l)))]
      (mapv (fn [[params & body]] (assoc (env/split-params params) :body body)) bodies))))

(defn- infer-fn-branch [ctx {:keys [fixed rest body reader?]} contracts]
  (let [env (:env ctx)
        file (:file ctx)
        kwargs (when (= :map (:op rest)) (env/pattern-type env file rest))
        ptypes (mapv (fn [i p]
                       (if (seq contracts)
                         (apply t/union (map #(nth (:params %) i) contracts))
                         (env/pattern-type env file p)))
                     (range) fixed)
        c (reduce (fn [c [i p ty]]
                    (let [bound (cond-> (bind-pattern c p ty)
                                  (and (seq contracts) (= :sym (:op p)))
                                  (assoc-in [:contextual-params (:val p)] {:source :callback :parameter (inc i) :type ty}))]
                      (when (seq contracts)
                        (record! bound p (get-in bound [:locals (:val p)] ty)))
                      bound))
                  ctx (map vector (range) fixed ptypes))
        c (cond-> c (and reader? (seq ptypes)) (bind '% (first ptypes)))
        rest-of (when (and rest (not kwargs))
                  (if (seq contracts)
                    (apply t/union (mapcat #(concat (drop (count fixed) (:params %))
                                                   (when-let [r (:rest %)] [r])) contracts))
                    t/unknown))
        tail {:t :coll :kind :seq :of rest-of}
        c (if rest
            (bind-pattern c rest (cond kwargs kwargs
                                       (not (seq contracts)) t/unknown
                                       (= t/never rest-of) t/nil-t
                                       (some #(= (count fixed) (count (:params %))) contracts) (t/union tail t/nil-t)
                                       :else tail))
            c)]
    (cond-> {:params (if (seq contracts)
                       (mapv #(or (env/hint-type file %1) %2) fixed ptypes)
                       ptypes)
             :rest rest-of
             :ret (let [ret (infer-body c body)] (if (seq contracts) (eff ctx ret) ret))}
      kwargs (assoc :kwargs kwargs))))

(defn- infer-callback [ctx node expected]
  (let [bodies (fn-bodies node)
        shapes (mapv (fn [i {:keys [fixed rest]}]
                       {:i i :params (vec (repeat (count fixed) t/unknown)) :rest (when rest t/unknown)})
                     (range) bodies)
        positional? (not-any? #(= :map (:op (:rest %))) bodies)
        targets (:arities expected)
        plans (when positional? (mapv #(t/dispatch-plan shapes %) targets))
        contracts (group-by (comp :i first) (mapcat identity plans))
        name-node (second (:children node))
        ctx (cond-> (dissoc ctx :recur-types :load-time?)
              (and (= :list (:op node)) (= :sym (:op name-node)))
              (bind (:val name-node) (or expected {:t :fn :arities (mapv #(assoc % :ret t/unknown) shapes)})))
        ft (with-meta {:t :fn :arities (mapv (fn [i branch]
                                             (infer-fn-branch ctx branch (map second (get contracts i))))
                                           (range) bodies)}
             {::t/dispatch positional?})]
    (cond-> ft
      (and positional? (= 1 (count targets) (count shapes)) (nil? (first plans)) (not (:rest (first targets))))
      (vary-meta assoc ::callback-arity-error
                 [(count (:params (first targets))) (count (:params (first shapes)))]))))

(defn- infer-fn [ctx node arg-types]
  (infer-callback ctx node (when arg-types {:t :fn :arities [{:params (vec arg-types) :ret t/unknown}]})))

(defn- force-callback [ctx {:keys [node type] captured :ctx} expected]
  (or @type
      (let [ty (infer (cond-> captured (:quiet ctx) (assoc :quiet true)) node expected)]
        (when-not (:quiet ctx) (reset! type ty))
        ty)))

(defn- apply-type
  "Result of calling a function type with these argument types; mismatches are reported at `at`."
  [ctx ftype arg-types at label]
  (if (= :all (:t ftype))
    (call-generic ctx at label ftype nil arg-types)
    (if-let [fits (seq (matching-arities ftype (count arg-types)))]
    (let [bad (fn [ar] (first (for [[a p] (map vector arg-types (concat (:params ar) (repeat (:rest ar))))
                                    :when (and p (not (t/sub? (:env ctx) (eff ctx a) p)))]
                                [a p])))
          env (:env ctx)
          vague? (some #(#{:unknown :any} (:t (t/resolve-t env %))) arg-types)
          ok (remove bad fits)
          bad-non-nil (fn [ar] (first (for [[a p] (map vector arg-types (concat (:params ar) (repeat (:rest ar))))
                                            :when (and p (not (t/sub? env (t/remove-nil env (eff ctx a)) p)))]
                                        a)))
          nil-ok (remove bad-non-nil fits)
          ar (or (if vague? (last ok) (first ok)) (first nil-ok) (last fits))]
      (when-let [[a p] (bad ar)]
        (if (seq nil-ok)
          (report! ctx at "TSP2531" (str label " takes " (show-ref ctx p) ", but may get nil: " (show-ref ctx a)))
          (let [result (t/sub-result env (eff ctx a) p)]
            (if (= :inconclusive (:status result))
              (let [[code msg extra] (incomplete-problem ctx a p result)]
                (apply report! ctx at code msg (mapcat identity extra)))
              (report! ctx at "TSP2345" (str label " takes " (show-ref ctx p) ", but gets " (show-ref ctx a)))))))
      (if (and vague? (next (distinct (map :ret ok)))) t/unknown (:ret ar)))
    (if (seq (:arities ftype))
      (do (report! ctx at "TSP2554" (str label " is called with " (count arg-types) " argument(s)")) t/unknown)
      t/unknown))))

(defn apply-fn-node [ctx fnode arg-types]
  (case (:op fnode)
    :kw (read-key ctx (first arg-types) (:val fnode) fnode)
    :set (t/union (first arg-types) t/nil-t)
    (let [ty (if (callback-node? ctx fnode)
               (infer ctx fnode {:t :fn :arities [{:params arg-types :ret t/unknown}]})
               (infer ctx fnode))
          ty (if (= :ref (:t ty)) (t/resolve-t (:env ctx) ty) ty)]
      (apply-type ctx ty arg-types fnode (parse/unparse fnode)))))

(defn- function-shape [ctx ty]
  (let [ty (t/resolve-t (:env ctx) (eff ctx ty))]
    (when (and (= :fn (:t ty)) (seq (:arities ty)) (not-any? :kwargs (:arities ty)))
      ty)))

(defn- unchecked-hof! [ctx node label reason]
  (report! ctx node "TSP210-01" (str label " (" (name reason) ")")
           :severity (if (:strict (:opts ctx)) :error :warning)
           :reasons #{reason}))

(defn- check-composable! [ctx node ty label]
  (let [ty (t/resolve-t (:env ctx) ty)]
    (if (or (= :nil (:t ty))
            (#{:string :int :double :number :boolean} (:name (if (= :lit (:t ty)) (t/lit-prim (:val ty)) ty))))
      (report! ctx node "TSP2345" (str label " expects a function, got " (t/show ty)))
      (unchecked-hof! ctx node (str label ": function signature not checked")
                      (if (= :unknown (:t ty)) :unknown :unsupported-function)))))

(defn- disjoint-scalars? [env a b]
  (let [scalar (fn [ty]
                 (case (:t ty)
                   :nil :nil
                   :lit (:name (t/lit-prim (:val ty)))
                   :prim (:name ty)
                   nil))
        domains #{:nil :string :keyword :symbol :boolean :number :int :double}
        numeric #{:number :int :double}]
    (every? (fn [b]
              (let [left (scalar a) right (scalar b)]
                (or (and (= :lit (:t a) (:t b)) (not= (:val a) (:val b)))
                    (and (domains left) (domains right) (not= left right)
                         (not (and (numeric left) (numeric right)))))))
            (t/members env b))))

(defn- comp-member [ctx fits input]
  ;; Earlier overloads may apply to part of a broad input. Keep their results
  ;; until an overload covers the entire input, without assuming disjoint maps.
  (let [env (:env ctx)
        param #(or (first (:params %)) (:rest %))]
    (loop [[a & more] fits returns [] unchecked #{}]
      (cond
        (= :never (:t input)) {:type t/never}
        (nil? a) {:reasons unchecked}
        :else
        (let [expected (param a)
              result (t/sub-result env input expected)]
          (if (= :proved (:status result))
            {:type (apply t/union (if (= :any (:t input)) (map :ret fits) (conj returns (:ret a))))}
            (recur more
                   (cond-> returns (not (disjoint-scalars? env input expected)) (conj (:ret a)))
                   (into unchecked (:reasons result)))))))))

(defn- comp-link [ctx stage ftype input]
  (if-let [fits (seq (matching-arities ftype 1))]
    (let [input (eff ctx input)
          parts (mapv #(comp-member ctx fits %) (t/members (:env ctx) input))]
      (if-let [failure (first (remove :type parts))]
        (if-let [reason (first (sort (:reasons failure)))]
          (unchecked-hof! ctx stage "comp: input compatibility not checked" reason)
          (report! ctx stage "TSP2345"
                   (str "comp: " (show-ref ctx input) " is not accepted by " (t/show ftype))))
        (apply t/union (map :type parts))))
    (report! ctx stage "TSP2554" "comp: every function except the rightmost must accept one argument")))

(defn- compose-functions [ctx nodes]
  (let [[right & stages] (rseq nodes)
        ty (infer ctx right)
        shape (or (function-shape ctx ty) (check-composable! ctx right ty "comp"))]
    (reduce (fn [inner stage]
              (let [expected (when (and inner (callback-node? ctx stage))
                               {:t :fn :arities [{:params [(apply t/union (map (comp #(eff ctx %) :ret) (:arities inner)))]
                                                  :ret t/unknown}]})
                    ty (if expected (infer ctx stage expected) (infer ctx stage))]
                (when inner
                  (if-let [outer (function-shape ctx ty)]
                    (let [arities (mapv (fn [arity]
                                          (when-let [ret (comp-link ctx stage outer (:ret arity))]
                                            (assoc arity :ret ret)))
                                        (:arities inner))]
                      (when (every? some? arities) (assoc inner :arities arities)))
                    (check-composable! ctx stage ty "comp")))))
            shape stages)))

(defn- partial-function [ctx node f args]
  (let [ty (infer ctx f)
        n (count args)]
    (if-let [shape (function-shape ctx ty)]
      (let [fits (filterv #(or (:rest %) (<= n (count (:params %)))) (:arities shape))
            inputs (infer-call-arguments ctx node args fits)
            candidates (mapv (fn [arity]
                               [arity (vec (arg-problems ctx arity args inputs node))])
                             fits)
            valid (filterv (comp empty? second) candidates)
            incomplete (some (fn [[_ ps]] (when (and (seq ps) (every? #(= "TSP210-01" (first (second %))) ps)) ps))
                             candidates)
            rest-minimum (when (::t/dispatch (meta shape))
                           (some (fn [[a _]] (when (:rest a) (count (:params a)))) valid))]
        (cond
          (empty? fits) (report! ctx node "TSP2554" (str "partial: no arity accepts " n " captured argument(s)"))
          (or incomplete (empty? valid))
          (doseq [[at [code message extra]] (or incomplete (second (apply min-key (comp count second) candidates)))]
            (apply report! ctx at code message (mapcat identity extra)))
          ;; Removing a fixed branch can expose a rest branch that runtime dispatch
          ;; would never select at that original arity.
          (and rest-minimum
               (some (fn [[a ps]]
                       (and (seq ps) (not (:rest a)) (<= rest-minimum (count (:params a)))))
                     candidates))
          (unchecked-hof! ctx node "partial: residual dispatch not checked" :unsupported-function)
          :else
          (assoc shape :arities (mapv #(update (first %) :params (fn [params] (vec (drop n params)))) valid))))
      (do (run! #(infer ctx %) args)
          (check-composable! ctx f ty "partial")))))

(defn- single-map [ctx ty]
  (let [ms (t/members (:env ctx) ty)]
    (when (and (= 1 (count ms)) (= :map (:t (first ms)))) (first ms))))

(defn- check-key-value!
  "Key = type: a value stored under a qualified key must fit the spec of that key."
  [ctx k vnode vtype]
  (when (and (qualified-keyword? k) (get-in (:env ctx) [:specs k]))
    (when-not (t/sub? (:env ctx) (eff ctx vtype) (t/ref-t k))
      (report! ctx vnode "TSP2322" (str (t/show vtype) " is not assignable to " (show-ref ctx (t/ref-t k)))))))

(defn- key-type [ctx k vtype]
  (if (and (qualified-keyword? k) (get-in (:env ctx) [:specs k])) (t/ref-t k) (widen vtype)))

;; ---- Java interop ----

(defn- class-of [ctx ty]
  (let [ms (remove #(= :nil (:t %)) (t/members (:env ctx) ty))]
    (when (= 1 (count ms))
      (let [m (first ms)]
        (case (:t m)
          :class (:name m)
          :prim (t/prim->class (:name m))
          :lit (t/prim->class (:name (t/lit-prim (:val m))))
          nil)))))

(defn- java-arg-ok? [ctx a pclass]
  (let [p (t/class-type pclass) env (:env ctx)
        num? (fn [x] (t/sub? env x (t/prim :number)))]
    (or (#{:unknown :any} (:t (t/resolve-t env a)))
        (t/sub? env (if (t/primitives pclass) a (t/remove-nil env a)) p)
        (and (num? (t/remove-nil env a)) (num? p))
        (and (= "char" pclass) (= :class (:t a))))))

(defn- java-ret [cname m]
  (let [base (t/class-type (:ret m))
        shown (java/short-name (:ret m))]
    (cond
      (java/nullable-return? cname (:name m))
      (with-meta (t/union base t/nil-t) {:null-return (str (java/short-name (:declaring m)) "." (:name m) " may return null")})
      (#{:nil :any} (:t base)) base
      :else (t/platform base shown))))

(defn- java-call [ctx node cname mname static? arg-nodes]
  (let [arg-types (mapv #(infer ctx %) arg-nodes)
        ms (map #(assoc % :name mname) (java/methods-named cname mname static?))
        fits (filter #(= (count arg-nodes) (count (:params %))) ms)
        shown (java/short-name cname)]
    (cond
      (not (java/has-class? cname)) t/unknown
      (empty? ms) (if (and (empty? arg-nodes) (java/has-field? cname mname))
                    t/unknown
                    (do (report! ctx (first (:children node)) "TSP2339"
                                 (str shown " has no " (when static? "static ") "method " mname
                                      (hint-suffix (symbol mname) (map symbol (java/method-names cname)))))
                        t/unknown))
      (some :varargs ms) t/unknown
      (empty? fits) (do (report! ctx node "TSP2554"
                                 (str shown "." mname " takes " (str/join " or " (distinct (map #(count (:params %)) ms)))
                                      " argument(s), got " (count arg-nodes)))
                        t/unknown)
      :else
      (let [ok (filter (fn [m] (every? boolean (map #(java-arg-ok? ctx %1 %2) arg-types (:params m)))) fits)]
        (when (empty? ok)
          ;; report only when every overload rejects the same argument
          ;; report the first argument that no overload accepts; stay silent when there is no such argument
          (when-let [i (first (filter (fn [i] (every? #(not (java-arg-ok? ctx (nth arg-types i) (nth (:params %) i))) fits))
                                      (range (count arg-nodes))))]
            (report! ctx (nth arg-nodes i) "TSP2345"
                     (str (t/show (nth arg-types i)) " is not assignable to " (java/short-name (nth (:params (first fits)) i)) ", "
                          (if (= 1 (count fits)) "the parameter" "a parameter") " of " shown "." mname))))
        (let [cands (if (seq ok) ok fits)
              rets (distinct (map #(java-ret cname %) cands))]
          (if (= 1 (count rets)) (first rets) t/unknown))))))

(defn- method-call [ctx node mname]
  (let [[_ target & arg-nodes] (:children node)
        actual (infer ctx target)
        hinted (check-hint! ctx target actual "type of the receiver")
        ;; Compatible hints must not erase known nullability or its provenance.
        tty (eff ctx (if (and hinted
                             (or (#{:unknown :any} (:t actual))
                                 (not (t/sub? (:env ctx) (t/remove-nil (:env ctx) actual) hinted))))
                      hinted actual))
        env (:env ctx)
        cname (class-of ctx tty)]
    (cond
      (nil? target) t/unknown
      (#{:unknown :any} (:t (t/resolve-t env tty)))
      (do (run! #(infer ctx %) arg-nodes)
          (when-not (compiler-knows? ctx target)
            (report! ctx node "TSP7020" (str "reflective call: the type of " (parse/unparse target) " is unknown")
                     :severity :warning))
          t/unknown)
      (nil? cname) (do (run! #(infer ctx %) arg-nodes) t/unknown)
      :else
      (do
        (when (t/nilable? env tty)
          (report! ctx target "TSP2531"
                   (str (parse/unparse target) " may be nil"
                        (when-let [w (or (:null-return (meta tty)) (when (= :sym (:op target)) (get-in ctx [:why (:val target)])))]
                          (str ": " w)))))
        (when (and (java/has-class? cname) (not (compiler-knows? ctx target)) (seq (java/methods-named cname mname false)))
          (let [hint (java/short-name cname)]
            (report! ctx node "TSP7023"
                     (str "reflective call, but " (parse/unparse target) " is known to be " hint)
                     :severity :warning
                     :fixes [{:title (str "Add hint ^" hint) :risk "safe"
                              :edits [{:range {:start [(:row (:pos target)) (:col (:pos target))]
                                               :end [(:row (:pos target)) (:col (:pos target))]}
                                       :text (str "^" hint " ")}]}])))
        (java-call ctx node cname mname false arg-nodes)))))

;; ---- special forms ----

(defmulti special (fn [_ctx _node head] head) :default ::none)
(defmethod special ::none [_ _ _] ::none)

(doseq [h '[ns comment declare require import in-ns defmacro defprotocol defrecord deftype defmulti
            definterface set! clojure.spec.alpha/def clojure.spec.alpha/fdef]]
  (defmethod special h [_ _ _] t/unknown))

(defn- var-reference [ctx [target & more]]
  (if (and (= :sym (:op target)) (empty? more))
    (do (resolve-var ctx (:val target) target)
        (t/class-type "clojure.lang.Var"))
    t/unknown))

(defmethod special 'var [ctx node _]
  (var-reference ctx (args-of node)))

(defmethod special 'quote [_ node _]
  (let [x (second (:children node))]
    (if (= :sym (:op x)) (t/prim :symbol) t/unknown)))

(defmethod special 'do [ctx node _] (infer-body ctx (args-of node)))

(defmethod special 'time [ctx node _]
  (let [[expr & more :as args] (args-of node)]
    (if (and expr (empty? more))
      (infer ctx expr)
      (do (run! #(infer ctx %) args)
          (report! ctx node "TSP2554" "time expects one expression")
          t/unknown))))

(defmethod special 'future [ctx node _]
  (infer-body (dissoc ctx :recur-types) (args-of node))
  (t/class-type "java.util.concurrent.Future"))

(doseq [h '[def defonce]]
  (defmethod special h [ctx node _]
    (some->> (env/definition-initializer node) (infer ctx))
    t/unknown))

(defmethod special 'let [ctx node _]
  (let [[bindings & body] (args-of node)
        pending (volatile! [])
        c (bind-pairs (assoc ctx :pending-callbacks pending) bindings)
        result (infer-body (dissoc c :pending-callbacks) body)]
    (run! #(force-callback ctx % nil) @pending)
    result))

(defmethod special 'loop [ctx node _]
  (let [[bindings & body] (args-of node)
        pairs (vec (partition 2 (:children bindings)))
        ;; Initializers have let-like scope and are checked exactly once.
        [_ seeds] (reduce (fn [[c ts] [p init]]
                            (let [ty (widen (infer c init))]
                              [(bind-pattern c p ty) (conj ts ty)]))
                          [ctx []] pairs)
        bind-loop (fn [ts calls]
                    (reduce (fn [c [[p _] ty]] (bind-pattern c p ty))
                            (assoc ctx :recur-types calls) (map vector pairs ts)))
        ;; Join back edges, not just initializers: recur can change a local's
        ;; type. Bound structural growth and keep trial diagnostics/caches local.
        stable (binding [*types* nil *diags* (atom [])
                         *trust* (atom {:cast 0 :any 0 :ignore 0})
                         *ignores* (atom []) *rets* (atom @*rets*)]
                 (loop [ts seeds n 0]
                   (let [calls (atom [])
                         _ (infer-body (assoc (bind-loop ts calls) :quiet true) body)
                         next-ts (if (every? #(= (count ts) (count %)) @calls)
                                   (mapv (fn [i ty]
                                           (widen (apply t/union ty (map #(nth % i) @calls))))
                                         (range (count ts)) ts)
                                   (vec (repeat (count ts) t/unknown)))]
                     (cond (= ts next-ts) ts
                           (>= n 7) (vec (repeat (count ts) t/unknown))
                           :else (recur next-ts (inc n))))))]
    (infer-body (bind-loop stable nil) body)))

(defmethod special 'recur [ctx node _]
  (let [ts (mapv #(infer ctx %) (args-of node))]
    (when-let [calls (:recur-types ctx)] (swap! calls conj ts)))
  t/never)
(defmethod special 'throw [ctx node _] (run! #(infer ctx %) (args-of node)) t/never)

(doseq [h '[some? nil?]]
  (defmethod special h [ctx node head]
    (let [ty (eff ctx (infer ctx (first (args-of node))))
          present (t/remove-nil (:env ctx) ty)
          absent (nil-part (:env ctx) ty)]
      (cond (= :never (:t present)) (t/lit (= head 'nil?))
            (= :never (:t absent)) (t/lit (= head 'some?))
            :else (t/prim :boolean)))))

(defn- if-form [ctx node inverted?]
  (let [[test then else] (args-of node)
        branches (condition-contexts ctx test)
        [tc fc] (if inverted? (reverse branches) branches)]
    (t/union (if tc (infer tc then) t/never)
             (if fc (if else (infer fc else) t/nil-t) t/never))))

(defmethod special 'if [ctx node _] (if-form ctx node false))

(defmethod special 'if-not [ctx node _] (if-form ctx node true))

(defn- when-form [ctx node inverted?]
  (let [[test & body] (args-of node)
        branches (condition-contexts ctx test)
        [tc fc] (if inverted? (reverse branches) branches)]
    (t/union (if tc (infer-body tc body) t/never)
             (if fc t/nil-t t/never))))

(defmethod special 'when [ctx node _] (when-form ctx node false))
(defmethod special 'when-not [ctx node _] (when-form ctx node true))

(defn- if-let-form [ctx node some?]
  (let [[bindings then else] (args-of node)
        [p init] (:children bindings)
        ty (eff ctx (infer ctx init))
        positive (if some? (t/remove-nil (:env ctx) ty) (truth-part (:env ctx) ty true))
        negative (if some? (nil-part (:env ctx) ty) (truth-part (:env ctx) ty false))
        tc (bind-pattern ctx p (dissoc positive :fresh))]
    [(if (= :never (:t positive)) t/never (infer tc then))
     (if (= :never (:t negative)) t/never (if else (infer ctx else) t/nil-t))]))

(defmethod special 'if-let [ctx node _] (apply t/union (if-let-form ctx node false)))
(defmethod special 'if-some [ctx node _] (apply t/union (if-let-form ctx node true)))

(defn- when-let-form [ctx node some?]
  (let [[bindings & body] (args-of node)
        [p init] (:children bindings)
        ty (eff ctx (infer ctx init))
        positive (if some? (t/remove-nil (:env ctx) ty) (truth-part (:env ctx) ty true))
        negative (if some? (nil-part (:env ctx) ty) (truth-part (:env ctx) ty false))
        tc (bind-pattern ctx p (dissoc positive :fresh))]
    (t/union (if (= :never (:t positive)) t/never (infer-body tc body))
             (if (= :never (:t negative)) t/never t/nil-t))))

(defmethod special 'when-let [ctx node _] (when-let-form ctx node false))
(defmethod special 'when-some [ctx node _] (when-let-form ctx node true))

(defmethod special 'cond [ctx node _]
  (loop [ctx ctx pairs (partition 2 (args-of node)) out []]
    (cond
      (nil? ctx) (apply t/union out)
      (empty? pairs) (apply t/union (conj out t/nil-t))
      :else (let [[test branch] (first pairs)
                  [tc fc] (condition-contexts ctx test)]
              (recur fc (rest pairs) (cond-> out tc (conj (infer tc branch))))))))

(defn- logical-form [ctx args and?]
  (if (empty? args)
    (t/lit and?)
    (let [a (first args)
          ty (infer ctx a)]
      (if-not (next args)
        ty
        (let [branches (narrow ctx a)
              [proceed stop] (if and? branches (reverse branches))
              ety (eff ctx ty)
              proceed-type (truth-part (:env ctx) ety and?)
              stop-type (truth-part (:env ctx) ety (not and?))]
          (t/union (if stop stop-type t/never)
                   (if (and proceed (not= :never (:t proceed-type)))
                     (logical-form proceed (rest args) and?)
                     t/never)))))))

(defmethod special 'and [ctx node _] (logical-form ctx (args-of node) true))
(defmethod special 'or [ctx node _] (logical-form ctx (args-of node) false))

(defn- case-int? [v]
  (and (integer? v) (<= Integer/MIN_VALUE v Integer/MAX_VALUE)))

(defn- case-tag-type? [int-mode? {:keys [t val]}]
  (or (= :nil t)
      (and (= :lit t)
           (or (keyword? val) (string? val) (boolean? val)
               ;; Hash-based case can distinguish equal numbers by JVM representation.
               (and int-mode? (case-int? val))))))

(defmethod special 'case [ctx node _]
  (let [[test & clauses] (args-of node)
        env (:env ctx)
        tty (infer ctx test)
        default? (odd? (count clauses))
        pairs (partition 2 clauses)
        lits (fn [n] (if (= :list (:op n)) (:children n) [n]))
        labels (mapcat (comp lits first) pairs)
        literal-labels? (every? literal-node? labels)
        subject (stable-path ctx test)
        int-mode? (every? #(case-int? (:val %)) labels)
        paths (when (and subject literal-labels?)
                (for [[member rebuild] (path-alternatives ctx (:type subject) (:path subject))
                      leaf (expand-boolean member)]
                  [leaf rebuild]))
        leaves (if subject (map first paths) (mapcat expand-boolean (t/members env (eff ctx tty))))
        closed? (and literal-labels? (seq leaves) (every? #(case-tag-type? int-mode? %) leaves))
        variants (when closed? paths)
        members (distinct leaves)
        handled (set (map :val labels))
        missing (remove #(contains? handled (:val %)) members)
        local (plain-local ctx test)
        infer-variant (fn [labels branch]
                        (if-let [branch-ctx (if variants
                                             (refine-alternatives ctx subject variants #(when (contains? labels (:val %)) %))
                                             (when (some #(contains? labels (:val %)) members) ctx))]
                          (infer branch-ctx branch)
                          t/never))]
    (when closed?
      (doseq [l labels :when (not (some #(= (:val l) (:val %)) members))]
        (report! ctx l "TSP2678" (str (pr-str (:val l)) " is not a possible value of " (show-ref ctx tty)
                                      (hint-suffix (:val l) (map :val members)))))
      (when (and (not default?) (seq missing))
        (report! ctx node "TSP8001" (str "case is not exhaustive: " (str/join ", " (map #(pr-str (:val %)) missing))
                                        (if (next missing) " are" " is") " not handled"))))
    (apply t/union
           (concat
            (for [[c branch] pairs]
              (if closed?
                (infer-variant (set (map :val (lits c))) branch)
                (infer (if (and local (every? literal-node? (lits c)))
                         (assoc-in ctx [:locals local] (apply t/union (map (comp literal-type :val) (lits c))))
                         ctx)
                       branch)))
            (when default?
              [(if closed?
                 (infer-variant (set (map :val missing)) (last clauses))
                 (infer ctx (last clauses)))])))))

(defn- literal-domain [ctx expr]
  (when-let [{:keys [type path]} (stable-path ctx expr)]
    (let [leaves (mapcat (comp expand-boolean first) (path-alternatives ctx type path))]
      (when (and (seq leaves) (every? finite-value? leaves))
        (distinct (map :val leaves))))))

(defn- condp-clauses [nodes]
  (loop [[test branch result & more :as left] nodes out []]
    (cond
      (nil? branch) {:pairs out :default test}
      (and (= :>> (:val branch)) result) (recur more (conj out [test result true]))
      :else (recur (drop 2 left) (conj out [test branch false])))))

(defmethod special 'condp [ctx node _]
  (let [[pred expr & clauses] (args-of node)
        pred-type (infer ctx pred)
        expr-type (infer ctx expr)
        equality? (= '= (builtin-head ctx {:op :list :children [pred]}))
        {:keys [pairs default]} (condp-clauses clauses)
        finite? (and equality? (every? #(literal-node? (first %)) pairs) (seq (literal-domain ctx expr)))]
    (loop [ctx ctx [[test result invoke?] & more] pairs out []]
      (cond
        (nil? ctx) (apply t/union out)
        (nil? test) (if default
                      (apply t/union (conj out (infer ctx default)))
                      (do (when-let [missing (and finite? (seq (literal-domain ctx expr)))]
                            (report! ctx node "TSP8001" (str "condp is not exhaustive: " (str/join ", " (map pr-str missing))
                                                            (if (next missing) " are" " is") " not handled")))
                          (apply t/union out)))
        :else (let [test-type (infer ctx test)
                    matched (apply-type ctx pred-type [test-type expr-type] test "condp predicate")
                    [yes no] (or (when (and equality? (literal-node? test))
                                  (equality-contexts ctx expr (:val test)))
                                [ctx ctx])
                    present (truth-part (:env ctx) matched true)
                    absent (truth-part (:env ctx) matched false)
                    yes (when-not (= :never (:t present)) yes)
                    no (when-not (= :never (:t absent)) no)]
                (recur no more
                       (cond-> out yes
                         (conj (if invoke?
                                 (apply-fn-node yes result [present])
                                 (infer yes result))))))))))

(defmethod special 'fn [ctx node _] (infer-fn ctx node nil))
(defmethod special 'fn* [ctx node _] (infer-fn ctx node nil))

(defmethod special 'try [ctx node _]
  (let [forms (args-of node)
        catch? #(= 'catch (env/head (:file ctx) %))
        finally? #(= 'finally (env/head (:file ctx) %))
        body (remove #(or (catch? %) (finally? %)) forms)]
    (run! #(infer-body ctx (args-of %)) (filter finally? forms))
    (apply t/union
           (infer-body ctx body)
           (for [c (filter catch? forms)
                 :let [[cls sym & cbody] (args-of c)
                       cn (t/resolve-class (:imports (:file ctx)) (:val cls))]]
             (infer-body (bind ctx (:val sym) (if cn (t/class-type cn) t/unknown) :hinted true) cbody)))))

(defn- threaded-step [ctx step first? previous display input hinted?]
  (let [names (into (set (keys (:locals ctx)))
                    (keep #(when (= :sym (:op %)) (:val %)))
                    (tree-seq (comp seq :children) :children step))
        sym (if (contains? names '__threaded)
              (first (remove names (map #(symbol (str "__threaded_" %)) (range))))
              '__threaded)
        acc {:op :sym :val sym :pos (:pos previous) :synthetic true :display display}
        call (if (= :list (:op step))
               (let [[h & args] (:children step)]
                 (assoc step :children (vec (if first? (concat [h acc] args) (concat [h] args [acc])))))
               {:op :list :pos (:pos step) :children [step acc]})]
    (infer (bind ctx sym input :why (why-of ctx previous input) :hinted hinted?) call)))

(defn- thread [ctx node first? some?]
  (let [[x & steps] (args-of node) env (:env ctx)]
    (loop [ty (infer ctx x) prev x steps steps maybe-nil? false shown (parse/unparse x) n 0]
      (if (empty? steps)
        (if (and some? maybe-nil?) (t/union ty t/nil-t) ty)
        (let [step (first steps)
              arrow (parse/unparse (first (:children node)))
              display (if (zero? n) shown (str "(" arrow " " shown ")"))
              in (if some? (t/remove-nil env ty) ty)]
          (recur (threaded-step ctx step first? prev display in (compiler-knows? ctx prev))
                 step (rest steps) (or maybe-nil? (t/nilable? env ty))
                 (str shown " " (parse/unparse step)) (inc n)))))))

(defmethod special '-> [ctx node _] (thread ctx node true false))
(defmethod special '->> [ctx node _] (thread ctx node false false))
(defmethod special 'some-> [ctx node _] (thread ctx node true true))
(defmethod special 'some->> [ctx node _] (thread ctx node false true))

(defmethod special 'doseq [ctx node _]
  (let [[bindings & body] (args-of node)]
    (infer-body (bind-seq-pairs ctx bindings) body)
    t/nil-t))

(defmethod special 'for [ctx node _]
  (let [[bindings & body] (args-of node)]
    {:t :coll :kind :seq :of (widen (dissoc (infer-body (bind-seq-pairs ctx bindings) body) :fresh))}))

(defmethod special 'dotimes [ctx node _]
  (let [[bindings & body] (args-of node)
        [s n] (:children bindings)]
    (infer ctx n)
    (infer-body (bind ctx (:val s) (t/prim :int)) body)
    t/nil-t))

(defmethod special 'defmethod [ctx node _]
  (let [[_ _ & more] (args-of node)
        [params & body] (drop-while #(not= :vector (:op %)) more)]
    (when params
      (infer-body (reduce #(bind-pattern %1 %2 (env/pattern-type (:env ctx) (:file ctx) %2)) (dissoc ctx :load-time?)
                          (:fixed (env/split-params params)))
                  body))
    t/unknown))

(doseq [h '[with-open binding with-redefs with-local-vars]]
  (defmethod special h [ctx node _]
    (let [[bindings & body] (args-of node)]
      (infer-body (case h
                    with-open (bind-pairs ctx bindings)
                    (do (run! #(infer ctx (second %)) (partition 2 (:children bindings)))
                        (cond-> ctx (= 'with-redefs h) (assoc :guards-disabled? true))))
                  body))))

(defmethod special 'doto [ctx node _]
  (let [[x & forms] (args-of node)
        ty (infer ctx x)]
    (run! (fn [f] (run! #(infer ctx %) (rest (:children f)))) (filter #(= :list (:op %)) forms))
    ty))

(defmethod special 'as-> [ctx node _]
  (let [[x nm & forms] (args-of node)]
    (reduce (fn [ty f] (infer (bind ctx (:val nm) (dissoc ty :fresh)) f)) (infer ctx x) forms)))

(defn- conditional-thread [ctx node first?]
  (let [[x & clauses] (args-of node)
        source-local (plain-local ctx x)
        initial (infer ctx x)]
    (if (or (nil? x) (odd? (count clauses)))
      (do (report! ctx node "TSP2554" "conditional threading requires a value followed by test/step pairs")
          t/unknown)
      (loop [ty initial, previous x, hinted? (compiler-knows? ctx x), pairs (seq (partition 2 clauses))]
        (if (and pairs (not= t/never ty))
          (let [[[test step] & more] pairs
                [on off] (condition-contexts ctx test)
                ;; Before any step can run, the accumulator still aliases the original local.
                input (fn [branch] (or (when (identical? previous x) (get-in branch [:locals source-local])) ty))
                executed (if on (threaded-step on step first? previous (parse/unparse previous) (input on) hinted?) t/never)
                joined (t/union executed (if off (input off) t/never))]
            ;; Bound the number of shapes produced by independent optional steps.
            (recur (if (> (count (:types joined)) 32) t/unknown joined)
                   (if on step previous)
                   (if on (and (not off) (compiler-knows? on step)) hinted?)
                   (seq more)))
          ty)))))

(defmethod special 'cond-> [ctx node _] (conditional-thread ctx node true))
(defmethod special 'cond->> [ctx node _] (conditional-thread ctx node false))

;; maps

(defmethod special 'get [ctx node _]
  (let [[m k default] (args-of node)
        mty (infer ctx m)]
    (if (= :kw (:op k))
      (if default
        (read-key-default ctx mty (:val k) k (infer ctx default))
        (read-key ctx mty (:val k) k))
      (do (infer ctx k) (some->> default (infer ctx)) t/unknown))))

(defn- numeric-path-value [ctx ty index]
  (let [env (:env ctx)
        vector-type (t/prim :vector)]
    (apply t/union
           (for [member (t/members env ty)]
             (case (:t member)
               :coll (case (:kind member)
                       :vector (if (<= 0 index Integer/MAX_VALUE) (:of member) t/never)
                       :set (:of member)
                       ;; A kindless coll-of can be a map whose entries fit the
                       ;; element spec. get returns the value of an entry there.
                       nil (if (some #(or (t/sub? env % vector-type)
                                         (not= :mismatch (:status (t/sub-result env vector-type %))))
                                     (t/members env (:of member)))
                             t/unknown (:of member))
                       t/never)
               :map (if (:complete-keys? member) t/never t/unknown)
               :map-of (:v member)
               :lit (if (and (string? (:val member)) (<= 0 index (dec (count (:val member)))))
                      (t/class-type "java.lang.Character") t/never)
               :prim (case (:name member)
                       :string (if (<= 0 index Integer/MAX_VALUE) (t/class-type "java.lang.Character") t/never)
                       (:map :vector :set :coll) t/unknown
                       t/never)
               :any t/any
               (:class :unknown :var :rigid :ivar) t/unknown
               t/never)))))

(defn- path-value [ctx ty keys not-found]
  (loop [ty ty, keys (seq keys), missing? false]
    (if (and keys (not= :never (:t ty)))
      (let [[key & more] keys
            current (eff ctx ty)
            {:keys [type optional?]} (if (= :num (:op key))
                                       {:type (numeric-path-value ctx current (:val key)) :optional? true}
                                       (key-info ctx current (:val key) key true))]
        (recur type more (or missing? optional?)))
      (if missing? (t/union ty not-found) ty))))

(defmethod special 'get-in [ctx node _]
  (let [[m path default & extra :as arguments] (args-of node)
        mty (infer ctx m)
        keys (:children path)]
    (cond
      (or (nil? path) (seq extra))
      (do (run! #(infer ctx %) (rest arguments))
          (report! ctx node "TSP2554" (str "get-in is called with " (count arguments) " argument(s)"))
          t/unknown)

      (and (#{:vector :nil} (:op path)) (empty? (:directives path)) (not (env/tag-node path))
           (every? #(and (literal-node? %) (or (#{:kw :str} (:op %)) (integer? (:val %)))) keys))
      (path-value ctx mty keys (if default (infer ctx default) t/nil-t))

      :else (do (infer ctx path) (some->> default (infer ctx)) t/unknown))))

(defn- assoc-map-key [m k ty]
  (-> m (dissoc :fresh) (assoc-in [:req k] ty) (update :opt dissoc k)))

(def ^:private empty-map-shape {:t :map :req {} :opt {} :inferred true :complete-keys? true})

(defn- assoc-key-type [ctx ty k value]
  (let [members (t/members (:env ctx) ty)]
    (if (every? #(#{:map :nil} (:t %)) members)
      (apply t/union (map #(assoc-map-key (if (= :nil (:t %)) empty-map-shape %) k value) members))
      ty)))

(defn- forget-map-keys [ctx ty]
  (apply t/union (map #(if (= :map (:t %)) (t/prim :map) %) (t/members (:env ctx) ty))))

(defn- path-writes
  "Find the previous leaf and rebuild each possible root after a keyword-path write."
  [ctx root keys]
  (let [env (:env ctx)
        budget (volatile! 256)
        unchecked [t/unknown (constantly t/unknown)]]
    (letfn [(walk [ty [key & more]]
              (let [k (:val key)
                    members (vec (take (inc @budget) (t/members env (eff ctx ty))))]
                (if (neg? (vswap! budget - (count members)))
                  [unchecked]
                  (into []
                        (mapcat
                         (fn [member]
                           (case (:t member)
                             (:map :nil)
                             (let [m (if (= :nil (:t member)) empty-map-shape member)
                                   previous (cond
                                              (contains? (:req m) k) (get (:req m) k)
                                              (contains? (:opt m) k) (t/union (get (:opt m) k) t/nil-t)
                                              (:complete-keys? m) t/nil-t
                                              (get-in env [:specs k]) (t/union (t/ref-t k) t/nil-t)
                                              :else t/unknown)]
                               (if (seq more)
                                 (for [[leaf rebuild] (walk previous more)]
                                   [leaf (fn [value]
                                           (let [child (rebuild value)]
                                             (when-not (= :unknown (:t child)) (check-key-value! ctx k key child))
                                             (assoc-map-key m k child)))])
                                 [[previous #(assoc-map-key m k %)]]))
                             :never []
                             (:any :unknown :map-of :class :var :rigid :ivar) [unchecked]
                             (if (and (= :prim (:t member)) (= :map (:name member)))
                               [unchecked]
                               (do (report! ctx key "TSP2339" (str "cannot write " k " into " (t/show member)))
                                   []))))
                        members)))))]
      (walk root keys))))

(defn- write-path [ctx node update?]
  (let [[m path value & args :as arguments] (args-of node)
        mty (infer ctx m)
        keys (:children path)]
    (cond
      (or (nil? value) (and (not update?) (seq args)))
      (do (run! #(infer ctx %) (rest arguments))
          (report! ctx node "TSP2554" (str (if update? "update-in" "assoc-in") " is called with " (count arguments) " argument(s)"))
          t/unknown)

      (and (= :vector (:op path)) (empty? (:directives path)) (not (env/tag-node path)) (seq keys)
           (every? #(and (= :kw (:op %)) (literal-node? %)) keys))
      (let [writes (path-writes ctx mty keys)
            vty (if (and update? (seq writes))
                  (apply-fn-node ctx value (into [(apply t/union (map first writes))] (map #(infer ctx %) args)))
                  (do (run! #(infer ctx %) args) (infer ctx value)))
            k (:val (last keys))
            spec (when (get-in (:env ctx) [:specs k]) (t/ref-t k))
            stored (if (and spec (not (t/sub? (:env ctx) (eff ctx vty) spec)))
                     spec
                     (widen (dissoc vty :fresh)))]
        (when (seq writes) (check-key-value! ctx k value vty))
        (if (= t/never vty) t/never (apply t/union (map #((second %) stored) writes))))

      :else
      (do (run! #(infer ctx %) (rest arguments)) t/unknown))))

(defmethod special 'assoc-in [ctx node _] (write-path ctx node false))
(defmethod special 'update-in [ctx node _] (write-path ctx node true))

(defmethod special 'assoc [ctx node _]
  (let [[m & kvs] (args-of node)
        mty (infer ctx m)]
    (reduce (fn [ty [k v]]
              (let [vty (infer ctx v)]
                (if (= :kw (:op k))
                  (do (check-key-value! ctx (:val k) v vty)
                      (assoc-key-type ctx ty (:val k) (key-type ctx (:val k) vty)))
                  (do (infer ctx k) (forget-map-keys ctx ty)))))
            mty (partition 2 kvs))))

(defmethod special 'dissoc [ctx node _]
  (let [[m & ks] (args-of node)
        mty (infer ctx m)
        members (t/members (:env ctx) mty)]
    (if (and (every? #(= :kw (:op %)) ks) (every? #(#{:map :nil} (:t %)) members))
      (apply t/union
             (for [member members]
               (if (= :map (:t member))
                 (reduce #(-> %1 (update :req dissoc (:val %2)) (update :opt dissoc (:val %2)))
                         (dissoc member :fresh) ks)
                 member)))
      (do (run! #(infer ctx %) ks) (forget-map-keys ctx mty)))))

(defmethod special 'update [ctx node _]
  (let [[m k f & args] (args-of node)
        mty (infer ctx m)
        arg-types (mapv #(infer ctx %) args)]
    (if (= :kw (:op k))
      (let [cur (read-key ctx mty (:val k) k)
            ret (apply-fn-node ctx f (into [cur] arg-types))]
        (check-key-value! ctx (:val k) f ret)
        (assoc-key-type ctx mty (:val k) (key-type ctx (:val k) ret)))
      (do (infer ctx k) (infer ctx f) (forget-map-keys ctx mty)))))

(defmethod special 'merge [ctx node _]
  (let [tys (map #(infer ctx %) (args-of node))
        ms (map #(single-map ctx %) tys)]
    (if (and (seq ms) (every? some? ms))
      (reduce (fn [a b]
                (let [req (merge (reduce-kv (fn [r k v]
                                             (if (contains? r k)
                                               (update r k t/union v)
                                               r))
                                           (:req a) (:opt b))
                                 (:req b))
                      opt (merge-with t/union (:opt a) (:opt b))]
                  {:t :map :req req :opt (apply dissoc opt (keys req))}))
              ms)
      t/unknown)))

(defmethod special 'select-keys [ctx node _]
  (let [[m ks] (args-of node)
        mty (infer ctx m)]
    (if (and (= :vector (:op ks)) (every? #(= :kw (:op %)) (:children ks)))
      (let [infos (for [k (:children ks)] [(:val k) (key-info ctx mty (:val k) k)])]
        {:t :map
         :req (into (array-map) (for [[k i] infos :when (not (:optional? i))] [k (:type i)]))
         :opt (into (array-map) (for [[k i] infos :when (:optional? i)] [k (:type i)]))})
      (do (infer ctx ks) t/unknown))))

;; collections

(defn- coll-of [kind ty] {:t :coll :kind kind :of (widen (dissoc ty :fresh))})

(defn- unsupported-sequence-member [ctx ty]
  (some (fn [member]
          (when (#{:int :double :number :boolean :keyword :symbol :uuid :inst}
                 (:name (if (= :lit (:t member)) (t/lit-prim (:val member)) member)))
            member))
        (t/members (:env ctx) ty)))

(defmethod special 'lazy-seq [ctx node _]
  (let [forms (args-of node)
        body (infer-body (dissoc ctx :recur-types) forms)]
    (when-let [bad (unsupported-sequence-member ctx body)]
      (report! ctx (last forms) "TSP2345" (str "lazy-seq cannot realize a sequence from " (t/show bad))))
    (coll-of :seq (if (= t/never body) t/never (elem-type ctx body)))))

(doseq [h '[first second last peek rand-nth ffirst]]
  (defmethod special h [ctx node _]
    (let [[c] (args-of node)]
      (indexed (elem-type ctx (infer ctx c))))))

(defmethod special 'nth [ctx node _]
  (let [[c & more] (args-of node)]
    (run! #(infer ctx %) more)
    (elem-type ctx (infer ctx c))))

(doseq [h '[rest next seq reverse distinct sort shuffle butlast vec set doall]]
  (defmethod special h [ctx node _]
    (let [[c] (args-of node)
          el (elem-type ctx (infer ctx c))
          res (coll-of (get '{vec :vector set :set} h :seq) el)]
      (if ('#{next seq butlast} h) (t/union res t/nil-t) res))))

(doseq [h '[take drop take-last drop-last partition-all]]
  (defmethod special h [ctx node _]
    (let [[n c] (args-of node)]
      (infer ctx n)
      (coll-of :seq (elem-type ctx (infer ctx c))))))

(def ^:private comp-identity
  (t/parse-type {:fail #(throw (ex-info "Invalid comp identity signature" {:form %}))}
                '(t/all [a] [a :-> a])))

(defmethod special 'comp [ctx node _]
  (let [stages (args-of node)]
    (case (count stages)
      0 comp-identity
      1 (infer ctx (first stages))
      (or (compose-functions ctx stages) t/unknown))))

(defmethod special 'partial [ctx node _]
  (let [[f & args] (args-of node)]
    (cond
      (nil? f) (do (report! ctx node "TSP2554" "partial expects at least one argument") t/unknown)
      (empty? args) (infer ctx f)
      :else (or (partial-function ctx node f args) t/unknown))))

(doseq [h '[map mapv keep mapcat pmap]]
  (defmethod special h [ctx node _]
    (let [[f & colls] (args-of node)
          els (mapv #(elem-type ctx (infer ctx %)) colls)]
      (if (empty? colls)
        (do (infer ctx f) t/unknown)
        (let [ret (apply-fn-node ctx f els)
              ret (case h keep (t/remove-nil (:env ctx) ret) mapcat (elem-type ctx ret) ret)]
          (coll-of (if (= 'mapv h) :vector :seq) ret))))))

(defn- filter-types [ctx f el]
  (let [env (:env ctx)]
    (if-let [summary (get-in env [:guards (guard-symbol ctx f)])]
      (if (empty? (:directives f)) (guards/apply-summary env summary el) [el el])
      (let [descriptor (deferred-callback ctx f)
            node (or (:node descriptor) f)
            captured (or (:ctx descriptor) ctx)
            plan (cond
                   (seq (:directives node)) nil
                   (= :sym (:op node))
                   (let [parameter (gensym "guard-value")]
                     (guards/compile-body (guard-compiler ctx) [parameter]
                                          [{:op :list :children [node {:op :sym :val parameter}]}]))
                   (and (inline-callback? captured node)
                        (or (= :fn-lit (:op node)) (not= :sym (:op (second (:children node))))))
                   (let [[branch :as bodies] (fn-bodies node)]
                     (when (and (= 1 (count bodies) (count (:fixed branch))) (not (:rest branch))
                                (= :sym (:op (first (:fixed branch)))))
                       (let [parameter (:val (first (:fixed branch)))]
                         (guards/compile-body (guard-compiler captured)
                                              (cond-> [parameter] (:reader? branch) (conj '%))
                                              (:body branch))))))]
        (if (guards/useful? plan) (guards/split env plan el) [el el])))))

(defn- apply-predicate [ctx f el narrow?]
  (let [before (when *diags* (count @*diags*))
        ret (apply-fn-node ctx f [el])
        [yes no] (if (and narrow? (not (:guards-disabled? ctx))
                          (or (nil? before) (= before (count @*diags*))))
                   (filter-types ctx f el) [el el])]
    [ret yes no]))

(doseq [h '[filter remove filterv take-while drop-while sort-by group-by some every? not-any? run! max-key min-key]]
  (defmethod special h [ctx node _]
    (let [[f & more] (args-of node)
          c (last more)]
      (if (nil? c)
        (do (infer ctx f) t/unknown)
        (let [el (elem-type ctx (infer ctx c))
              [ret yes no] (apply-predicate ctx f el (#{'filter 'filterv 'remove 'take-while} h))
              result (fn [kind selected] {:t :coll :kind kind :of (dissoc selected :fresh)})]
          (case h
            (filter take-while) (result :seq yes)
            remove (result :seq no)
            (drop-while sort-by) (coll-of :seq el)
            filterv (result :vector yes)
            (every? not-any?) (t/prim :boolean)
            some (t/union (t/remove-nil (:env ctx) ret) t/nil-t)
            run! t/nil-t
            (max-key min-key) el
            group-by {:t :map-of :k (widen ret) :v (coll-of :vector el)}))))))

(defn- collection-count [ctx node type]
  (cond
    (#{:vector :set} (:op node)) (count (:children node))
    (and (= :quote (:op node)) (= :list (:op (first (:children node)))))
    (count (:children (first (:children node))))
    (= t/never (elem-type ctx type)) 0))

(defn- reduction-function [ctx f calls]
  (let [expected {:t :fn :arities (mapv #(hash-map :params % :ret t/unknown) calls)}
        ty (if (callback-node? ctx f) (infer ctx f expected) (infer ctx f))]
    (if (= :ref (:t ty)) (t/resolve-t (:env ctx) ty) ty)))

(defn- reduction-result [ctx f ty args]
  (if (or (= :all (:t ty)) (seq (:arities ty)))
    (let [ret (eff ctx (apply-type ctx ty args f (parse/unparse f)))]
      ;; reduce unwraps Reduced. Until its value is represented, the wrapper
      ;; cannot serve as evidence of the final accumulator type.
      (if (some #(= "clojure.lang.Reduced" (:name %)) (t/members (:env ctx) ret)) t/unknown ret))
    t/unknown))

(defn- join-accumulator [acc step]
  (if (and (= :coll (:t acc) (:t step)) (= (:kind acc) (:kind step)))
    (update acc :of t/union (:of step))
    (t/union acc step)))

(defn- reduction-accumulator [ctx f seed el empty-call?]
  (loop [acc seed iterations 0]
    (let [args [acc el]
          calls (cond-> [] empty-call? (conj []) true (conj args))
          step (probe ctx #(reduction-result % f (reduction-function % f calls) args))]
      (cond
        (t/sub? (:env ctx) step acc) acc
        (= :unknown (:t step)) nil
        (= iterations 7) nil
        :else (recur (join-accumulator acc (widen (dissoc step :fresh))) (inc iterations))))))

(defmethod special 'reduce [ctx node _]
  (let [[f a b & more :as args] (args-of node)]
    (if (or (< (count args) 2) (seq more))
      (do (run! #(infer ctx %) args)
          (report! ctx node "TSP2554" "reduce expects 2 or 3 arguments")
          t/unknown)
      (let [[init c] (if b [a b] [nil a])
            initial (when init (infer ctx init))
            ct (infer ctx c)
            el (elem-type ctx ct)
            n (collection-count ctx c ct)
            may-be-empty? (or (nil? n) (zero? n))
            step? (or (nil? n) (>= n (if init 1 2)))
            repeat? (and step? (or (nil? n) (>= n (if init 2 3))))
            start (if init (widen (dissoc initial :fresh)) el)
            empty-call? (and (not init) may-be-empty?)
            invariant (if repeat? (reduction-accumulator ctx f start el empty-call?) start)
            step-args [(or invariant start) el]
            calls (cond-> [] empty-call? (conj []) step? (conj step-args))
            ty (if (seq calls) (reduction-function ctx f calls) (infer ctx f))
            step (when step? (reduction-result ctx f ty step-args))
            base (if init
                   (if may-be-empty? initial t/never)
                   (t/union (if may-be-empty? (reduction-result ctx f ty []) t/never)
                            (if (or (nil? n) (= n 1)) el t/never)))]
        (when (or (and step? (or (nil? invariant) (= :unknown (:t step)))
                       (or (empty? (:arities ty)) (seq (matching-arities ty 2))))
                  (and empty-call? (not (or (= :all (:t ty)) (seq (:arities ty))))))
          (report! ctx f "TSP210-01" "reduce accumulator is not checked"
                   :severity (if (:strict (:opts ctx)) :error :warning)
                   :reasons #{:accumulator}))
        (cond
          (not step?) base
          (nil? invariant) t/unknown
          :else (t/union base step))))))

(defmethod special 'conj [ctx node _]
  (let [[c & xs] (args-of node)
        cty (infer ctx c)
        xts (map #(infer ctx %) xs)
        m (first (remove #(= :nil (:t %)) (t/members (:env ctx) cty)))]
    (if (= :coll (:t m))
      (assoc m :of (apply t/union (:of m) (map #(widen (dissoc % :fresh)) xts)))
      cty)))

(defn- transducer-element [ctx ty]
  (t/with-assumption
   (apply t/union
          (for [member (t/members (:env ctx) ty)]
            (case (:t member)
              :never t/never
              :map (if (and (:complete-keys? member) (empty? (:req member)) (empty? (:opt member)))
                     t/never (t/prim :vector))
              (elem-type ctx member))))
   (t/assumption ty)))

(defn- apply-transducer
  "Infer the emitted element type. A nil result means a stage could not be modeled."
  [ctx xf input]
  (let [[f & more :as args] (args-of xf)
        head (when (empty? (:directives (first (:children xf)))) (builtin-head ctx xf))]
    (cond
      (= 'comp head)
      (let [out (reduce (fn [current stage]
                          ;; An opaque stage can also change completion. Keep the
                          ;; chain incomplete while still checking following forms.
                          (let [next-type (apply-transducer ctx stage (or current t/unknown))]
                            (when current next-type)))
                        input args)]
        (record! ctx xf (t/prim :fn))
        out)

      (and (#{'map 'filter} head) f (empty? more))
      (let [out (case head
                  map (apply-fn-node ctx f [input])
                  filter (let [[ret yes] (apply-predicate ctx f input true)]
                           (if (= t/never (truth-part (:env ctx) (eff ctx ret) true)) t/never yes)))]
        (record! ctx xf (t/prim :fn))
        (if (= :never (:t input)) t/never out))

      :else
      (do (infer ctx xf)
          (unchecked-hof! ctx xf "into: transducer input and output types not checked" :unsupported-function)
          nil))))

(defn- into-transformed [ctx target out]
  (if (= t/never out)
    (dissoc target :fresh)
    (let [out (widen (dissoc out :fresh))]
      (apply t/union
             (for [m (t/members (:env ctx) target)]
               (case (:t m)
                 :coll (update (dissoc m :fresh) :of t/union out)
                 (:map :map-of) (t/prim :map)
                 :nil (t/union t/nil-t (coll-of :seq out))
                 :prim (if (#{:vector :set :seq :map :coll} (:name m)) m t/unknown)
                 :never t/never
                 :any t/any
                 t/unknown))))))

(defmethod special 'into [ctx node _]
  (let [[to second-arg from :as args] (args-of node)]
    (case (count args)
      0 (coll-of :vector t/never)
      1 (infer ctx to)
      2 (let [target (infer ctx to)
              el (elem-type ctx (infer ctx second-arg))]
          (cond (and (= :vector (:op to)) (empty? (:children to))) (coll-of :vector el)
                (and (= :set (:op to)) (empty? (:children to))) (coll-of :set el)
                :else (dissoc target :fresh)))
      3 (let [target (infer ctx to)
              source (infer ctx from)
              out (or (apply-transducer ctx second-arg (transducer-element ctx (eff ctx source))) t/unknown)]
          (when-let [bad (unsupported-sequence-member ctx source)]
            (report! ctx from "TSP2345" (str "into cannot reduce " (t/show bad))))
          (if (= t/never source) t/never (into-transformed ctx target out)))
      (do (run! #(infer ctx %) args)
          (report! ctx node "TSP2554" "into expects zero to three arguments")
          t/unknown))))

(defmethod special 'new [ctx node _]
  (let [[cls & args] (args-of node)]
    (run! #(infer ctx %) args)
    (if-let [c (and (= :sym (:op cls)) (t/resolve-class (:imports (:file ctx)) (:val cls)))]
      ;; A concrete Object constructor is not the declared Object/top escape.
      (if (= c "java.lang.Object") {:t :class :name c} (t/class-type c))
      t/unknown)))

;; ---- defn ----

(defn- check-return! [ctx node actual expected]
  (when-not (or (:generic-failure actual) (#{:unknown :any} (:t expected)))
    (let [env (:env ctx) actual (eff ctx actual)
          result (t/sub-result env actual expected)
          emit (fn [[c m extra]] (apply report! ctx node c m (mapcat identity extra)))]
      (cond
        (= :proved (:status result)) (run! emit (problems ctx node actual expected))
        (seq (fresh-problems ctx actual expected)) (run! emit (fresh-problems ctx actual expected))
        (= :inconclusive (:status result)) (emit (incomplete-problem ctx actual expected result))
        (and (t/nilable? env actual) (not (t/nilable? env expected)) (t/sub? env (t/remove-nil env actual) expected))
        (report! ctx node "TSP2322"
                 (str "may return nil, but the signature says " (show-ref ctx expected)
                      (when-let [w (and (= :sym (:op node)) (get-in ctx [:why (:val node)]))] (str ": " w))))
        :else (if-let [[k opt?] (missing-req ctx actual expected)]
                (report! ctx node "TSP2741" (str "the returned map has no required key " k " of " (t/show expected)
                                                 (when opt? " (it is optional in the returned map)")))
                (report! ctx node "TSP2322" (str "returns " (show-ref ctx actual) ", but the signature says " (show-ref ctx expected))))))))

(defn- check-defn [ctx node]
  (let [file (:file ctx) env (:env ctx)
        parts (env/defn-parts node)
        nm (:name parts)
        sym (symbol (str (:ns file)) (str (:val nm)))
        v (get-in env [:vars sym])
        ftype (if (= :all (:t (:type v))) (solver/open-rigid (:type v)) (:type v))]
    (when-let [tn (env/tag-node nm)]
      (when (t/primitives (str (:val tn)))
        (report! ctx tn "TSP7021"
                 (str "a hint on the name is not a return type: write (defn " (:val nm) " ^" (:val tn) " [...] ...)"))))
    (when (and (:strict (:opts ctx)) (not (:private? parts)) (= :hints (:source v))
               (some (fn [a] (or (= :unknown (:t (:ret a))) (some #(= :unknown (:t %)) (:params a)))) (:arities ftype)))
      (report! ctx nm "TSP7010" (str "public function " (:val nm) " has no signature: add s/fdef, {:t/sig '[...]} or type hints")))
    (doseq [{:keys [params body]} (:arities parts)
            :let [{:keys [fixed rest]} (env/split-params params)
                  declared (or (first (filter #(and (= (count fixed) (count (:params %)))
                                                    (= (boolean rest) (boolean (or (:rest %) (:kwargs %)))))
                                              (:arities ftype)))
                               (first (filter #(= (count fixed) (count (:params %))) (:arities ftype))))]]
      (if (nil? declared)
        (report! ctx params "TSP2790" (str "the signature of " (:val nm) " has no arity with " (count fixed) " parameter(s)"))
        (let [prims (filter #(some-> (env/tag-node %) :val str t/primitives) fixed)
              _ (when (and (seq prims) (> (count fixed) 4))
                  (report! ctx params "TSP7025" "functions taking primitives support only 4 or fewer parameters"))
              ctx' (reduce (fn [c [p ty]]
                             (let [c' (bind-pattern c p ty)]
                               (record! ctx p ty)
                               (if (= :sym (:op p)) (update c' :params (fnil conj #{}) (:val p)) c')))
                           (dissoc ctx :recur-types :load-time?) (map vector fixed (:params declared)))
              ctx' (cond (nil? rest) ctx'
                         (:kwargs declared) (bind-pattern ctx' rest (:kwargs declared))
                         (:rest declared) (bind-pattern ctx' rest (t/union {:t :coll :kind :seq :of (:rest declared)} t/nil-t))
                         :else (bind-pattern ctx' rest t/unknown))
              ret-hint (env/hint-type file params)]
          (when (and ret-hint (not (#{:unknown :any} (:t (:ret declared))))
                     (not (t/sub? env ret-hint (:ret declared))) (not (t/sub? env (:ret declared) ret-hint)))
            (report! ctx (env/tag-node params) "TSP2322"
                     (str "hint " (t/show ret-hint) " contradicts the signature: " (t/show (:ret declared)))))
          (let [rty (infer-body ctx' body)]
            (when (seq body) (check-return! ctx' (last body) rty (:ret declared)))))))
    t/unknown))

(defmethod special 'defn [ctx node _] (check-defn ctx node))
(defmethod special 'defn- [ctx node _] (check-defn ctx node))

(defn- guard-claim [env entry]
  (when-let [node (env/attr-get (get-in entry [:parts :attr]) :t/guard)]
    (let [raw (parse/node->data node)
          data (if (and (seq? raw) (= 'quote (first raw))) (second raw) raw)
          {:keys [type mode two-sided] :as contract}
          (if (map? data) data {:type data :mode :trusted})
          invalid (volatile! false)
          fail! (fn [& _] (vreset! invalid true))
          type-ctx (assoc (env/type-ctx (:pfile entry) fail!) :operator-error fail!)
          target (operators/normalize env (t/parse-type type-ctx type) fail!)
          valid? (and (#{:checked :trusted} mode) (contains? contract :type) (not @invalid)
                      (every? #{:type :mode :two-sided} (keys contract))
                      (or (nil? two-sided) (boolean? two-sided))
                      (not (and two-sided (= :trusted mode))))]
      {:node node :target target :mode mode :two-sided two-sided :valid? valid?})))

(defn- dynamic-definition? [entry]
  (or (some-> (env/attr-get (get-in entry [:parts :attr]) :dynamic) parse/node->data)
      (some (fn [node]
              (let [form (parse/node->data node)]
                (or (= :dynamic form) (and (map? form) (:dynamic form)))))
            (get-in entry [:parts :name :meta]))))

(defn- guard-candidate [env [sym entry]]
  (when-let [definition (:definition entry)]
    (let [claim (guard-claim env entry)
          parts (get-in entry [:parts :arities])
          body (:body (first parts))
          {:keys [fixed rest]} (env/split-params (:params (first parts)))
          arities (get-in entry [:type :arities])
          param (first fixed)
          unary? (and (= 1 (count parts) (count fixed) (count arities))
                      (= 1 (count (:params (first arities))))
                      (nil? rest) (= :sym (:op param))
                      (not (:rest (first arities))) (not (:kwargs (first arities))))
          stable? (and (= 1 (get-in env [:root-definitions sym]))
                       (not (dynamic-definition? entry)))
          domain (guards/input-type (get-in arities [0 :params 0]))
          domain (if (#{:any :unknown} (:t (t/resolve-t env domain))) t/unknown domain)
          ctx {:env env :file (:pfile entry) :locals {}}
          plan (when (and unary? stable?) (guards/compile-body (guard-compiler ctx) [(:val param)] body))]
      (when (or claim (guards/useful? plan))
        [sym {:claim claim :entry entry :definition definition :ctx ctx
              :unary? unary? :stable? stable? :domain domain :plan plan}]))))

(defn- compile-guards [env opts]
  (let [candidates (into {} (keep #(guard-candidate env %)) (:vars env))
        solved (atom {})
        summaries (atom {})
        diagnostics (atom [])]
    (letfn [(finish [sym result]
              (swap! solved assoc sym result)
              result)
            (reject! [candidate code message reason]
              (when-let [node (get-in candidate [:claim :node])]
                (swap! diagnostics conj {:code code :severity :error :file (get-in candidate [:entry :file])
                                         :pos (:pos node) :message message :reasons #{reason}})))
            (body-valid? [{:keys [ctx definition]}]
              (binding [*diags* (atom []) *types* nil *ignores* (atom []) *rets* (atom {})
                        *trust* (atom {:cast 0 :any 0 :ignore 0})]
                (let [checked (into {} (filter #(= :checked (:mode (val %)))) @summaries)]
                  (check-defn (-> ctx (assoc :opts (assoc opts :strict false)) (assoc-in [:env :guards] checked)) definition)
                  (not-any? #(= :error (:severity %)) @*diags*))))
            (visit [sym path]
              (cond
                (path sym) :recursive
                (>= (count path) 64) :limit
                (contains? @solved sym) (get @solved sym)
                :else
                (when-let [{:keys [claim unary? stable? domain plan] :as candidate} (get candidates sym)]
                  (let [{:keys [valid? mode target two-sided]} claim
                        deps (when (and plan (not= :trusted mode)) (guards/dependencies plan))
                        dependency-error (some #{:recursive :limit} (map #(visit % (conj path sym)) (sort deps)))
                        proof-env (assoc env :guards @summaries)]
                    (cond
                      (or (and claim (not valid?)) (not unary?))
                      (do (reject! candidate "TSP250-03" "invalid guard declaration or unsupported parameter list" :invalid-guard)
                          (finish sym :invalid))

                      (not stable?)
                      (do (reject! candidate "TSP250-03" "guard Var can have a different definition" :unstable-var)
                          (finish sym :invalid))

                      (= :trusted mode)
                      (do (swap! summaries assoc sym {:mode :trusted :domain domain :target target})
                          (finish sym :trusted))

                      (or dependency-error (not (guards/useful? plan)))
                      (do (reject! candidate "TSP250-03" "guard body cannot provide checked facts"
                                   (or dependency-error :unsupported-body))
                          (finish sym (or dependency-error :unsupported)))

                      (not (body-valid? candidate))
                      (do (reject! candidate "TSP250-03" "guard body has type errors" :body-errors)
                          (finish sym :invalid))

                      :else
                      (let [[yes no] (guards/split proof-env plan domain)]
                        (cond
                          (and claim (or (= t/never yes) (not (guards/proves? proof-env yes target))))
                          (do (reject! candidate "TSP250-01" (str "truthy result does not prove " (t/show target)) :truthy)
                              (finish sym :invalid))

                          (and two-sided (not= t/never no)
                               (not= t/never (second (guards/split proof-env plan target))))
                          (do (reject! candidate "TSP250-02" (str "falsy result does not exclude " (t/show target)) :falsy)
                              (finish sym :invalid))

                          :else
                          (do (swap! summaries assoc sym (cond-> {:mode :checked :domain domain :plan plan
                                                                 :truthy yes :falsy no}
                                                          claim (assoc :target target)))
                              (finish sym :checked)))))))))]
      (run! #(visit % #{}) (sort (keys candidates)))
      {:guards @summaries :diagnostics @diagnostics})))

;; ---- infer ----

(defn- parse-directive-type [ctx d]
  (let [form (parse/node->data (second (:children d)))]
    (t/parse-type (env/type-ctx (:file ctx) (fn [f] (report! ctx d "TSP9001" (str "not a type: " (pr-str f))))) form)))

(defn- directive-head [directive]
  (:val (first (:children directive))))

(def ^:private literal-directive? (comp '#{t/const t/satisfies} directive-head))

(defn- literal-directives [ctx node]
  (when-let [directives (seq (filter literal-directive? (:directives node)))]
    (let [groups (group-by directive-head directives)
          conflict (or (some #(#{'t/cast 't/any} (directive-head %)) (:directives node))
                       (env/tag-node node))]
      (into {}
            (keep (fn [d]
                    (let [h (directive-head d)]
                      (if-let [message (cond
                                         (:missing-directive-target node) "missing expression"
                                         (next (groups h)) "duplicate directive on one expression"
                                         conflict "cannot combine with t/cast, t/any or a type hint on the same expression"
                                         (not= (count (:children d)) (if (= h 't/const) 1 2))
                                         (if (= h 't/const) "expects no arguments" "expects exactly one type"))]
                        (do (report! ctx d "TSP280-02" (str h ": " message)) nil)
                        [h d]))))
            directives))))

(defn- check-satisfies! [ctx node directive actual before-trust]
  (let [annotation (second (:children directive))
        env (:env ctx)
        {:keys [type reason]} (literals/parse-contract env (:file ctx) (parse/node->data annotation true))
        escape (some #(when (> (get @*trust* % 0) (get before-trust % 0)) %) [:cast :any])]
    (if reason
      (report! ctx annotation "TSP280-01" (str "t/satisfies cannot check " (parse/unparse annotation)
                                             " (" (name reason) ")")
               :reasons #{reason})
      (if-let [issue (or escape (literals/evidence-issue env actual))]
        (report! ctx node "TSP280-01" (str "t/satisfies cannot establish " (show-ref ctx type)
                                         " (" (name issue) ")")
                 :reasons #{issue})
        (let [actual (if (:strict-nil (:opts ctx))
                       (walk/postwalk #(if (and (map? %) (:platform %)) (eff ctx %) %) actual)
                       actual)
              fields (when (= :map (:op node))
                       (into {} (map (fn [[k v]] [(:val k) {:key k :value v}]))
                             (partition 2 (:children node))))]
          (binding [t/*strict-shapes* true]
            (doseq [[code message details [role field]] (problems ctx node actual type)]
              (apply report! ctx (or (get-in fields [field role]) node) code message
                     (mapcat identity (cond-> details (= "TSP210-01" code) (assoc :severity :error)))))))))))

(defn- check-macro-body [ctx node contract]
  (if-not (macros/valid-bindings? node contract)
    (report! ctx node "TSP7032"
             "macro contract requires a first-argument vector of distinct simple symbols matching :bindings; body not checked")
    (let [[_ bindings & body] (:children node)
          body-ctx (reduce (fn [c [p ty]]
                             (record! ctx p ty)
                             (bind-pattern c p ty))
                           ctx (map vector (:children bindings) (:bindings contract)))
          actual (infer-body body-ctx body)]
      (check-return! body-ctx (or (last body) node) actual (:ret contract))))
  ;; A trusted body contract says nothing about the expansion's expression type.
  t/unknown)

(defn- check-unknown-call [ctx args]
  (let [binding-vector (first args)
        bindings (:children binding-vector)
        possible? (and (= :vector (:op binding-vector)) (seq bindings)
                       (every? #(and (= :sym (:op %)) (simple-symbol? (:val %))) bindings))
        ;; The call may be a foreign binding macro. Without a contract these
        ;; names could be locals, so their uses cannot prove a read of a Var.
        ctx (cond-> ctx possible? (update :possible-bindings (fnil into #{}) (map :val bindings)))]
    (run! #(infer ctx %) args)
    t/unknown))

(defn- infer-list [ctx node]
  (let [h (first (:children node))
        args (args-of node)
        file (:file ctx)]
    (when (and (#{:sym :kw} (:op h))
               (some literal-directive? (:directives h)))
      (infer ctx h))
    (cond
      (nil? h) {:t :coll :kind :seq :of t/never}
      (= :kw (:op h)) (let [[m default] args
                            ty (infer ctx m)]
                        (if default
                          (read-key-default ctx ty (:val h) h (infer ctx default))
                          (read-key ctx ty (:val h) h)))
      (not= :sym (:op h))
      (let [ft (t/resolve-t (:env ctx) (eff ctx (infer ctx h)))]
        (if (#{:fn :all} (:t ft))
          (call-fn ctx node (parse/unparse h) ft args)
          (do (run! #(infer ctx %) args) t/unknown)))
      (contains? (:locals ctx) (:val h))
      (if-let [callback (deferred-callback ctx h)]
        (let [types (mapv #(infer ctx %) args)
              params (mapv #(widen (dissoc % :fresh)) types)
              ft (force-callback ctx callback {:t :fn :arities [{:params params :ret t/unknown}]})]
          (apply-type ctx ft types node (str (:val h))))
        (let [local (get-in ctx [:locals (:val h)])
              ft (if (= :ref (:t local)) (t/resolve-t (:env ctx) local) local)]
          (if (or (= :all (:t ft)) (seq (:arities ft)))
            (call-fn ctx node (str (:val h)) ft args)
            (do (run! #(infer ctx %) args) t/unknown))))
      :else
      (let [sym (:val h) n (name sym) head (env/head file node)
            own? (and (nil? (namespace (env/resolve-sym file sym)))
                      (contains? (get-in (:env ctx) [:ns-defs (:ns file)]) head))
            core-sym (if (namespace head) head (symbol "clojure.core" (name head)))
            extern? (= :extern (get-in ctx [:env :vars core-sym :source]))
            has-rule? (and (not own?) (not extern?) (not= (get-method special head) (get-method special ::none)))
            ;; An unqualified, unreferred head names a contract only when this
            ;; namespace itself defines that macro: never a function or core form.
            local-sym (symbol (str (:ns file)) (name head))
            contract-sym (cond (namespace head) head
                               (contains? (:macros (:env ctx)) local-sym) local-sym)
            contract (when contract-sym (get-in ctx [:env :macro-contracts contract-sym]))
            r (cond contract (check-macro-body ctx node contract)
                    has-rule? (special ctx node head)
                    ;; a macro of Clojure itself without a rule yet: its body is not checked
                    (and (not own?) (not extern?) (core/stdlib-macro? core-sym)) t/unknown
                    :else ::none)]
        (cond
          (not= ::none r) r
          (and (nil? (namespace sym)) (str/starts-with? n ".-")) (do (run! #(infer ctx %) args) t/unknown)
          (and (nil? (namespace sym)) (str/starts-with? n ".") (> (count n) 1) (not= ".." n)) (method-call ctx node (subs n 1))
          (and (nil? (namespace sym)) (str/ends-with? n ".") (> (count n) 1))
          (do (run! #(infer ctx %) args)
              (if-let [c (t/resolve-class (:imports file) (symbol (subs n 0 (dec (count n)))))]
                (if (= c "java.lang.Object") {:t :class :name c} (t/class-type c)) t/unknown))
          (java-static? ctx sym) (java-call ctx node (java-static? ctx sym) n true args)
          (contains? (:macros (:env ctx)) (:sym (resolve-var (assoc ctx :quiet true) sym h)))
          (do (report! ctx h "TSP7031" (str "macro " sym " is not described: its body is not checked") :severity :warning)
              t/unknown)
          :else
          (let [{:keys [type]} (read-var ctx sym h)]
            (if type
              (call-fn ctx node (str sym) type args)
              (check-unknown-call ctx args))))))))

(defn- literal-elements [ctx node]
  (apply t/union
         (map (fn [child]
                (let [ty (cond-> (infer ctx child) (= :vector (:op node)) (dissoc :fresh))]
                  (if (:preserve-literals? ctx) ty (widen ty))))
              (:children node))))

(defn- infer* [ctx node]
  (case (:op node)
    :str (t/lit (:val node))
    :char (t/class-type "java.lang.Character")
    :num (t/lit (:val node))
    :kw (t/lit (:val node))
    :bool (t/lit (:val node))
    :nil t/nil-t
    :var (var-reference ctx (:children node))
    :regex (t/class-type "java.util.regex.Pattern")
    :sym (let [s (:val node)]
           (cond (contains? (:locals ctx) s) (get-in ctx [:locals s])
                 (java-static? ctx s) t/unknown
                 (and (nil? (namespace s)) (not (t/primitives (name s)))
                      (t/resolve-class (:imports (:file ctx)) s)) t/unknown
                 :else (or (:type (read-var ctx s node)) t/unknown)))
    :vector {:t :coll :kind :vector :of (literal-elements ctx node)}
    :set {:t :coll :kind :set :of (literal-elements ctx node)}
    :map (let [children (:children node)
               pairs (partition 2 children)]
           (when (:missing-directive-target (peek children))
             (literal-directives ctx (peek children)))
           (if (every? #(= :kw (:op (first %))) pairs)
             {:t :map :fresh true :inferred true :complete-keys? true :opt {}
              :req (into (array-map)
                         (for [[k v] pairs]
                           (let [_ (when (some literal-directive? (:directives k)) (infer ctx k))
                                 vty (infer ctx v)
                                 kk (:val k)]
                             (check-key-value! ctx kk v vty)
                             (when (and (qualified-keyword? kk) (not (get-in (:env ctx) [:specs kk]))
                                        (contains? (:key-nss (:env ctx)) (namespace kk))
                                        (or (:strict (:opts ctx)) (similar kk (keys (:specs (:env ctx))))))
                               (report! ctx k "TSP2304" (str "no spec for " kk (hint-suffix kk (keys (:specs (:env ctx)))))))
                             ;; a wrong value is reported once; after that the key has the type of its spec
                             [kk (if (and (qualified-keyword? kk) (get-in (:env ctx) [:specs kk])
                                          (not (t/sub? (:env ctx) (eff ctx vty) (t/ref-t kk))))
                                   (t/ref-t kk)
                                   (dissoc vty :fresh))])))}
             (do (run! #(infer ctx %) (:children node)) (t/prim :map))))
    :quote (if (= :sym (:op (first (:children node)))) (t/prim :symbol) t/unknown)
    :fn-lit (infer-callback ctx node nil)
    :list (infer-list ctx node)
    t/unknown))

(defn infer
  ([ctx node] (infer ctx node nil))
  ([ctx node callback-arity]
   (let [dirs (:directives node)
         find-d (fn [h] (first (filter #(= h (directive-head %)) dirs)))
         literal-dirs (literal-directives ctx node)
         satisfies (get literal-dirs 't/satisfies)
         before-trust (when satisfies @*trust*)
         ctx (if (and (#{:map :vector :set} (:op node))
                      (or (:preserve-literals? ctx) (get literal-dirs 't/const)))
               (assoc ctx :preserve-literals? true)
               (dissoc ctx :preserve-literals?))
         ty (if-let [callback (deferred-callback ctx node)]
              (force-callback ctx callback callback-arity)
              (if callback-arity (infer-callback ctx node callback-arity) (infer* ctx node)))
         ty (if-let [h (and (not= :sym (:op node)) (env/hint-type (:file ctx) node))]
              (do (check-hint! ctx node ty "type of the expression")
                  (if-let [why (:null-return (meta ty))]
                    (with-meta (t/union (dissoc h :platform) t/nil-t) {:null-return why})
                    h))
              ty)
         ty (cond
              (find-d 't/cast) (do (swap! *trust* update :cast inc)
                                   (t/with-assumption (parse-directive-type ctx (find-d 't/cast)) :cast))
              (find-d 't/any) (do (swap! *trust* update :any inc) (t/with-assumption t/any :any))
              :else ty)]
     (when satisfies (check-satisfies! ctx node satisfies ty before-trust))
     (record! ctx node ty))))

;; ---- entry points ----

(def ^:private known-diagnostic-codes
  #{"TSP250-01" "TSP250-02" "TSP250-03" "TSP270-01" "TSP270-02" "TSP270-03" "TSP280-01" "TSP280-02"
    "TSP2304" "TSP2305" "TSP2322" "TSP2339" "TSP2345" "TSP2353"
    "TSP2531" "TSP2554" "TSP2578" "TSP2678" "TSP2741" "TSP2790"
    "TSP7010" "TSP7020" "TSP7021" "TSP7023" "TSP7025" "TSP7031" "TSP7032" "TSP721-01"
    "TSP8001" "TSP9001" "TSP9002" "TSP9003" "TSP9004" "TSP830-01" "TSP210-01"})

(defn- directive-children
  "Directive scope follows source code, not quoted data or opaque declarations.
  Keep directives on the boundary form itself, but do not inspect its contents.
  Ordinary collection expressions and function binding forms remain traversable."
  [file node]
  (when-not (or (#{:quote :var} (:op node))
                (and (= :list (:op node))
                     (contains? '#{quote ns comment declare require import in-ns defmacro
                                   defprotocol defrecord deftype defmulti definterface set! var
                                   clojure.spec.alpha/def clojure.spec.alpha/fdef}
                                (env/head file node))))
    (:children node)))

(defn- collect-ignores
  "Collect once from source AST, excluding data and opaque declaration contents.
  Invalid directives never enter matching and cannot themselves be ignored."
  [files]
  (reduce
    (fn [acc [file node d]]
      (let [args (rest (:children d))
            broad? (and (= 1 (count args)) (= :str (:op (first args))))
            code-node (when-not broad? (first args))
            code (when (= :sym (:op code-node)) (str (:val code-node)))
            reason (if broad? (first args) (second args))
            problem (cond
                      (:missing-directive-target node) "t/ignore has no following form"
                      (not (or broad? (= 2 (count args)))) "t/ignore requires [CODE] and a nonblank string reason"
                      (or (not= :str (:op reason)) (str/blank? (:val reason))) "t/ignore requires a nonblank string reason"
                      (and (not broad?) (not (known-diagnostic-codes code))) (str "unknown t/ignore code: " (parse/unparse code-node)))
            entry {:file file :pos (:pos node) :at (:pos d) :code code}]
        (-> acc
            (update :count inc)
            (update (if problem :invalid :valid) conj
                    (if problem
                      {:code "TSP830-01" :severity :error :file file :pos (:pos d) :message problem}
                      entry)))))
    {:count 0 :valid [] :invalid []}
    (for [f files form (:forms f)
          node (tree-seq #(seq (directive-children f %)) #(directive-children f %) form)
          d (:directives node)
          :when (= 't/ignore (:val (first (:children d))))]
      [(:file f) node d])))

(defn- inside? [pos outer]
  (let [start (juxt :row :col) end (juxt :end-row :end-col)]
    (and (every? number? (concat (start pos) (end pos) (start outer) (end outer)))
         (not (neg? (compare (start pos) (start outer))))
         (neg? (compare (start pos) (end outer)))
         (not (pos? (compare (end pos) (end outer)))))))

(defn- diag-id [d]
  (let [identity [(:code d) (str (:file d))
                  (mapv #(get (:pos d) %) [:row :col :end-row :end-col])
                  (:message d)]
        digest (.digest (java.security.MessageDigest/getInstance "SHA-256")
                        (.getBytes (pr-str identity) java.nio.charset.StandardCharsets/UTF_8))]
    ;; 128 bits of SHA-256: stable across runs, with practical collision resistance.
    (apply str (map #(format "%02x" (bit-and 0xff %)) (take 16 digest)))))

(defn check-files
  "Checks parsed files. Returns {:diagnostics [...] :types [...] :trust {...} :env env}."
  [files opts]
  (let [[env0 env-diags] (env/build files (:extern-contracts opts))
        compiled (macros/compile-contracts
                  (get opts :macro-contracts {}) env0
                  #(not= (get-method special %) (get-method special ::none)))
        env-diags (concat env-diags (:diagnostics compiled))
        guard-env (merge env0
                         (env/namespace-vars files (:macros env0))
                         {:macro-contracts (:contracts compiled)
                          :nss (set (map :ns files))})
        guard-result (compile-guards guard-env opts)
        env (assoc guard-env :guards (:guards guard-result))
        guard-trust (count (filter #(= :trusted (:mode %)) (vals (:guards env))))]
    (binding [*diags* (atom (vec (map #(merge {:severity :error} %) (concat env-diags (:diagnostics guard-result)))))
              *types* (atom [])
              *ignores* (atom [])
              *trust* (atom (cond-> {:cast 0 :any 0 :ignore 0} (pos? guard-trust) (assoc :guard guard-trust)))
              *rets* (atom {})]
      (doseq [f files]
        (binding [t/*cur-ns* (:ns f)]
          (let [ctx {:env env :file f :opts opts :locals {} :load-time? true}]
            (doseq [form (:forms f)] (infer ctx form)))))
      (let [{:keys [valid invalid count]} (collect-ignores files)
            ;; Inner ranges win; equal ranges retain source directive order.
            ignores (sort-by (fn [i] [(- (or (:row (:pos i)) 0)) (- (or (:col (:pos i)) 0))
                                     (:end-row (:pos i)) (:end-col (:pos i))]) valid)
            _ (swap! *trust* assoc :ignore count)
            hit (fn [d] (first (filter #(and (= (:file %) (:file d))
                                             (or (nil? (:code %)) (= (:code %) (:code d)))
                                             (inside? (:pos d) (:pos %))) ignores)))
            all (distinct @*diags*)
            used (set (keep hit all))
            unused (for [i ignores :when (not (used i))]
                     {:code "TSP2578" :severity :warning :file (:file i) :pos (:at i) :message "unused t/ignore: nothing to ignore here"})
            levels (:levels opts)
            ds (->> (concat (remove hit all) invalid unused)
                    (map #(if-let [l (get levels (symbol (:code %)))] (assoc % :severity l) %))
                    (remove #(= :off (:severity %)))
                    (map #(assoc % :id (diag-id %)))
                    (sort-by (juxt :file #(:row (:pos %)) #(:col (:pos %)) :code)))]
        {:diagnostics (vec ds) :types @*types* :trust @*trust* :env env
         :sigs (into {} (for [[sym v] (:vars env)]
                          [sym (var-type {:env env :opts opts} sym)]))}))))
