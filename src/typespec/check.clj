(ns typespec.check
  "The checker: walks forms, infers types, reports diagnostics."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [typespec.parse :as parse]
            [typespec.types :as t]
            [typespec.length :as length]
            [typespec.literals :as literals]
            [typespec.type-operators :as operators]
            [typespec.spec-conform :as conform]
            [typespec.solver :as solver]
            [typespec.env :as env]
            [typespec.java :as java]
            [typespec.core-types :as core]
            [typespec.guards :as guards]
            [typespec.macro-contracts :as macros]))

(def ^:dynamic *diags* nil)
(def ^:dynamic *types* nil)
(def ^:dynamic *var-uses* nil)
(def ^:dynamic *spec-uses* nil)
(def ^:dynamic *ignores* nil)
(def ^:dynamic *trust* nil)
(def ^:dynamic *rets* nil)

(declare infer infer-body bind-pattern apply-fn-node why-of var-type infer-fn narrow record! force-callback
         assoc-map-key specialize-map-return dynamic-definition?)

(defn- static-map-key [node]
  (case (:op node)
    (:kw :str) (:val node)
    :quote (let [value (first (:children node))]
             (when (= :sym (:op value)) (:val value)))
    :list (let [[head value & more] (:children node)]
            (when (and (= 'quote (:val head)) (= :sym (:op value)) (empty? more))
              (:val value)))
    nil))

(defn- at-type-path [node path]
  (reduce (fn [current step]
            (if-let [child (case (:op current)
                             :map (some (fn [[k v]] (when (= step (static-map-key k)) v))
                                        (partition 2 (:children current)))
                             :vector (when (integer? step) (get (:children current) step))
                             nil)]
              child
              (reduced current)))
          node path))

(defn- report! [ctx node code msg & {:as extra}]
  (when-not (:quiet ctx)
    (let [at (at-type-path node (:type-path extra))]
      (swap! *diags* conj (merge {:code code :severity :error :file (:file (:file ctx)) :pos (:pos at) :message msg} extra))))
  nil)

(defn- eff
  "With :strict-nil, a platform type (from a hint or from Java) may be nil."
  [ctx ty]
  (if (:strict-nil (:opts ctx)) (t/allow-platform-nil ty) ty))

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
         (filter (fn [[d c]]
                   (<= d (max 1 (min 2 (quot (count (if (string? c) c (name c))) 2))))))
         (sort-by first) first second)))

(defn- hint-suffix [x candidates]
  (if-let [c (similar x candidates)] (str ". Did you mean " c "?") ""))

;; ---- maps and keys ----

(defn- map-members [ctx ty]
  (filter #(= :map (:t %)) (t/members (:env ctx) ty)))

(defn- map-of-entry [env ty k]
  (when-not (= :mismatch (:status (t/sub-result env (t/lit k) (:k ty))))
    (:v ty)))

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
                :map-of (if-let [value (map-of-entry env m k)] [:opt value] [:absent nil])
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
      :nil {:type t/never :optional? true :declared? true}
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
    (if optional? (t/union type default-type) type)))

;; ---- assignability ----

(defn- incomplete-problem [ctx actual expected result]
  ["TSP210-01"
   (str "compatibility not checked: " (show-ref ctx actual) " to " (show-ref ctx expected)
        " (" (str/join ", " (sort (map name (:reasons result)))) ")")
   {:severity (if (:strict (:opts ctx)) :error :warning)
    :reasons (:reasons result)}])

(defn- length-message [actual expected]
  (str "expected collection length " (length/describe expected) ", got " (length/describe actual)))

(defn- nested-type-problem [ctx code {:keys [path missing-key actual expected length?]}]
  (when (seq path)
    [(cond missing-key "TSP2741" length? "TSP330-01" :else code)
     (str "at " (pr-str path) ": "
          (cond missing-key (str "required key " missing-key " is missing")
                length? (length-message actual expected)
                :else (str (show-ref ctx actual) " is not assignable to " (show-ref ctx expected))))
     {:type-path path}]))

(defn- nested-platform-problem [ctx code actual expected result]
  (when-let [{:keys [path platform-origin expected]}
             (t/nested-platform-mismatch (:env ctx) actual expected result)]
    [code (str "at " (pr-str path) ": " (show-ref ctx platform-origin)
               " may be nil, expected " (show-ref ctx expected))
     {:type-path path}]))

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
             ["TSP2322" (str "value of " k ": " (t/show (eff ctx v)) " is not assignable to " (show-ref ctx exp)) nil [:value k]]))
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

(defn- collection-problem [ctx actual expected]
  (let [env (:env ctx)
        am (t/members env actual)
        em (t/members env expected)
        source (first am)
        target (first em)
        a (:items source)
        e (:items target)]
    (when (= 1 (count am) (count em))
      (cond
        (and a e (= :vector (:kind source) (:kind target)))
        (if (not= (count a) (count e))
          ["TSP330-01" (str "expected vector length " (count e) ", got " (count a))]
          (first (keep-indexed (fn [i item]
                                (let [target (nth e i)]
                                  (when (= :mismatch (:status (t/sub-result env item target)))
                                    ["TSP2345" (str "vector element " i ": " (show-ref ctx (eff ctx item))
                                                     " is not assignable to " (show-ref ctx target))])))
                              a)))

        (and (= :coll (:t source) (:t target))
             (or (nil? (:kind target)) (= (:kind source) (:kind target)))
             (or (length/constrained? target) (and e (length/constrained? source)))
             (not (length/subset? source target)))
        ["TSP330-01" (length-message source target)]))))

(defn- problems
  "Why a value of type `actual` (written as `node`) cannot be used as `expected`. Empty when it can."
  [ctx node actual expected]
  (let [env (:env ctx) actual (eff ctx actual)
        result (t/sub-result env actual expected)
        nested-error (when (= :mismatch (:status result))
                       (or (nested-platform-problem ctx "TSP2531" actual expected result)
                           (nested-type-problem ctx (if (:fresh actual) "TSP2322" "TSP2345") result)))
        collection-error (when (= :mismatch (:status result)) (collection-problem ctx actual expected))]
    (cond
      (:generic-failure actual) []
      (= :proved (:status result)) (if (:fresh actual) (filter #(= "TSP2353" (first %)) (fresh-problems ctx actual expected)) [])
      nested-error [nested-error]
      (seq (fresh-problems ctx actual expected)) (fresh-problems ctx actual expected)
      (= :inconclusive (:status result)) [(incomplete-problem ctx actual expected result)]
      collection-error [collection-error]
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
        core-sym (when head (if (namespace head) head (symbol "clojure.core" (name head))))]
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

(declare projected-type)

(defn- map-entry-type? [ctx ty]
  (every? :map-entry? (t/members (:env ctx) ty)))

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
                            [a (mapv :val (:children b))]
                            (and (#{'first 'second 'key 'val} h) a (nil? b))
                            [a [(if (#{'first 'key} h) 0 1)]]
                            (and (= 'nth h) (= :num (:op b)) (integer? (:val b))
                                 (<= 0 (:val b) Integer/MAX_VALUE) (empty? more)
                                 (empty? (:directives b)) (not (env/tag-node b)))
                            [a [(int (:val b))]])]
          (when-let [p (and base (stable-path ctx base))]
            (when (or (not (integer? (first path)))
                      (let [base-type (projected-type ctx (:type p) (:path p))]
                        (and (or (not (#{'key 'val} h)) (map-entry-type? ctx base-type))
                             (projected-type ctx base-type path))))
              (update p :path into path))))))))

(defn- track-path [ctx {:keys [local origin path type]}]
  (if origin
    [ctx origin path]
    (let [origin (Object.)
          ctx (assoc-in ctx [:origins origin] type)]
      [(cond-> ctx local (remember-projection local origin [])) origin path])))

(defn- projected-type [ctx ty path]
  (reduce (fn [current key]
            (if (integer? key)
              (let [positions (map #(get (:items %) key) (t/members (:env ctx) current))]
                (if (every? some? positions) (apply t/union positions) (reduced nil)))
              (let [{:keys [type status]} (lookup-key ctx current key)]
                (case status
                  :req (if (t/nilable? (:env ctx) current) (t/union type t/nil-t) type)
                  :opt (t/union type t/nil-t)
                  :nil t/nil-t
                  (reduced nil)))))
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
  "Pairs of [leaf type, rebuild]. Each rebuild retains its enclosing map or tuple."
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
                                     (integer? key)
                                     (when-let [field (get (:items m) key)]
                                       (when-let [children (walk field more)]
                                         (for [[leaf rebuild] children]
                                           [leaf (fn [new-leaf]
                                                   (let [items (assoc (:items m) key (rebuild new-leaf))]
                                                     (assoc m :items items :of (apply t/union items))))])))

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

(defn- literal-map-key-node? [node]
  (and (#{:kw :str} (:op node)) (literal-node? node)))

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

(defn- count-parts [size member]
  (case (:t member)
    :never [nil nil]
    :nil (if (zero? size) [member nil] [nil member])
    :coll
    (let [[lo hi] (length/bounds member)]
      (if (or (< size lo) (and hi (> size hi)))
        [nil member]
        (let [shorter (when (< lo size) (length/with-bounds member lo (dec' size)))
              longer (when (or (nil? hi) (< size hi)) (length/with-bounds member (inc' size) hi))]
          [(if (= lo hi) member (length/with-bounds member size size))
           (if (and shorter longer) (t/union shorter longer) (or shorter longer))])))
    [member member]))

(defn- count-equality-contexts [ctx expr size]
  (let [[source & extra] (rest (:children expr))]
    (when (and (not (:guards-disabled? ctx)) (integer? size)
               (= 'count (builtin-head ctx expr)) source (empty? extra))
      (refine-path ctx source #(count-parts size %)))))

(defn- invalid-parts [ctx leaf]
  (if (#{:coll :map :map-of :fn} (:t leaf))
    [nil leaf]
    (equality-parts ctx conform/invalid-key leaf)))

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
              (#{'some? 'nil? 'seq 'empty? 'not 'and 'or '= 'not= 'contains? 'instance?
                 'clojure.spec.alpha/valid? 'clojure.spec.alpha/invalid?} head))
      {:test node
       :bindings (into {} (for [n (tree-seq (comp seq :children) :children node) :when (= :sym (:op n))]
                            [(:val n) (get-in ctx [:binding-ids (:val n)])]))})))

(defn- truth-contexts [ctx node]
  (refine-path ctx node (fn [leaf]
                         (for [truthy? [true false]
                               :let [ty (truth-part (:env ctx) leaf truthy?)]]
                           (when-not (= :never (:t ty)) ty)))))

(defn- emptiness-contexts [ctx node some?]
  (let [[source & extra] (rest (:children node))]
    (when (and source (empty? extra))
      (case (builtin-head ctx node)
        seq (some-> (refine-path ctx source #(count-parts 0 %)) rseq vec)
        empty? (when (and (not some?) (not (:guards-disabled? ctx)))
                 (refine-path ctx source #(count-parts 0 %)))
        nil))))

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
      (#{'empty? 'seq} h) (or (emptiness-contexts ctx test false) [ctx ctx])
      (= 'and h) (logical-contexts ctx args true)
      (= 'or h) (logical-contexts ctx args false)

      (and (#{'= 'not=} h) (= 2 (count args)))
      (let [branches (or (cond (and (literal-node? a) (literal-node? b))
                               (if (= (:val a) (:val b)) [ctx nil] [nil ctx])
                               (literal-node? b) (or (count-equality-contexts ctx a (:val b))
                                                     (equality-contexts ctx a (:val b)))
                               (literal-node? a) (or (count-equality-contexts ctx b (:val a))
                                                     (equality-contexts ctx b (:val a))))
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

      (and (= 'clojure.spec.alpha/invalid? h) (= 1 (count args)))
      (or (refine-path ctx a #(invalid-parts ctx %)) [ctx ctx])

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

(defn- keys-directive [node]
  (when (= :kw (:op node))
    (let [key (:val node)
          nsp (namespace key)]
      (case (name key)
        ("keys" "keys!") {:key-of #(keyword (or nsp (namespace %)) (name %))
                          :bang? (= "keys!" (name key))}
        "strs" (when-not nsp {:key-of #(when (simple-symbol? %) (name %))})
        "syms" {:key-of #(when (symbol? %) (symbol (or nsp (namespace %)) (name %)))}
        nil))))

(defn- has-bang? [pattern]
  (and (= :map (:op pattern))
       (some (fn [[k _]] (or (:bang? (keys-directive k)) (has-bang? k))) (partition 2 (:children pattern)))))

(defn- bind-map-entry [ctx pattern node key ty outer-why projection defaults]
  (let [env (:env ctx)
        {:keys [type optional? declared? status]} (key-info ctx ty key node)
        local (when (= :sym (:op pattern)) (:val pattern))
        default (get defaults local)
        default-type (when default (infer ctx default))
        bang? (has-bang? pattern)
        tname (when (and declared? (or optional? bang?)) (t/show (t/remove-nil env ty)))
        why (cond
              default (when (t/nilable? env type)
                        (str key " is " (t/show (t/resolve-t env type))
                             ", and :or applies only when the key is absent"))
              optional? (cond
                          (= :req status) (or outer-why "the destructured value may be nil")
                          declared? (str key " is optional in " tname)
                          :else (str key " may be absent")))]
    (when (and bang? declared? (or optional? (t/nilable? env type)))
      (report! ctx node "TSP2741" (str ":keys! below requires " key ", but it is optional in " tname)))
    (bind-pattern ctx pattern (cond bang? (t/remove-nil env type)
                                    optional? (t/union type (or default-type t/nil-t))
                                    :else type)
                  why
                  (when-not (or default bang?) projection))))

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
        ctx (if-let [alias (:as special)]
              (-> (bind ctx (:val alias) ty)
                  (remember-projection (:val alias) origin path))
              ctx)
        project (fn [key] {:origin origin :path (conj path key)})
        ctx (reduce
             (fn [ctx [k v]]
               (let [kd (keys-directive k)]
                 (cond
                   (and kd (= :vector (:op v)))
                   (reduce
                    (fn [ctx s]
                      (if-let [key (when (#{:sym :kw} (:op s)) ((:key-of kd) (:val s)))]
                        (let [sym (symbol (name (:val s)))
                              binding (assoc s :op :sym :val sym)]
                          (if (:bang? kd)
                            (let [{:keys [type optional? declared?]} (key-info ctx ty key s)]
                              (if missing-sym
                                (-> (bind ctx sym (t/union type t/nil-t)
                                          :why (str key " is missing when " missing-sym " is not nil"))
                                    (update-in [:missing-of missing-sym] (fnil conj []) sym))
                                (do (when (and optional? declared?)
                                      (report! ctx s "TSP2741" (str ":keys! requires " key ", but it is optional in " tname)))
                                    (bind ctx sym type))))
                            (bind-map-entry ctx binding s key ty outer-why (project key) defaults)))
                        ctx))
                    ctx (:children v))

                   (and (= :kw (:op k)) (#{:as :or :missing} (:val k))) ctx
                   (and (= :kw (:op k)) (#{:select :excess :all} (:val k))) (bind ctx (:val v) t/unknown)
                   (and (= :kw (:op k)) (#{:strs! :syms!} (:val k)))
                   (reduce #(bind %1 (:val %2) t/unknown) ctx (filter #(= :sym (:op %)) (:children v)))

                   :else
                   (if-let [key (static-map-key v)]
                     (let [binding (if (#{:sym :kw} (:op k)) (assoc k :op :sym :val (symbol (name (:val k)))) k)]
                       (bind-map-entry ctx binding v key ty outer-why (project key) defaults))
                     ctx))))
             ctx (sort-by (fn [[k _]] (if (keys-directive k) 1 0)) pairs))]
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

(defn- positional-type [ctx ty i]
  (let [members (t/members (:env ctx) ty)]
    (if (some #(or (:items %) (length/constrained? %) (= :nil (:t %))) members)
      (t/with-assumption
       (apply t/union (for [m members]
                        (cond
                          (:items m) (get (:items m) i t/nil-t)
                          (= :nil (:t m)) t/nil-t
                          (length/constrained? m)
                          (case (length/position m i)
                            :present (:of m)
                            :absent t/nil-t
                            :possible (t/union (:of m) t/nil-t))
                          :else (eff ctx (indexed (elem-type ctx m))))))
       (t/assumption ty))
      (indexed (elem-type ctx ty)))))

(defn- sequence-tail [ctx ty skip nullable?]
  (t/with-assumption
   (apply t/union
          (for [member (t/members (:env ctx) (eff ctx ty))]
            (if (= :never (:t member))
              t/never
              (let [items (:items member)
                    [min-count max-count] (if (= :nil (:t member)) [0 0] (length/bounds member))
                    lo (max 0 (- min-count skip))
                    hi (when max-count (max 0 (- max-count skip)))
                    tail (cond
                           (and nullable? (= 0 hi)) t/nil-t
                           items (assoc (t/tuple (subvec items (min skip (count items)))) :kind :seq)
                           :else (length/with-bounds {:t :coll :kind :seq :of (elem-type ctx member)}
                                                     (if nullable? (max lo 1) lo) hi))]
                (if (and nullable? (zero? lo)) (t/union tail t/nil-t) tail)))))
   (t/assumption ty)))

(defn- positional-rest [ctx ty i]
  (sequence-tail ctx ty i true))

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
     :vector (let [members (t/members (:env ctx) ty)
                   [ctx origin path] (if (every? :items members)
                                       (track-path ctx (or projection {:path [] :type ty}))
                                       [ctx])
                   project (fn [path] (when origin {:origin origin :path path}))]
               (loop [ctx ctx, i 0, [p & more] (:children pattern)]
                 (cond (nil? p) ctx
                       (and (= :sym (:op p)) (= '& (:val p)))
                       (recur (bind-pattern ctx (first more) (positional-rest ctx ty i)) i (next more))
                       (and (= :kw (:op p)) (= :as (:val p)))
                       (bind-pattern ctx (first more) ty why (project path))
                       :else
                       (recur (bind-pattern ctx p (positional-type ctx ty i) nil
                                            (when (every? #(contains? (:items %) i) members)
                                              (project (conj path i))))
                              (inc i) more))))
     ctx)))

;; ---- function calls ----

(defn- matching-arities [ftype n]
  (let [arities (:arities ftype)]
    (if (::t/dispatch (meta ftype))
      (when-let [arity (t/dispatch-arity arities n)] [arity])
      (filter #(t/arity-fits? % n) arities))))

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

(defn- infer-with-expected [ctx node expected]
  (let [ty (when (and expected (= 'atom (builtin-head ctx node)))
             (t/resolve-t (:env ctx) expected))]
    (infer (cond-> ctx (= :atom (:t ty)) (assoc :atom-content (:of ty))) node)))

(defn- infer-call-arguments [ctx call-node arg-nodes fits]
  (let [pending? (fn [i arg]
                   (and (callback-node? ctx arg)
                        (some #(callback-signature ctx (argument-type ctx % arg-nodes i)) fits)))
        types (mapv (fn [i arg]
                      (when-not (pending? i arg)
                        (let [params (when (= 'atom (builtin-head ctx arg))
                                       (map #(argument-type ctx % arg-nodes i) fits))]
                          (infer-with-expected ctx arg (when (and (seq params) (apply = params)) (first params))))))
                    (range) arg-nodes)
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

(defn- call-fn [ctx node fname ftype arg-nodes & [typed-args]]
  (if (= :all (:t ftype))
    (call-generic ctx node fname ftype arg-nodes typed-args)
    (let [n (count arg-nodes)
        ars (:arities ftype)
          fits (matching-arities ftype n)
        arg-types (or typed-args (infer-call-arguments ctx node arg-nodes fits))]
    (cond
      (empty? ars) (or (:ret ftype) t/unknown)
      (empty? fits) (do (report! ctx node "TSP2554"
                                 (str fname " expects " (str/join " or " (distinct (map #(str (t/arity-minimum %) (when (or (:rest %) (:kwargs %)) "+")) ars)))
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
                :else (let [arity (nth fits i)]
                        (if (some #{i} passing)
                          (specialize-map-return ctx arity (first arg-types))
                          (:ret arity)))))))))

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

(defn- record-var-use! [ctx sym node kind]
  (when (and *var-uses* (not (:quiet ctx)) (not (:synthetic node)) (:pos node)
             (or (= :var kind) (not (contains? (:possible-bindings ctx) (:val node)))))
    (when-let [entry (get-in ctx [:env :vars sym])]
      (when-not (:private? entry)
        (swap! *var-uses* conj {:var sym :kind kind :file (get-in ctx [:file :file]) :pos (:pos node)})))))

(defn- read-var [ctx sym node kind]
  (let [{full :sym :as resolved} (resolve-var ctx sym node)
        {:keys [tracks-root? initializers]} (get-in ctx [:env :var-declarations full])]
    (record-var-use! ctx full node kind)
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
  (cond-> (case (:t ty)
            :lit (t/with-assumption (t/lit-prim (:val ty)) (t/assumption ty))
            :union (apply t/union (map widen (:types ty)))
            ty)
    (:platform ty) (assoc :platform true)))

(defn- vector-member? [member]
  (or (= :vector (:kind member))
      (and (= :prim (:t member)) (= :vector (:name member)))))

(defn- widen-vector-growth [env ty]
  (when (= :union (:t ty))
    (let [members (t/members env ty)]
      (when (every? vector-member? members)
        (t/with-assumption
         {:t :coll :kind :vector :of (widen (apply t/union (map #(or (:of %) t/unknown) members)))}
         (t/assumption ty))))))

(defn- map-edits [ctx head args]
  (case head
    assoc (when (and (seq args) (even? (count args)))
            (let [pairs (partition 2 args)]
              (when (every? literal-map-key-node? (map first pairs))
                (let [edits (mapv (fn [[key value]] [(:val key) (quietly ctx value)]) pairs)]
                  (when (every? (fn [[k ty]]
                                  (and (not (t/assumption ty))
                                       (or (nil? (get-in (:env ctx) [:specs k]))
                                           (t/sub? (:env ctx) (eff ctx ty) (t/ref-t k)))))
                                edits)
                    edits)))))
    dissoc (when (every? literal-map-key-node? args)
             (mapv (fn [key] [(:val key) nil]) args))
    nil))

(defn- infer-map-edits [ctx param node]
  (cond
    (= param (plain-local ctx node)) []
    (#{'assoc 'dissoc '->} (builtin-head ctx node))
    (let [[base & args] (args-of node)
          head (builtin-head ctx node)]
      (when-let [before (infer-map-edits ctx param base)]
        (if (= '-> head)
          (reduce (fn [edits step]
                    (when-let [next-edits (and edits (map-edits ctx (builtin-head ctx step) (args-of step)))]
                      (into edits next-edits)))
                  before args)
          (when-let [after (map-edits ctx head args)] (into before after)))))))

(defn- specialize-map-return [ctx {:keys [map-edits ret]} arg]
  (if (or (not map-edits) (:guards-disabled? ctx))
    ret
    (let [members (t/members (:env ctx) (eff ctx arg))]
      (if (every? #(= :map (:t %)) members)
        (let [result (apply t/union
                            (for [m members]
                              (reduce (fn [shape [k v]]
                                        (if v
                                          (assoc-map-key shape k (widen (dissoc v :fresh)))
                                          (cond-> (-> shape (update :req dissoc k) (update :opt dissoc k))
                                            (not (:complete-keys? shape)) (assoc-in [:opt k] t/never))))
                                      (dissoc m :fresh) map-edits)))]
          (if (t/sub? (:env ctx) result ret) (t/with-assumption result (t/assumption arg)) ret))
        ret))))

(defn var-type
  "Type of a var. A function with no declared return type gets it inferred from its body, once."
  [ctx sym]
  (if-let [v (get-in (:env ctx) [:vars sym])]
    (let [infer-return? (and (= :hints (:source v)) (some #(= :unknown (:t (:ret %))) (:arities (:type v))))
          shape-candidate? (and (#{:hints :sig :fdef} (:source v))
                                (= 1 (get-in ctx [:env :root-definitions sym]))
                                (not (dynamic-definition? v))
                                (= 1 (count (:arities (:type v))) (count (:arities (:parts v))))
                                (not (get-in v [:type :arities 0 :rest]))
                                (not (get-in v [:type :arities 0 :kwargs]))
                                (= 1 (count (get-in v [:type :arities 0 :params]))))]
      (cond
        (not (or infer-return? shape-candidate?)) (:type v)
        (contains? @*rets* sym) (get @*rets* sym)
        :else
        (do (swap! *rets* assoc sym (:type v)) ; a recursive call sees the declared part only
            (let [fctx {:env (:env ctx) :file (:pfile v) :opts (:opts ctx) :locals {} :quiet true}
                  ars (mapv (fn [a {:keys [params body]}]
                              (if (and (not shape-candidate?) (not= :unknown (:t (:ret a))))
                                a
                                (let [{:keys [fixed rest]} (env/split-params params)
                                      c (reduce (fn [c [p ty]] (bind-pattern c p ty)) fctx (map vector fixed (:params a)))
                                      c (if rest (bind-pattern c rest (or (:kwargs a) t/unknown)) c)
                                      a (if (and infer-return? (= :unknown (:t (:ret a))))
                                          (let [ret (binding [*types* nil *trust* (atom {:cast 0 :any 0 :ignore 0}) *ignores* (atom [])]
                                                      (infer-body c body))]
                                            (assoc a :ret (widen (dissoc ret :fresh)) :inferred true))
                                          a)
                                      param (first fixed)
                                      expr (first body)
                                      edits (when (and shape-candidate? (not rest) (= :sym (:op param)) (= 1 (count body))
                                                       (not-any? :directives (tree-seq (comp seq :children) :children expr)))
                                              (seq (infer-map-edits c (:val param) expr)))]
                                  (cond-> a edits (assoc :map-edits (vec edits))))))
                            (:arities (:type v)) (:arities (:parts v)))
                  ty (assoc (:type v) :arities ars)]
              (swap! *rets* assoc sym ty)
              ty))))
    (when-let [{:keys [file node]} (get-in ctx [:env :atom-initializers sym])]
      (or (get @*rets* sym)
          (do (swap! *rets* assoc sym t/unknown)
              (let [ty (quietly {:env (:env ctx) :file file :opts (:opts ctx) :locals {} :load-time? true} node)]
                (swap! *rets* assoc sym ty)
                ty))))))

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

(defn- bind-pairs [ctx bindings & [on-init]]
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
                  _ (when on-init (on-init ctx p init ty))
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

(defn infer-body
  ([ctx forms] (infer-body ctx forms nil))
  ([ctx forms expected]
   (loop [[form & more] forms, ctx ctx, result t/nil-t]
     (if form
       (let [reachable? (not= :never (:t result))
             ;; Keep diagnostics for later forms without adding dead loop edges.
             inferred (infer-with-expected (cond-> ctx (not reachable?) (dissoc :recur-types))
                                           form (when-not (seq more) expected))
             result (if reachable? inferred result)
             initialized (when (not= :never (:t result))
                           (seq (body-initializers ctx form)))]
         (recur more (cond-> ctx initialized (update :initialized-vars (fnil into #{}) initialized)) result))
       result))))

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
        tail {:t :coll :kind :seq :of rest-of :min-count 1}
        c (if rest
            (bind-pattern c rest (cond kwargs kwargs
                                       (not (seq contracts)) t/unknown
                                       (= t/never rest-of) t/nil-t
                                       (some #(= (count fixed) (t/arity-minimum %)) contracts) (t/union tail t/nil-t)
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
  [ctx ftype arg-types at label & [require-match?]]
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
      (cond (and vague? (next (distinct (map :ret ok)))) t/unknown
            (seq ok) (specialize-map-return ctx ar (first arg-types))
            require-match? t/unknown
            :else (:ret ar)))
    (if (seq (:arities ftype))
      (do (report! ctx at "TSP2554" (str label " is called with " (count arg-types) " argument(s)")) t/unknown)
      t/unknown))))

(defn apply-fn-node [ctx fnode arg-types & [require-match?]]
  (case (:op fnode)
    :kw (read-key ctx (first arg-types) (:val fnode) fnode)
    :set (t/union (first arg-types) t/nil-t)
    (let [ty (if (callback-node? ctx fnode)
               (infer ctx fnode {:t :fn :arities [{:params arg-types :ret t/unknown}]})
               (infer ctx fnode))
          ty (if (= :ref (:t ty)) (t/resolve-t (:env ctx) ty) ty)]
      (apply-type ctx ty arg-types fnode (parse/unparse fnode) require-match?))))

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
                           (some (fn [[a _]] (when (:rest a) (t/arity-minimum a))) valid))]
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
          (assoc shape :arities
                 (mapv (fn [[arity _]]
                         (let [remaining (max 0 (- (t/arity-minimum arity) (max n (count (:params arity)))))]
                           (cond-> (-> arity (update :params #(vec (drop n %))) (dissoc :rest-min))
                             (pos? remaining) (assoc :rest-min remaining))))
                       valid))))
      (do (run! #(infer ctx %) args)
          (check-composable! ctx f ty "partial")))))

(defn- single-map [ctx ty]
  (let [ms (t/members (:env ctx) ty)]
    (when (and (= 1 (count ms)) (= :map (:t (first ms)))) (first ms))))

(defn- check-key-value!
  "Key = type: a value stored under a qualified key must fit the spec of that key."
  [ctx k vnode vtype]
  (when (and (qualified-keyword? k) (get-in (:env ctx) [:specs k]))
    (let [actual (eff ctx vtype) expected (t/ref-t k)
          result (t/sub-result (:env ctx) actual expected)]
      (case (:status result)
        :inconclusive (let [[code msg details] (incomplete-problem ctx actual expected result)]
                        (apply report! ctx vnode code msg (mapcat identity details)))
        :mismatch (if-let [[code msg details] (nested-platform-problem ctx "TSP2322" actual expected result)]
                    (apply report! ctx vnode code msg (mapcat identity details))
                    (report! ctx vnode "TSP2322" (str (t/show actual) " is not assignable to " (show-ref ctx expected))))
        :proved nil))))

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

(defmethod special 'clojure.spec.alpha/conform [ctx node _]
  (let [[spec value :as args] (args-of node)]
    (if (= 2 (count args))
      (let [form (when (or (and (= :kw (:op spec)) (literal-node? spec))
                           (#{'clojure.spec.alpha/or 'clojure.spec.alpha/spec} (builtin-head ctx spec)))
                   (parse/node->data spec))
            result (when form
                     (conform/tagged-result (:env ctx)
                                            (assoc (env/type-ctx (:file ctx) nil)
                                                   :locals (:locals ctx)
                                                   :defined-symbols (get-in ctx [:env :ns-defs (get-in ctx [:file :ns])]))
                                            form))]
        (when (and *spec-uses* (qualified-keyword? form) (not (:quiet ctx)) (not (:synthetic node)))
          (swap! *spec-uses* conj {:spec form :kind :conform :file (get-in ctx [:file :file]) :pos (:pos spec)}))
        (when-not (and result (= :list (:op spec))) (infer ctx spec))
        (if (= t/never (infer ctx value)) t/never (or result t/unknown)))
      (do (run! #(infer ctx %) args)
          (report! ctx node "TSP2554" "s/conform expects two arguments")
          t/unknown))))

(defmethod special 'clojure.spec.alpha/invalid? [ctx node _]
  (let [[value :as args] (args-of node)]
    (if (= 1 (count args))
      (let [leaves (t/members (:env ctx) (eff ctx (infer ctx value)))
            parts (map #(invalid-parts ctx %) leaves)]
        (apply t/union (concat (when (some first parts) [(t/lit true)])
                               (when (some second parts) [(t/lit false)]))))
      (do (run! #(infer ctx %) args)
          (report! ctx node "TSP2554" "s/invalid? expects one argument")
          t/unknown))))

(defn- var-reference [ctx [target & more]]
  (if (and (= :sym (:op target)) (empty? more))
    (do (record-var-use! ctx (:sym (resolve-var ctx (:val target) target)) target :var)
        (t/class-type "clojure.lang.Var"))
    t/unknown))

(defmethod special 'var [ctx node _]
  (var-reference ctx (args-of node)))

(defn- quoted-type [node]
  (case (:op node)
    :sym (t/prim :symbol)
    :list (t/class-type "clojure.lang.IPersistentList")
    t/unknown))

(defmethod special 'quote [_ node _]
  (quoted-type (second (:children node))))

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
                                           (let [joined (widen (apply t/union ty (map #(nth % i) @calls)))]
                                             ;; Repeated vector appends may grow the shape on every back edge.
                                             (or (when (and (>= n 2) (not= ty joined))
                                                   (widen-vector-growth (:env ctx) joined))
                                                 joined)))
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
(defmethod special 'throw [ctx node _]
  (let [[exception :as args] (args-of node)]
    (if (= 1 (count args))
      ;; The JVM also accepts a null reference here and raises an NPE.
      (assignable! ctx exception (infer ctx exception)
                   (t/union (t/class-type "java.lang.Throwable") t/nil-t))
      (do (run! #(infer ctx %) args)
          (report! ctx node "TSP2554" "throw expects exactly one argument"))))
  t/never)

(defmethod special 'assert [ctx node _]
  (if (= 'assert (t/core-name (:file ctx) (:val (first (:children node)))))
    (let [[test message :as args] (args-of node)]
      (if (<= 1 (count args) 2)
        (let [[_ failure] (condition-contexts ctx test)]
          (when (and message failure)
            (infer failure message)))
        (do (run! #(infer ctx %) args)
            (report! ctx node "TSP2554" "assert expects one or two arguments")))
      ;; Clojure may omit assertions at compile time.
      t/nil-t)
    t/unknown))

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
        [tc fc] (or (emptiness-contexts ctx init some?) [ctx ctx])]
    [(if (or (nil? tc) (= :never (:t positive))) t/never
         (infer (bind-pattern tc p (dissoc positive :fresh)) then))
     (if (or (nil? fc) (= :never (:t negative))) t/never (if else (infer fc else) t/nil-t))]))

(defmethod special 'if-let [ctx node _] (apply t/union (if-let-form ctx node false)))
(defmethod special 'if-some [ctx node _] (apply t/union (if-let-form ctx node true)))

(defn- when-let-form [ctx node some?]
  (let [[bindings & body] (args-of node)
        [p init] (:children bindings)
        ty (eff ctx (infer ctx init))
        positive (if some? (t/remove-nil (:env ctx) ty) (truth-part (:env ctx) ty true))
        negative (if some? (nil-part (:env ctx) ty) (truth-part (:env ctx) ty false))
        [tc fc] (or (emptiness-contexts ctx init some?) [ctx ctx])]
    (t/union (if (or (nil? tc) (= :never (:t positive))) t/never
                 (infer-body (bind-pattern tc p (dissoc positive :fresh)) body))
             (if (or (nil? fc) (= :never (:t negative))) t/never t/nil-t))))

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
        body (remove #(or (catch? %) (finally? %)) forms)
        normal (apply t/union
                      (infer-body ctx body)
                      (for [c (filter catch? forms)
                            :let [[cls sym & cbody] (args-of c)
                                  cn (t/resolve-class (:imports (:file ctx)) (:val cls))]]
                        (infer-body (bind ctx (:val sym) (if cn (t/class-type cn) t/unknown) :hinted true) cbody)))
        cleanup (mapv #(infer-body ctx (args-of %)) (filter finally? forms))]
    (if (some #(= :never (:t %)) cleanup) t/never normal)))

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

(defn- close-receiver-class [ctx member init]
  (or (class-of ctx member)
      (:jvm-class member)
      (case (:t member)
        :atom "clojure.lang.Atom"
        :map (when (= :map (:op init)) "clojure.lang.APersistentMap")
        :coll (case (:op init)
                :vector "clojure.lang.PersistentVector"
                :set "clojure.lang.PersistentHashSet"
                nil)
        nil)))

(defn- check-close! [ctx binding init actual]
  (let [env (:env ctx)
        hint (when (#{:unknown :any} (:t (t/resolve-t env actual)))
               (env/hint-type (:file ctx) binding))
        members (t/members env (eff ctx (or hint actual)))
        classes (distinct (map #(close-receiver-class ctx % init)
                               (remove #(#{:any :nil :never} (:t %)) members)))]
    (when (some #(= :nil (:t %)) members)
      (report! ctx init "TSP2531" "with-open resource may be nil when close is called"))
    (doseq [cname classes
            :when (and cname (java/has-class? cname))
            :let [methods (java/methods-named cname "close" false)
                  shown (java/short-name cname)]]
      (cond
        (empty? methods)
        (report! ctx init "TSP2339" (str "with-open resource " shown " has no instance method close"))

        (not-any? #(empty? (:params %)) methods)
        (report! ctx init "TSP2554" (str "with-open resource " shown " has no zero-argument close method"))))
    (when (and (:strict (:opts ctx))
               (some #(not (some-> % java/has-class?)) classes))
      (report! ctx init "TSP210-01" "with-open resource close method is not checked"
               :reasons #{:close-method}))))

(defmethod special 'with-open [ctx node _]
  (let [[bindings & body] (args-of node)
        forms (:children bindings)]
    (if (and (= :vector (:op bindings)) (even? (count forms))
             (every? #(= :sym (:op %)) (take-nth 2 forms)))
      (let [reachable? (volatile! true)
            c (bind-pairs ctx bindings
                          (fn [c binding init actual]
                            (when @reachable?
                              (if (= t/never actual)
                                (vreset! reachable? false)
                                (check-close! c binding init actual)))))
            result (infer-body (cond-> c (not @reachable?) (dissoc :recur-types)) body)]
        (if @reachable? result t/never))
      (do (report! ctx (or bindings node) "TSP2345" "with-open expects a binding vector of symbol and initializer pairs")
          (when (= :vector (:op bindings))
            (run! #(infer ctx %) (take-nth 2 (rest forms))))
          (infer-body ctx body)))))

(doseq [h '[binding with-redefs with-local-vars]]
  (defmethod special h [ctx node _]
    (let [[bindings & body] (args-of node)]
      (run! #(infer ctx (second %)) (partition 2 (:children bindings)))
      (infer-body (cond-> ctx (= 'with-redefs h) (assoc :guards-disabled? true)) body))))

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

(defn- read-map-of [ctx source key-type not-found]
  (let [env (:env ctx)
        members (t/members env source)
        key-members (t/members env key-type)]
    (if (every? #(#{:map-of :nil :never} (:t %)) members)
      (t/with-assumption
       (apply t/union not-found
              (for [{:keys [k v] :as member} members
                    :when (and (= :map-of (:t member))
                               (not-every? #(disjoint-scalars? env % k) key-members))]
                v))
       (or (t/assumption source) (t/assumption key-type)))
      t/unknown)))

(defn- read-string-key [ctx source k not-found]
  (let [{:keys [status type]} (lookup-key ctx source k)]
    (case status
      :req (if (t/nilable? (:env ctx) source) (t/union type not-found) type)
      :opt (t/union type not-found)
      (:nil :absent) not-found
      t/unknown)))

(defn- vector-element-at [member index]
  (cond
    (= :absent (length/position member index)) t/never
    (:items member) (nth (:items member) (int index))
    :else (or (:of member) t/unknown)))

(defn- read-vector-index [ctx vectors key-type not-found]
  (let [env (:env ctx)
        keys (t/members env key-type)]
    (apply t/union
           (for [member vectors
                 key keys]
             (cond
               (= :nil (:t member)) not-found
               (or (= :never (:t member)) (= :never (:t key))) t/never

               (= :lit (:t key))
               (if (integer? (:val key))
                 ;; Clojure vectors use Number.intValue for lookup, including wraparound.
                 (let [index (.intValue ^Number (:val key))
                       element (vector-element-at member index)]
                   (if (= :present (length/position member index)) element (t/union element not-found)))
                 not-found)

               (or (= :double (:name key))
                   (disjoint-scalars? env key (t/prim :int))
                   (= 0 (second (length/bounds member)))) not-found

               :else (t/union (or (:of member) t/unknown) not-found))))))

(defmethod special 'get [ctx node _]
  (let [[m k default] (args-of node)
        mty (infer ctx m)]
    (if (= :kw (:op k))
      (if default
        (read-key-default ctx mty (:val k) k (infer ctx default))
        (read-key ctx mty (:val k) k))
      (let [source (eff ctx mty)
            key-type (eff ctx (infer ctx k))
            not-found (if default (infer ctx default) t/nil-t)
            members (t/members (:env ctx) source)
            assumption (or (t/assumption source) (t/assumption key-type))]
        (cond
          (some #{t/never} [source key-type not-found])
          t/never

          (and (= :lit (:t key-type)) (string? (:val key-type)) (some #(= :map (:t %)) members))
          (t/with-assumption (read-string-key ctx source (:val key-type) not-found) assumption)

          (and (some vector-member? members) (every? #(or (vector-member? %) (#{:nil :never} (:t %))) members))
          (t/with-assumption (read-vector-index ctx members key-type not-found) assumption)

          :else
          (read-map-of ctx source key-type not-found))))))

(defn- numeric-path-value [ctx ty index]
  (let [env (:env ctx)
        vector-type (t/prim :vector)]
    (apply t/union
           (for [member (t/members env ty)]
             (case (:t member)
               :coll (case (:kind member)
                       :vector (if (<= 0 index Integer/MAX_VALUE) (vector-element-at member index) t/never)
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
                                       (let [i (:val key)]
                                         {:type (numeric-path-value ctx current i)
                                          :optional? (not (every? #(and (= :vector (:kind %)) (<= 0 i Integer/MAX_VALUE)
                                                                          (= :present (length/position % i)))
                                                                  (t/members (:env ctx) current)))})
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
  (apply t/union (for [member (t/members (:env ctx) ty)]
                   (cond (= :map (:t member)) (t/prim :map)
                         (or (:items member) (length/constrained? member))
                         (assoc (dissoc member :items :min-count :max-count) :of t/unknown)
                         :else member))))

(defn- literal-map-keys [node]
  (when (and (= :vector (:op node)) (empty? (:directives node)) (not (env/tag-node node))
             (every? literal-map-key-node? (:children node)))
    (:children node)))

(defn- path-writes
  "Find the previous leaf and rebuild each possible root after a literal map-path write."
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
                             (:map :nil :map-of)
                             (let [m (if (= :nil (:t member)) empty-map-shape member)
                                   map-of? (= :map-of (:t m))
                                   previous (cond
                                              map-of? (t/union (or (map-of-entry env m k) t/never) t/nil-t)
                                              (contains? (:req m) k) (get (:req m) k)
                                              (contains? (:opt m) k) (t/union (get (:opt m) k) t/nil-t)
                                              (:complete-keys? m) t/nil-t
                                              (get-in env [:specs k]) (t/union (t/ref-t k) t/nil-t)
                                              :else t/unknown)
                                   store (fn [value]
                                           (if map-of?
                                             (cond-> (update m :v t/union value)
                                               (not (t/sub? env (t/lit k) (:k m))) (update :k t/union (t/lit k)))
                                             (assoc-map-key m k value)))]
                               (if (seq more)
                                 (for [[leaf rebuild] (walk previous more)]
                                   [leaf (fn [value]
                                           (let [child (rebuild value)]
                                             (when-not (= :unknown (:t child)) (check-key-value! ctx k key child))
                                             (store child)))])
                                 [[previous store]]))
                             :never []
                             (:any :unknown :class :var :rigid :ivar) [unchecked]
                             (if (and (= :prim (:t member)) (= :map (:name member)))
                               [unchecked]
                               (do (report! ctx key "TSP2339" (str "cannot write " k " into " (t/show member)))
                                   []))))
                        members)))))]
      (walk root keys))))

(defn- write-path [ctx node update?]
  (let [[m path value & args :as arguments] (args-of node)
        mty (infer ctx m)
        keys (literal-map-keys path)]
    (cond
      (or (nil? value) (and (not update?) (seq args)))
      (do (run! #(infer ctx %) (rest arguments))
          (report! ctx node "TSP2554" (str (if update? "update-in" "assoc-in") " is called with " (count arguments) " argument(s)"))
          t/unknown)

      (seq keys)
      (let [writes (path-writes ctx mty keys)
            vty (if (and update? (seq writes))
                  (apply-fn-node ctx value (into [(apply t/union (map first writes))] (map #(infer ctx %) args)))
                  (do (run! #(infer ctx %) args) (infer ctx value)))
            k (:val (last keys))
            spec (when (get-in (:env ctx) [:specs k]) (t/ref-t k))
            stored (if (and spec (not (t/sub? (:env ctx) (eff ctx vty) spec)))
                     spec
                     (dissoc vty :fresh))]
        (when (seq writes) (check-key-value! ctx k value vty))
        (if (= t/never vty) t/never (apply t/union (map #((second %) stored) writes))))

      :else
      (do (run! #(infer ctx %) (rest arguments)) t/unknown))))

(defmethod special 'assoc-in [ctx node _] (write-path ctx node false))
(defmethod special 'update-in [ctx node _] (write-path ctx node true))

(defn- assoc-entries [ctx base entries]
  (reduce (fn [ty [[k v] [kt vt]]]
            (when (= :kw (:op k)) (check-key-value! ctx (:val k) v vt))
            (let [literal-key (when (or (= :kw (:op k)) (literal-map-key-node? k)) (:val k))
                  env (:env ctx)
                  members (t/members env ty)
                  add (fn [base shape]
                        (cond
                          (= :map-of (:t shape))
                          (if (and (t/sub? env kt (:k shape)) (t/sub? env vt (:v shape)))
                            base
                            (-> shape (update :k t/union (widen kt)) (update :v t/union (widen vt))))
                          (= :vector (:kind shape))
                          (let [index (:val kt)
                                present? (and (= :lit (:t kt)) (integer? index)
                                              (= :present (length/position shape index)))]
                            (assignable! ctx k kt (t/prim :int))
                            (if (and (:items shape) present?)
                              (t/tuple (assoc (:items shape) (int index) vt))
                              (-> (cond-> (dissoc shape :items) (not present?) (dissoc :max-count))
                                  (update :of t/union (widen vt)))))
                          (= (t/prim :vector) shape)
                          (do (assignable! ctx k kt (t/prim :int)) base)
                          (some? literal-key)
                          (assoc-key-type ctx base literal-key (key-type ctx literal-key vt))
                          (= :nil (:t shape)) (t/prim :map)
                          :else (forget-map-keys ctx base)))]
              (if (and (next members)
                       (some #(or (= :map-of (:t %)) (= :vector (:kind %)) (= (t/prim :vector) %)) members))
                (apply t/union (map #(add % %) members))
                (add ty (first members)))))
          base entries))

(defmethod special 'assoc [ctx node _]
  (let [[m & kvs] (args-of node)
        base (infer ctx m)
        pairs (partition 2 kvs)
        types (mapv (fn [[k v]]
                      (let [vt (infer ctx v)
                            kt (if (= :kw (:op k)) (t/lit (:val k)) (infer ctx k))]
                        [kt vt]))
                    pairs)]
    (assoc-entries ctx base (map vector pairs types))))

(defmethod special 'dissoc [ctx node _]
  (let [[m & ks] (args-of node)
        mty (infer ctx m)
        members (t/members (:env ctx) mty)]
    (if (and (every? #(or (= :kw (:op %)) (literal-map-key-node? %)) ks)
             (every? #(#{:map :nil} (:t %)) members))
      (apply t/union
             (for [member members]
               (if (= :map (:t member))
                 (reduce (fn [shape node]
                           (let [k (:val node)]
                             (cond-> (-> shape (update :req dissoc k) (update :opt dissoc k))
                               (and (string? k) (not (:complete-keys? shape))) (assoc-in [:opt k] t/never))))
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
    (if-let [keys (literal-map-keys ks)]
      (if (= t/never mty)
        t/never
        (reduce (fn [shape node]
                  (let [k (:val node)
                        {:keys [type optional? declared?]} (key-info ctx mty k node)]
                    (if (= t/never type)
                      shape
                      (assoc-in shape [(if (or optional? (not declared?)) :opt :req) k] type))))
                empty-map-shape keys))
      (do (infer ctx ks) t/unknown))))

;; collections

(defn- coll-of [kind ty] {:t :coll :kind kind :of (widen (dissoc ty :fresh))})

(defn- unsupported-sequence-member [ctx ty]
  (some (fn [member]
          (when (#{:int :double :number :boolean :keyword :symbol :uuid :inst}
                 (:name (if (= :lit (:t member)) (t/lit-prim (:val member)) member)))
            member))
        (t/members (:env ctx) ty)))

(defn- report-invalid-sequence-args! [ctx head args sources]
  (let [invalid (keep (fn [[arg source]]
                        (when-let [member (unsupported-sequence-member ctx source)]
                          [arg member]))
                      (map vector args sources))]
    (doseq [[arg member] invalid]
      (report! ctx arg "TSP2345" (str head " cannot sequence " (t/show member))))
    (boolean (seq invalid))))

(defn- spread-tail-types [ctx tail ty]
  (let [form (if (= :quote (:op tail)) (first (:children tail)) tail)]
    (if (and (= :list (:op form)) (empty? (:children form)) (empty? (:directives tail)))
      [[]]
      (mapv (fn [member]
              (cond
                (= :nil (:t member)) []
                (:items member) (:items member)
                (= :coll (:t member)) (let [[lo hi] (length/bounds member)]
                                        (when (and (= lo hi) (<= lo 64))
                                          (vec (repeat lo (:of member)))))))
            (t/members (:env ctx) (eff ctx ty))))))

(defn- spread-tail-nodes [tail items]
  (if (and (= :vector (:op tail)) (empty? (:directives tail)) (not (env/tag-node tail))
           (= (count items) (count (:children tail))))
    (:children tail)
    (mapv (fn [i ty]
            (cond-> {:op :spread :pos (:pos tail) :synthetic true
                     :display (str (parse/unparse tail) " at index " i)}
              (and (= :lit (:t ty)) (keyword? (:val ty))) (assoc :op :kw :val (:val ty))))
          (range) items)))

(defn- checkable-spread-contract? [ty]
  (or (= :all (:t ty))
      (some (fn [{:keys [params rest kwargs]}]
              (or (not rest) kwargs
                  (some #(not (#{:unknown :any} (:t %))) (conj params rest))))
            (:arities ty))))

(defmethod special 'apply [ctx node _]
  (let [[f & args] (args-of node)]
    (if (empty? args)
      (do (when f (infer ctx f))
          (report! ctx node "TSP2554" "apply expects a function and a final sequence argument")
          t/unknown)
      (let [prefix (vec (butlast args))
            tail (last args)
            prefix-types (mapv #(infer ctx %) prefix)
            tail-type (infer ctx tail)
            spreads (spread-tail-types ctx tail tail-type)
            exact? (every? some? spreads)
            expected (when (and exact? (callback-node? ctx f))
                       {:t :fn :arities (mapv #(hash-map :params (into prefix-types %) :ret t/unknown) spreads)})
            ftype (t/resolve-t (:env ctx) (infer ctx f expected))
            invalid-tail (unsupported-sequence-member ctx tail-type)
            callable? (or (= :all (:t ftype)) (seq (:arities ftype)))]
        (cond
          (some #(= :never (:t %)) (into [ftype tail-type] prefix-types)) t/never

          invalid-tail
          (do (report! ctx tail "TSP2345" (str "apply cannot spread " (t/show invalid-tail)))
              t/unknown)

          (and exact? callable?)
          (let [label (str "apply " (parse/unparse f))]
            (apply t/union
                   (for [items spreads]
                     (call-fn ctx node label ftype (into prefix (spread-tail-nodes tail items))
                              (into prefix-types items)))))

          exact?
          (do (when-not (= :any (:t ftype)) (check-composable! ctx f ftype "apply"))
              t/unknown)

          :else
          (do (when (or (:strict (:opts ctx)) (checkable-spread-contract? ftype))
                (report! ctx tail "TSP410-01" "apply cannot verify the length or positions of the final argument"
                         :severity (if (:strict (:opts ctx)) :error :warning) :reasons #{:apply-tail}))
              t/unknown))))))

(defmethod special 'lazy-seq [ctx node _]
  (let [forms (args-of node)
        body (infer-body (dissoc ctx :recur-types) forms)]
    (when-let [bad (unsupported-sequence-member ctx body)]
      (report! ctx (last forms) "TSP2345" (str "lazy-seq cannot realize a sequence from " (t/show bad))))
    (coll-of :seq (if (= t/never body) t/never (elem-type ctx body)))))

(doseq [[h i] {'first 0 'second 1}]
  (defmethod special h [ctx node _]
    (positional-type ctx (infer ctx (first (args-of node))) i)))

(doseq [[h i] {'key 0 'val 1}]
  (defmethod special h [ctx node _]
    (let [[entry :as args] (args-of node)
          ty (infer ctx entry)]
      (run! #(infer ctx %) (rest args))
      (if (and (= 1 (count args)) (map-entry-type? ctx ty))
        (positional-type ctx ty i)
        t/unknown))))

(doseq [h '[rand-nth ffirst]]
  (defmethod special h [ctx node _]
    (let [[c] (args-of node)]
      (indexed (elem-type ctx (infer ctx c))))))

(defmethod special 'nth [ctx node _]
  (let [[c index default & extra :as args] (args-of node)
        ty (infer ctx c)
        it (infer ctx index)
        default-type (when default (infer ctx default))
        value (:val it)
        literal-index? (and (= :lit (:t it)) (number? value))
        i (when literal-index? (try (int value) (catch IllegalArgumentException _ nil)
                                                  (catch ArithmeticException _ nil)))
        members (t/members (:env ctx) (eff ctx ty))]
    (cond
      (or (nil? index) (seq extra))
      (do (run! #(infer ctx %) extra)
          (report! ctx node "TSP2554" (str "nth expects 2 or 3 arguments, got " (count args)))
          t/unknown)

      (= :mismatch (:status (t/sub-result (:env ctx) (eff ctx it) (t/prim :number))))
      (do (assignable! ctx index it (t/prim :number))
          t/unknown)

      (and literal-index? (nil? i))
      (do (report! ctx index "TSP330-02" (str "index " value " cannot be represented as a JVM int"))
          t/unknown)

      (some? i)
      (let [missing? #(and (= :coll (:t %)) (= :absent (length/position % i)))]
        (when-let [miss (and (nil? default) (first (filter missing? members)))]
          (report! ctx index "TSP330-02" (str "index " i
                                              (if (every? missing? members) " is" " may be")
                                              " out of bounds for length " (length/describe miss))))
        (apply t/union (for [m members]
                         (cond
                           (= :nil (:t m)) (or default-type t/nil-t)
                           (missing? m) (or default-type t/never)
                           (:items m) (get (:items m) i (or default-type t/never))
                           (and (= :coll (:t m)) (= :present (length/position m i))) (:of m)
                           :else (t/union (elem-type ctx m) (or default-type t/never))))))
      :else (apply t/union (or default-type t/never)
                   (map (fn [m]
                          (cond (= :nil (:t m)) (or default-type t/nil-t)
                                (and (= :coll (:t m)) (= 0 (second (length/bounds m)))) t/never
                                :else (elem-type ctx m)))
                        members)))))

(doseq [[h [offset nullable?]] {'seq [0 true] 'next [1 true] 'rest [1 false]}]
  (defmethod special h [ctx node _]
    (sequence-tail ctx (infer ctx (first (args-of node))) offset nullable?)))

(doseq [h '[distinct shuffle set doall]]
  (defmethod special h [ctx node _]
    (let [[c] (args-of node)
          el (elem-type ctx (infer ctx c))]
      (coll-of (if (= 'set h) :set :seq) el))))

(doseq [h '[take drop take-last]]
  (defmethod special h [ctx node _]
    (let [[n c] (args-of node)]
      (infer ctx n)
      (coll-of :seq (elem-type ctx (infer ctx c))))))

(defn- partition-count [size step]
  (when (some? size)
    (cond
      (zero? size) 0
      (and step (pos? step)) (inc (quot (dec size) step)))))

(defn- partition-all-type [ctx source size step]
  (t/with-assumption
   (apply t/union
          (for [member (t/members (:env ctx) source)
                :let [[lo hi] (if (= :nil (:t member)) [0 0] (length/bounds member))
                      minimum (or (partition-count lo step) (min 1 lo))
                      maximum (partition-count hi step)
                      chunk (cond
                              (= 0 hi) t/never
                              (and size (not (pos? size))) (length/with-bounds (coll-of :seq t/never) 0 0)
                              :else (length/with-bounds (coll-of :seq (elem-type ctx member))
                                                        (if size 1 0)
                                                        (cond (and size hi) (min size hi) size size :else hi)))]]
            (length/with-bounds (coll-of :seq chunk) minimum maximum)))
   (t/assumption source)))

(defmethod special 'partition-all [ctx node _]
  (let [[n step-or-coll coll :as args] (args-of node)
        types (mapv #(infer ctx %) args)
        arity (count args)]
    (cond
      (not (#{1 2 3} arity))
      (do (report! ctx node "TSP2554" "partition-all expects 1, 2 or 3 arguments") t/unknown)

      (some #{t/never} types) t/never

      :else
      (let [env (:env ctx)
            value (fn [ty]
                    (let [v (:val (t/resolve-t env ty))]
                      (when (integer? v) v)))]
        (assignable! ctx n (first types) (t/prim :number))
        (when coll
          (assignable! ctx step-or-coll (second types) (t/prim :number)))
        (if (= 1 arity)
          t/unknown
          (let [source (eff ctx (peek types))
                size (value (first types))
                step (if coll (value (second types)) size)]
            (if-let [invalid (unsupported-sequence-member ctx source)]
              (do (report! ctx (last args) "TSP2345" (str "partition-all cannot sequence " (t/show invalid)))
                  t/unknown)
              (partition-all-type ctx source size step))))))))

(defn- interposed-length [size]
  (when (some? size)
    (if (zero? size) 0 (dec' (*' 2 size)))))

(defmethod special 'interpose [ctx node _]
  (let [[_ coll :as args] (args-of node)
        [separator-type source :as types] (mapv #(eff ctx (infer ctx %)) args)]
    (cond
      (not (#{1 2} (count args)))
      (do (report! ctx node "TSP2554" "interpose expects 1 or 2 arguments") t/unknown)

      (some #{t/never} types) t/never
      (nil? coll) t/unknown

      :else
      (if-let [invalid (unsupported-sequence-member ctx source)]
        (do (report! ctx coll "TSP2345" (str "interpose cannot sequence " (t/show invalid))) t/unknown)
        (t/with-assumption
         (apply t/union
                (for [member (t/members (:env ctx) source)
                      :let [element (elem-type ctx member)
                            [lo hi] (if (= t/never element) [0 0] (length/bounds member))
                            output (cond
                                     (= 0 hi) t/never
                                     (= 1 hi) element
                                     :else (t/union element separator-type))]]
                  (length/with-bounds (coll-of :seq output) (interposed-length lo) (interposed-length hi))))
         (t/assumption source))))))

(defn- sequence-member-bounds [member]
  (cond
    (= :nil (:t member)) [0 0]
    (and (= :lit (:t member)) (string? (:val member)))
    (let [size (count (:val member))] [size size])
    :else (length/bounds member)))

(defn- last-position-type [ctx member unbounded-result]
  (let [[lo hi] (sequence-member-bounds member)
        element (elem-type ctx member)]
    (cond
      (or (= 0 hi) (= t/never element)) t/nil-t
      (:items member) (peek (:items member))
      (pos? lo) element
      (nil? hi) (unbounded-result element)
      :else (t/union element t/nil-t))))

(defmethod special 'peek [ctx node _]
  (let [args (args-of node)
        [source :as types] (mapv #(eff ctx (infer ctx %)) args)]
    (if (not= 1 (count types))
      (do (report! ctx node "TSP2554" "peek expects 1 argument") t/unknown)
      (t/with-assumption
       (apply t/union
              (for [member (t/members (:env ctx) source)]
                (cond
                  (#{:nil :never} (:t member)) member
                  (vector-member? member) (last-position-type ctx member #(t/union % t/nil-t))
                  :else (indexed (elem-type ctx member)))))
       (t/assumption source)))))

(defmethod special 'last [ctx node _]
  (let [args (args-of node)
        [source :as types] (mapv #(eff ctx (infer ctx %)) args)]
    (cond
      (not= 1 (count types))
      (do (report! ctx node "TSP2554" "last expects 1 argument") t/unknown)

      (report-invalid-sequence-args! ctx 'last args types) t/unknown

      :else
      (t/with-assumption
       (apply t/union
              (for [member (t/members (:env ctx) source)]
                (if (#{:nil :never} (:t member))
                  member
                  (last-position-type ctx member indexed))))
       (t/assumption source)))))

(defn- trim-tail-type [ctx kind member n]
  (let [element (elem-type ctx member)
        [lo hi] (if (= t/never element) [0 0] (sequence-member-bounds member))
        minimum (if n (max 0 (-' lo n)) 0)
        maximum (if (and n hi) (max 0 (-' hi n)) hi)
        items (when (and n (:items member)) (subvec (:items member) 0 (int maximum)))
        output (cond (= 0 maximum) t/never
                     items (apply t/union items)
                     :else element)]
    (cond-> (length/with-bounds (coll-of kind output) minimum maximum)
      items (assoc :items items))))

(defmethod special 'butlast [ctx node _]
  (let [args (args-of node)
        [source :as types] (mapv #(eff ctx (infer ctx %)) args)]
    (cond
      (not= 1 (count types))
      (do (report! ctx node "TSP2554" "butlast expects 1 argument") t/unknown)

      (report-invalid-sequence-args! ctx 'butlast args types) t/unknown

      :else
      (t/with-assumption
       (apply t/union
              (for [member (t/members (:env ctx) source)]
                (if (= :never (:t member))
                  t/never
                  (let [prefix (trim-tail-type ctx :seq member 1)
                        [lo hi] (length/bounds prefix)]
                    (cond
                      (= 0 hi) t/nil-t
                      (pos? lo) prefix
                      :else (t/union (length/with-bounds prefix 1 hi) t/nil-t))))))
       (t/assumption source)))))

(defmethod special 'pop [ctx node _]
  (let [args (args-of node)
        [source :as types] (mapv #(eff ctx (infer ctx %)) args)]
    (if (not= 1 (count types))
      (do (report! ctx node "TSP2554" "pop expects 1 argument") t/unknown)
      (let [members (t/members (:env ctx) source)
            empty-vector? #(and (vector-member? %) (= 0 (second (length/bounds %))))]
        (when (some empty-vector? members)
          (report! ctx (first args) "TSP330-02"
                   (if (every? empty-vector? members) "pop cannot remove from an empty vector"
                       "pop may receive an empty vector")))
        (t/with-assumption
         (apply t/union
                (for [member members]
                  (cond
                    (#{:nil :never} (:t member)) member
                    (empty-vector? member) t/never
                    (vector-member? member) (trim-tail-type ctx :vector member 1)
                    :else t/unknown)))
         (t/assumption source))))))

(defmethod special 'drop-last [ctx node _]
  (let [[n-node coll :as args] (args-of node)
        types (mapv #(eff ctx (infer ctx %)) args)]
    (cond
      (not (#{1 2} (count args)))
      (do (report! ctx node "TSP2554" "drop-last expects 1 or 2 arguments") t/unknown)

      (some #{t/never} types) t/never

      :else
      (let [source (peek types)
            amount (if coll (:val (t/resolve-t (:env ctx) (first types))) 1)
            n (when (integer? amount) (max 0 amount))]
        (when coll
          (assignable! ctx n-node (first types) (t/prim :number)))
        (if-let [invalid (unsupported-sequence-member ctx source)]
          (do (report! ctx (peek args) "TSP2345" (str "drop-last cannot sequence " (t/show invalid))) t/unknown)
          (t/with-assumption
           (apply t/union
                  (map #(trim-tail-type ctx :seq % n) (t/members (:env ctx) source)))
           (t/assumption source)))))))

(defn- sequence-summary [ctx source]
  (let [parts (for [member (t/members (:env ctx) source)
                    :let [element (elem-type ctx member)
                          [lo hi] (if (= t/never element) [0 0] (sequence-member-bounds member))]]
                {:lower lo :upper hi
                 :element (if (= 0 hi) t/never element)})]
    {:lower (apply min (map :lower parts))
     :upper (when (every? :upper parts) (apply max (map :upper parts)))
     :element (t/with-assumption (apply t/union (map :element parts)) (t/assumption source))}))

(defmethod special 'concat [ctx node _]
  (let [args (args-of node)
        sources (mapv #(eff ctx (infer ctx %)) args)]
    (cond
      (some #{t/never} sources) t/never
      (report-invalid-sequence-args! ctx 'concat args sources) t/unknown
      :else
      (let [parts (mapv #(sequence-summary ctx %) sources)]
        (t/with-assumption
         (length/with-bounds (coll-of :seq (apply t/union (map :element parts)))
                             (reduce +' 0 (map :lower parts))
                             (when (every? :upper parts) (reduce +' 0 (map :upper parts))))
         (some t/assumption sources))))))

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

(doseq [h '[keep mapcat pmap]]
  (defmethod special h [ctx node _]
    (let [[f & colls] (args-of node)
          els (mapv #(elem-type ctx (infer ctx %)) colls)]
      (if (empty? colls)
        (do (infer ctx f) t/unknown)
        (let [ret (apply-fn-node ctx f els)
              ret (case h keep (t/remove-nil (:env ctx) ret) mapcat (elem-type ctx ret) ret)]
          (coll-of :seq ret))))))

(doseq [[head kind] {'map :seq 'mapv :vector}]
  (defmethod special head [ctx node _]
    (let [[f & colls] (args-of node)
          sources (mapv #(eff ctx (infer ctx %)) colls)]
      (cond
        (and f (= 'map head) (empty? colls))
        (do (infer ctx f) t/unknown)

        (empty? colls)
        (do (when f (infer ctx f))
            (report! ctx node "TSP2554" (if (= 'map head)
                                         "map expects at least one argument"
                                         "mapv expects a function and at least one collection"))
            t/unknown)

        (some #{t/never} sources)
        (do (infer ctx f) t/never)

        (report-invalid-sequence-args! ctx head colls sources)
        (do (infer ctx f) t/unknown)

        :else
        (let [parts (mapv #(sequence-summary ctx %) sources)
              minimum (apply min (map :lower parts))
              known-maxima (seq (keep :upper parts))
              maximum (when known-maxima (apply min known-maxima))
              element (if (= 0 maximum)
                        (do (infer ctx f) t/never)
                        (apply-fn-node ctx f (mapv :element parts)))]
          (t/with-assumption
           (length/with-bounds (coll-of kind element) minimum maximum)
           (some t/assumption sources)))))))

(defmethod special 'map-indexed [ctx node _]
  (let [[f coll :as args] (args-of node)]
    (case (count args)
      1 (do (infer ctx f) t/unknown)
      2 (let [source (eff ctx (infer ctx coll))
              invalid (unsupported-sequence-member ctx source)]
          (cond
            (= t/never source) (do (infer ctx f) t/never)
            invalid (do (infer ctx f)
                        (report! ctx coll "TSP2345" (str "map-indexed cannot sequence " (t/show invalid)))
                        t/unknown)
            :else (let [{:keys [lower upper element]} (sequence-summary ctx source)
                        result (if (= 0 upper)
                                 (do (infer ctx f) t/never)
                                 (apply-fn-node ctx f [(t/prim :int) element]))]
                    (t/with-assumption
                     (length/with-bounds (coll-of :seq result) lower upper)
                     (t/assumption source)))))
      (do (run! #(infer ctx %) args)
          (report! ctx node "TSP2554" "map-indexed expects 1 or 2 arguments")
          t/unknown))))

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

(doseq [[head kind] {'filter :seq 'remove :seq 'filterv :vector}]
  (defmethod special head [ctx node _]
    (let [[f coll :as args] (args-of node)
          arity (count args)]
      (cond
        (and (= arity 1) (= kind :seq))
        (do (infer ctx f) t/unknown)

        (not= arity 2)
        (do (run! #(infer ctx %) args)
            (report! ctx node "TSP2554" (str head (if (= kind :seq) " expects 1 or 2 arguments" " expects 2 arguments")))
            t/unknown)

        :else
        (let [source (eff ctx (infer ctx coll))]
          (if (= t/never source)
            (do (infer ctx f) t/never)
            (if-let [invalid (unsupported-sequence-member ctx source)]
              (do (infer ctx f)
                  (report! ctx coll "TSP2345" (str head " cannot sequence " (t/show invalid)))
                  t/unknown)
              (let [{:keys [element upper]} (sequence-summary ctx source)
                    selected (if (= 0 upper)
                               (do (infer ctx f) t/never)
                               (let [[_ yes no] (apply-predicate ctx f element true)]
                                 (if (= 'remove head) no yes)))
                    maximum (if (= t/never selected) 0 upper)]
                (t/with-assumption
                 (length/with-bounds {:t :coll :kind kind :of (dissoc selected :fresh)} 0 maximum)
                 (t/assumption source))))))))))

(defn- sequence-with-kind [ctx source kind reorder-items]
  (t/with-assumption
   (apply t/union
          (for [member (t/members (:env ctx) source)
                :let [element (elem-type ctx member)
                      [lo hi] (if (= t/never element) [0 0] (sequence-member-bounds member))
                      result (length/with-bounds (coll-of kind (if (= 0 hi) t/never element)) lo hi)]]
            (if-let [items (and reorder-items (:items member))]
              (assoc result :items (reorder-items items))
              result)))
   (t/assumption source)))

(defmethod special 'vec [ctx node _]
  (let [args (args-of node)
        [source :as types] (mapv #(eff ctx (infer ctx %)) args)]
    (cond
      (not= 1 (count types))
      (do (report! ctx node "TSP2554" "vec expects 1 argument") t/unknown)

      (= t/never source) t/never

      (report-invalid-sequence-args! ctx 'vec args types) t/unknown

      :else (sequence-with-kind ctx source :vector identity))))

(defmethod special 'reverse [ctx node _]
  (let [args (args-of node)
        [source :as types] (mapv #(eff ctx (infer ctx %)) args)]
    (cond
      (not= 1 (count types))
      (do (report! ctx node "TSP2554" "reverse expects 1 argument") t/unknown)

      (= t/never source) t/never

      :else
      (if-let [invalid (unsupported-sequence-member ctx source)]
        (do (report! ctx (first args) "TSP2345" (str "reverse cannot sequence " (t/show invalid))) t/unknown)
        (sequence-with-kind ctx source :seq (comp vec rseq))))))

(doseq [[head minimum] {'sort 1 'sort-by 2}]
  (defmethod special head [ctx node _]
    (let [args (args-of node)
          arity (count args)]
      (if-not (<= minimum arity (inc minimum))
        (do (run! #(infer ctx %) args)
            (report! ctx node "TSP2554" (str head " expects " minimum " or " (inc minimum) " arguments"))
            t/unknown)
        (let [key-fn (when (= 'sort-by head) (first args))
              comparator (when (= arity (inc minimum)) (infer ctx (nth args (dec minimum))))
              source-node (peek args)
              source (eff ctx (infer ctx source-node))
              invalid (unsupported-sequence-member ctx source)]
          (cond
            (or (= t/never source) (= t/never comparator))
            (do (when key-fn (infer ctx key-fn)) t/never)

            invalid
            (do (when key-fn (infer ctx key-fn))
                (report! ctx source-node "TSP2345" (str head " cannot sequence " (t/show invalid)))
                t/unknown)

            :else
            (do (when key-fn (apply-fn-node ctx key-fn [(elem-type ctx source)]))
                (sequence-with-kind ctx source :seq nil))))))))

(doseq [h '[take-while drop-while group-by some every? not-any? run! max-key min-key]]
  (defmethod special h [ctx node _]
    (let [[f & more] (args-of node)
          c (last more)]
      (if (nil? c)
        (do (infer ctx f) t/unknown)
        (let [el (elem-type ctx (infer ctx c))
              [ret yes] (apply-predicate ctx f el (= 'take-while h))]
          (case h
            take-while {:t :coll :kind :seq :of (dissoc yes :fresh)}
            drop-while (coll-of :seq el)
            (every? not-any?) (t/prim :boolean)
            some (t/union (t/remove-nil (:env ctx) ret) t/nil-t)
            run! t/nil-t
            (max-key min-key) el
            group-by (if (= t/never el)
                       empty-map-shape
                       {:t :map-of :k (widen ret)
                        :v (length/with-bounds (coll-of :vector el) 1 nil)})))))))

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

(defn- join-accumulator [ctx acc step]
  (if (and (= :coll (:t acc) (:t step)) (= (:kind acc) (:kind step)))
    (update (dissoc acc :items :min-count :max-count) :of t/union (:of step))
    (let [joined (t/union acc step)]
      (or (widen-vector-growth (:env ctx) joined) joined))))

(defn- reduction-accumulator [ctx f seed inputs empty-call?]
  (loop [acc seed iterations 0]
    (let [args (into [acc] inputs)
          calls (cond-> [] empty-call? (conj []) true (conj args))
          step (probe ctx #(reduction-result % f (reduction-function % f calls) args))]
      (cond
        (t/sub? (:env ctx) step acc) acc
        (= :unknown (:t step)) nil
        (= iterations 7) nil
        :else (recur (join-accumulator ctx acc (widen (dissoc step :fresh))) (inc iterations))))))

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
            invariant (if repeat? (reduction-accumulator ctx f start [el] empty-call?) start)
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

(defn- kv-reduction-input [member]
  (let [opaque {:bounds [0 nil] :key t/unknown :value t/unknown}]
    (case (:t member)
      (:nil :never) {:bounds [0 0]}
      :map-of {:bounds (length/bounds member) :key (:k member) :value (:v member)}
      :map (if (:complete-keys? member)
             (let [entries (concat (:req member) (:opt member))]
               {:bounds [(count (:req member)) (count entries)]
                :key (apply t/union (map (comp t/lit key) entries))
                :value (apply t/union (map val entries))})
             (assoc opaque :bounds [(count (:req member)) nil]))
      :coll (if (= :vector (:kind member))
              {:bounds (length/bounds member) :key (t/prim :int) :value (:of member)}
              opaque)
      :prim (cond-> opaque (= :vector (:name member)) (assoc :key (t/prim :int)))
      opaque)))

(doseq [[head part] {'keys :key 'vals :value}]
  (defmethod special head [ctx node _]
    (let [args (args-of node)
          [source :as types] (mapv #(eff ctx (infer ctx %)) args)]
      (if (not= 1 (count types))
        (do (report! ctx node "TSP2554" (str head " expects 1 argument")) t/unknown)
        (t/with-assumption
         (apply t/union
                (for [member (t/members (:env ctx) source)]
                  (case (:t member)
                    (:nil :never) member
                    (let [{[lo hi] :bounds :as entries} (when (#{:map :map-of} (:t member))
                                                        (kv-reduction-input member))
                          element (if (or (= :map-of (:t member)) (:complete-keys? member))
                                    (part entries) t/any)
                          minimum (or lo 0)]
                      (if (= 0 hi)
                        t/nil-t
                        (let [items (length/with-bounds {:t :coll :kind :seq :of element} (max 1 minimum) hi)]
                          (if (zero? minimum) (t/union items t/nil-t) items)))))))
         (t/assumption source))))))

(defmethod special 'reduce-kv [ctx node _]
  (let [[f init c :as args] (args-of node)]
    (if (not= 3 (count args))
      (do (run! #(infer ctx %) args)
          (report! ctx node "TSP2554" "reduce-kv expects 3 arguments")
          t/unknown)
      (let [initial (infer ctx init)
            ct (eff ctx (infer ctx c))
            entries (mapv kv-reduction-input (t/members (:env ctx) ct))
            active (remove #(= 0 (second (:bounds %))) entries)]
        (cond
          (or (= :never (:t initial)) (= :never (:t ct)))
          (do (infer ctx f) t/never)

          (empty? active)
          (do (infer ctx f) initial)

          :else
          (let [may-be-empty? (some #(zero? (first (:bounds %))) entries)
                repeat? (some #(let [maximum (second (:bounds %))] (or (nil? maximum) (> maximum 1))) active)
                inputs (mapv (fn [part]
                               (t/with-assumption (apply t/union (map part active)) (t/assumption ct)))
                             [:key :value])
                start (widen (dissoc initial :fresh))
                invariant (if repeat? (reduction-accumulator ctx f start inputs false) start)
                step-args (into [(or invariant start)] inputs)
                ty (reduction-function ctx f [step-args])
                step (reduction-result ctx f ty step-args)]
            (when (and (or (nil? invariant) (= :unknown (:t step)))
                       (or (empty? (:arities ty)) (seq (matching-arities ty 3))))
              (unchecked-hof! ctx f "reduce-kv accumulator is not checked" :accumulator))
            (cond
              (nil? invariant) t/unknown
              may-be-empty? (t/union initial step)
              :else step)))))))

(defn- append-to-vectors [ctx target vectors source]
  (let [{:keys [lower upper element]} (sequence-summary ctx source)]
    (t/with-assumption
     (if (= 0 upper)
       (dissoc target :fresh)
       (apply t/union
              (for [vector-type vectors
                    :let [[target-min target-max] (length/bounds vector-type)
                          elements (t/union (elem-type ctx vector-type) element)
                          result (length/with-bounds (coll-of :vector elements)
                                                     (+' target-min lower)
                                                     (when (and target-max upper) (+' target-max upper)))]]
                (cond-> result
                  (and (:items vector-type) (:items source))
                  (assoc :items (into (:items vector-type) (:items source)))))))
     (or (t/assumption target) (t/assumption source)))))

(defmethod special 'conj [ctx node _]
  (let [[c & xs] (args-of node)
        cty (infer ctx c)
        xts (map #(dissoc (infer ctx %) :fresh) xs)
        members (t/members (:env ctx) cty)
        m (first (remove #(= :nil (:t %)) members))]
    (cond
      (nil? c) (length/with-bounds (t/tuple []) 0 0)
      (= t/never cty) t/never
      (empty? xs) cty
      (every? vector-member? members)
      (let [items (vec xts)]
        (if (some #{t/never} items)
          t/never
          (append-to-vectors ctx cty members (t/tuple items))))
      (= :coll (:t m)) (assoc (dissoc m :items :min-count :max-count)
                             :of (apply t/union (:of m) (map widen xts)))
      :else cty)))

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
                 :coll (update (dissoc m :fresh :items :min-count :max-count) :of t/union out)
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
              source (eff ctx (infer ctx second-arg))
              members (t/members (:env ctx) target)]
          (cond (some #{t/never} [target source]) t/never
                (every? vector-member? members) (append-to-vectors ctx target members source)
                (and (= :vector (:op to)) (empty? (:children to))) (coll-of :vector (elem-type ctx source))
                (and (= :set (:op to)) (empty? (:children to))) (coll-of :set (elem-type ctx source))
                (some #(or (:items %) (length/constrained? %)) members)
                (into-transformed ctx target (elem-type ctx source))
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

(defn- initial-atom-content [ty]
  (case (:t ty)
    :map (t/prim :map)
    :coll (if (and (= :never (:t (:of ty))) (:kind ty))
            (t/prim (:kind ty))
            (dissoc ty :items :min-count :max-count :fresh))
    :union (apply t/union (map initial-atom-content (:types ty)))
    (widen (dissoc ty :fresh))))

(defn- atom-contents [ctx node ty]
  (let [members (t/members (:env ctx) (eff ctx ty))
        atoms (filter #(= :atom (:t %)) members)]
    (when (and (seq atoms) (every? #(#{:atom :nil} (:t %)) members))
      (when (some #(= :nil (:t %)) members)
        (assignable! ctx node ty (t/class-type "clojure.lang.Atom")))
      (vec (distinct (map :of atoms))))))

(defn- atom-write! [ctx node actual content]
  (binding [t/*strict-shapes* true]
    (let [resolved (t/resolve-t (:env ctx) content)]
      (if (= :unknown (:t resolved))
        (unchecked-hof! ctx node "atom content type is not checked" :atom-content)
        (assignable! ctx node actual content)))))

(defmethod special 'atom [ctx node _]
  (let [[init & options] (args-of node)
        content (:atom-content ctx)
        ctx (dissoc ctx :atom-content)]
    (if init
      (let [ty (infer-with-expected ctx init content)]
        (run! #(infer ctx %) options)
        (when content (atom-write! ctx init ty content))
        (when (odd? (count options))
          (report! ctx node "TSP2554" "atom expects key/value options after its initializer"))
        (if (= t/never ty) t/never {:t :atom :of (or content (initial-atom-content ty))}))
      (do (report! ctx node "TSP2554" "atom expects an initializer") t/unknown))))

(defmethod special 'deref [ctx node _]
  (let [[target & options :as args] (args-of node)
        ty (when target (infer ctx target))
        contents (when ty (atom-contents ctx target ty))]
    (run! #(infer ctx %) options)
    (cond
      (= t/never ty) t/never
      (and (seq contents) (empty? options)) (apply t/union contents)
      (or (not (#{1 3} (count args))) (seq contents))
      (do (report! ctx node "TSP2554" (if (seq contents) "deref of an atom expects one argument" "deref expects 1 or 3 arguments"))
          t/unknown)
      :else t/unknown)))

(defn- atom-write-contents [ctx target ty values?]
  (or (atom-contents ctx target ty)
      (let [expected (t/class-type (if values? "clojure.lang.IAtom2" "clojure.lang.IAtom"))]
        (when (= :mismatch (:status (t/sub-result (:env ctx) (eff ctx ty) expected)))
          (assignable! ctx target ty expected))
        nil)))

(defn- atom-value-pairs [contents after]
  (apply t/union (for [before (or (seq contents) [t/unknown])]
                   (t/tuple [before (or after before)]))))

(doseq [op '[reset! reset-vals! compare-and-set!]]
  (defmethod special op [ctx node _]
    (let [args (args-of node)
          compare? (= 'compare-and-set! op)
          values? (= 'reset-vals! op)
          arity (if compare? 3 2)
          types (mapv #(infer ctx %) args)]
      (cond
        (not= arity (count args))
        (do (report! ctx node "TSP2554" (str op " expects " arity " arguments")) t/unknown)

        (some #{t/never} types) t/never

        :else
        (let [contents (atom-write-contents ctx (first args) (first types) values?)
              actual (peek types)]
          ;; The old value in compare-and-set! is an identity test. Only the new value is written.
          (doseq [content contents] (atom-write! ctx (last args) actual content))
          (cond compare? (t/prim :boolean)
                values? (atom-value-pairs contents (dissoc actual :fresh))
                (seq contents) (apply t/union contents)
                :else t/unknown))))))

(doseq [op '[swap! swap-vals!]]
  (defmethod special op [ctx node _]
    (let [[target f & args :as all] (args-of node)
          values? (= 'swap-vals! op)]
      (if-not f
        (do (run! #(infer ctx %) all)
            (report! ctx node "TSP2554" (str op " expects an atom and an update function"))
            t/unknown)
        (let [ty (infer ctx target)
              contents (atom-write-contents ctx target ty values?)
              arg-types (mapv #(infer ctx %) args)]
          (if (seq contents)
            (let [current (apply t/union contents)
                  before (count @*diags*)
                  assoc? (= 'assoc (builtin-head ctx {:op :list :children [f]}))
                  result (if assoc?
                           (if (and (seq args) (even? (count args)))
                             (let [accepted (t/union (t/prim :map) (t/prim :vector) t/nil-t)]
                               (assignable! ctx f current accepted)
                               (assoc-entries ctx current (map vector (partition 2 args) (partition 2 arg-types))))
                             (do (report! ctx f "TSP2554" (str "assoc in " op " expects key/value arguments")) t/never))
                           (apply-fn-node ctx f (into [current] arg-types) true))]
              (if (or (= t/never result) (some #{t/never} arg-types))
                t/never
                (do (when (or (not= :unknown (:t result)) (= before (count @*diags*)))
                      (doseq [content contents] (atom-write! ctx f result content)))
                    (if values? (atom-value-pairs contents nil) current))))
            (do (infer ctx f)
                (cond (or (= t/never ty) (some #{t/never} arg-types)) t/never
                      values? (atom-value-pairs nil nil)
                      :else t/unknown))))))))

(defmethod special 'new [ctx node _]
  (let [[cls & args] (args-of node)]
    (run! #(infer ctx %) args)
    (if-let [c (and (= :sym (:op cls)) (t/resolve-class (:imports (:file ctx)) (:val cls)))]
      ;; A concrete Object constructor is not the declared Object/top escape.
      (if (= c "java.lang.Object") {:t :class :name c} (t/class-type c))
      t/unknown)))

;; ---- defn ----

(defn- check-return! [ctx node actual expected & [implementation]]
  (when-not (or (:generic-failure actual) (#{:unknown :any} (:t expected)))
    (let [env (:env ctx) actual (eff ctx actual)
          result (t/sub-result env actual expected)
          nested-error (when (= :mismatch (:status result))
                         (or (nested-platform-problem ctx "TSP2322" actual expected result)
                             (nested-type-problem ctx "TSP2322" result)))
          collection-error (when (= :mismatch (:status result)) (collection-problem ctx actual expected))
          emit (fn [[c m extra]]
                 (apply report! ctx node c (if implementation (str m " (" implementation ")") m)
                        (mapcat identity extra)))]
      (cond
        (= :proved (:status result)) (run! emit (problems ctx node actual expected))
        nested-error (emit nested-error)
        (seq (fresh-problems ctx actual expected)) (run! emit (fresh-problems ctx actual expected))
        (= :inconclusive (:status result)) (emit (incomplete-problem ctx actual expected result))
        collection-error (let [[code message] collection-error]
                      (emit [(if (= "TSP330-01" code) code "TSP2322") message]))
        (and (t/nilable? env actual) (not (t/nilable? env expected)) (t/sub? env (t/remove-nil env actual) expected))
        (emit ["TSP2322" (str "may return nil, but the signature says " (show-ref ctx expected)
                             (when-let [w (and (= :sym (:op node)) (get-in ctx [:why (:val node)]))]
                               (str ": " w)))])
        :else (if-let [[k opt?] (missing-req ctx actual expected)]
                (emit ["TSP2741" (str "the returned map has no required key " k " of " (t/show expected)
                                     (when opt? " (it is optional in the returned map)"))])
                (emit ["TSP2322" (str "returns " (show-ref ctx actual) ", but the signature says "
                                     (show-ref ctx expected))]))))))

(defn- spec-implementation-plans [bodies contracts coverage]
  (let [runtime (mapv (fn [{:keys [params]}]
                        (let [{:keys [fixed rest]} (env/split-params params)]
                          {:node params :params (vec (repeat (count fixed) t/unknown))
                           :rest (when rest t/unknown)}))
                      bodies)]
    (mapv #(t/dispatch-plan runtime % coverage) contracts)))

(defn- merge-implementation-contracts [n contracts]
  (when (seq contracts)
    {:params (mapv (fn [i] (apply t/union (map #(nth (:params %) i) contracts))) (range n))
     :tail (apply t/union
                  (for [{:keys [params rest] :as contract} contracts]
                    (if rest
                      (let [tail {:t :coll :kind :seq :of (apply t/union rest (drop n params))
                                  :min-count (max 1 (- (t/arity-minimum contract) n))}]
                        (if (= n (t/arity-minimum contract)) (t/union tail t/nil-t) tail))
                      (if (= n (count params))
                        t/nil-t
                        (assoc (t/tuple (subvec params n)) :kind :seq)))))
     :ret (apply t/union (map :ret contracts))}))

(defn- check-defn [ctx node]
  (let [file (:file ctx) env (:env ctx)
        parts (env/defn-parts node)
        nm (:name parts)
        sym (symbol (str (:ns file)) (str (:val nm)))
        v (get-in env [:vars sym])
        ftype (if (= :all (:t (:type v))) (solver/open-rigid (:type v)) (:type v))
        bodies (:arities parts)
        plans (when (and (= :fdef (:source v))
                         (not-any? #(= :map (:op (:rest (env/split-params (:params %))))) bodies))
                (spec-implementation-plans bodies (:arities ftype) :total))
        body-contracts (group-by (comp :node first) (mapcat identity plans))]
    (when-let [tn (env/tag-node nm)]
      (when (t/primitives (str (:val tn)))
        (report! ctx tn "TSP7021"
                 (str "a hint on the name is not a return type: write (defn " (:val nm) " ^" (:val tn) " [...] ...)"))))
    (when (and (:strict (:opts ctx)) (not (:private? parts)) (= :hints (:source v))
               (some (fn [a] (or (= :unknown (:t (:ret a))) (some #(= :unknown (:t %)) (:params a)))) (:arities ftype)))
      (report! ctx nm "TSP7010" (str "public function " (:val nm) " has no signature: add s/fdef, {:t/sig '[...]} or type hints")))
    (when (some nil? plans)
      (report! ctx nm "TSP2790" (str "implementation of " (:val nm) " does not accept every arity in " (t/show ftype))))
    (doseq [{:keys [params body]} bodies
            :let [{:keys [fixed rest]} (env/split-params params)
                  declared (if plans
                             (merge-implementation-contracts (count fixed) (map second (get body-contracts params)))
                             (or (first (filter #(and (= (count fixed) (count (:params %)))
                                                    (= (boolean rest) (boolean (or (:rest %) (:kwargs %)))))
                                              (:arities ftype)))
                                 (first (filter #(= (count fixed) (count (:params %))) (:arities ftype)))))]]
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
                         (:tail declared) (bind-pattern ctx' rest (:tail declared))
                         (:rest declared) (bind-pattern ctx' rest (t/union {:t :coll :kind :seq :of (:rest declared)} t/nil-t))
                         :else (bind-pattern ctx' rest t/unknown))
              ret-hint (env/hint-type file params)]
          (when (and ret-hint (not (#{:unknown :any} (:t (:ret declared))))
                     (not (t/sub? env ret-hint (:ret declared))) (not (t/sub? env (:ret declared) ret-hint)))
            (report! ctx (env/tag-node params) "TSP2322"
                     (str "hint " (t/show ret-hint) " contradicts the signature: " (t/show (:ret declared)))))
          (let [rty (infer-body ctx' body (:ret declared))]
            (when (seq body) (check-return! ctx' (last body) rty (:ret declared)))))))
    t/unknown))

(defmethod special 'defn [ctx node _] (check-defn ctx node))
(defmethod special 'defn- [ctx node _] (check-defn ctx node))

(defn- qualified-var [file sym]
  (let [resolved (env/resolve-sym file sym)]
    (if (qualified-symbol? resolved) resolved (symbol (str (:ns file)) (name resolved)))))

(defn- narrow-dispatch-params [ctx types {:keys [arity index key default]} dispatch]
  (if (and (= arity (count types)) (= :str (:op dispatch)) (literal-node? dispatch)
           (not= default (:val dispatch)))
    (let [original (nth types index)
          alternatives (path-alternatives ctx original [key])]
      (if (and (seq alternatives)
               (every? (fn [[leaf]] (and (= :lit (:t leaf)) (string? (:val leaf)))) alternatives))
        (let [matches (keep (fn [[leaf rebuild]]
                              (when (= (:val dispatch) (:val leaf)) (rebuild leaf)))
                            alternatives)]
          (if (seq matches)
            (assoc types index (t/with-assumption (apply t/union matches) (t/assumption original)))
            types))
        types))
    types))

(defmethod special 'defmethod [ctx node _]
  (let [{:keys [target dispatch local-name arities]} (env/defmethod-parts node)
        {:keys [env file]} ctx
        sym (when (and (= :sym (:op target)) (not (contains? (:locals ctx) (:val target))))
              (qualified-var file (:val target)))
        shared (get-in env [:vars sym])
        contracts (when (= :fdef (:source shared)) (get-in shared [:type :arities]))
        selector (get-in env [:multimethod-dispatches sym])
        plans (spec-implementation-plans arities contracts :partial)
        by-params (group-by (comp :node first) (mapcat identity plans))
        description (when contracts (str "defmethod " sym " " (parse/unparse dispatch) ", s/fdef"))
        ctx (cond-> (dissoc ctx :recur-types :load-time?)
              local-name (bind (:val local-name) t/unknown))]
    (when sym (record-var-use! ctx sym target :implementation))
    (doseq [{:keys [params body]} arities
            :let [{:keys [fixed rest]} (env/split-params params)
                  expected (merge-implementation-contracts (count fixed) (map second (get by-params params)))
                  types (or (:params expected) (mapv #(env/pattern-type env file %) fixed))
                  types (if (and expected selector (nil? rest))
                          (narrow-dispatch-params ctx types selector dispatch)
                          types)
                  method-ctx (reduce (fn [c [param type]]
                                       (when expected (record! c param type))
                                       (bind-pattern c param type))
                                     ctx (map vector fixed types))
                  method-ctx (if rest
                               (bind-pattern method-ctx rest
                                             (if (= :map (:op rest))
                                               (env/pattern-type env file rest)
                                               (or (:tail expected) t/unknown)))
                               method-ctx)
                  actual (infer-body method-ctx body)]
            :when expected]
      (check-return! method-ctx (or (last body) params) actual (:ret expected) description))
    t/unknown))

(defn- method-groups [forms target-ops]
  (reduce (fn [groups form]
            (cond
              (target-ops (:op form)) (conj groups [form []])
              (and (= :list (:op form)) (seq groups)) (update-in groups [(dec (count groups)) 1] conj form)
              :else groups))
          [] forms))

(defn- protocol-diagnostic! [ctx at code message]
  (report! ctx at code message :severity (if (:strict (:opts ctx)) :error :warning)))

(defn- check-protocol-body [ctx method fixed body params expected receiver]
  (let [types (or (:params expected) (mapv #(env/pattern-type (:env ctx) (:file ctx) %) fixed))
        types (if receiver (assoc types 0 receiver) types)
        method-ctx (reduce (fn [c [p ty]] (record! c p ty) (bind-pattern c p ty))
                           (dissoc ctx :recur-types :load-time?) (map vector fixed types))
        actual (infer-body method-ctx body)]
    (when expected
      (check-return! method-ctx (or (last body) params method) actual (:ret expected)))))

(defn- check-protocol-group [ctx protocol methods receiver form-kind]
  (let [full (qualified-var (:file ctx) (:val protocol))
        declared (get-in ctx [:env :protocols full])]
    (when declared
      (let [covered (volatile! #{})]
        (doseq [form methods
                :let [{method :name arities :arities}
                      (env/defn-parts (update form :children #(into [{:op :sym :val 'defn}] %)))
                      method-name (when (= :sym (:op method))
                                    ;; extend-type and extend-protocol key the method by its name.
                                    (if (= :reify form-kind) (:val method) (symbol (name (:val method)))))
                      {:keys [var] allowed :arities} (get declared method-name)]]
          (if-not (and var (seq arities))
            (report! ctx (or method form) "TSP430-01"
                     (str "invalid method " (parse/unparse method) " for protocol " full))
            (do
              (record-var-use! ctx var method :implementation)
              (doseq [{:keys [params body]} arities
                      :let [{:keys [fixed rest]} (env/split-params params)
                            n (count fixed)
                            method-arity [method-name n]
                            contracts (get-in ctx [:env :vars var :contract-arities])
                            contract (merge-implementation-contracts n (filter #(= n (count (:params %))) contracts))]]
                (cond
                  (or rest (not (contains? allowed n)))
                  (report! ctx params "TSP430-01" (str var " has no fixed arity " n " in " full))

                  (contains? @covered method-arity)
                  (report! ctx params "TSP430-01" (str "duplicate implementation of " var " with arity " n))

                  :else
                  (do (vswap! covered conj method-arity)
                      (when-not contract
                        (protocol-diagnostic! ctx method "TSP430-03" (str var " has no s/fdef for arity " n)))
                      (check-protocol-body ctx method fixed body params contract receiver)))))))
        (when (not= :extend-protocol form-kind)
          (doseq [[method {:keys [var arities]}] (sort-by key declared)
                  n arities :when (not (contains? @covered [method n]))]
            (protocol-diagnostic! ctx protocol "TSP430-02" (str "missing implementation of " var " with arity " n))))))))

(defmethod special 'reify [ctx node _]
  (doseq [[protocol methods] (method-groups (args-of node) #{:sym})]
    (check-protocol-group ctx protocol methods nil :reify))
  t/unknown)

(defn- extension-receiver [ctx target]
  (case (:op target)
    :nil t/nil-t
    :sym (some-> (t/resolve-class (get-in ctx [:file :imports]) (:val target)) t/class-type)
    nil))

(defmethod special 'extend-type [ctx node _]
  (let [[target & groups] (args-of node)
        receiver (extension-receiver ctx target)]
    (doseq [[protocol methods] (method-groups groups #{:sym})]
      (check-protocol-group ctx protocol methods receiver :extend-type)))
  t/nil-t)

(defmethod special 'extend-protocol [ctx node _]
  (let [[protocol & groups] (args-of node)]
    (when (= :sym (:op protocol))
      (doseq [[target methods] (method-groups groups #{:sym :nil})]
        (check-protocol-group ctx protocol methods (extension-receiver ctx target) :extend-protocol))))
  t/nil-t)

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
        (let [fields (when (= :map (:op node))
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

(defn- check-spec-macro [ctx node sym {:keys [spec index arity]} args]
  (if (= arity (count args))
    (doseq [[i arg] (map-indexed vector args)]
      (let [actual (infer ctx arg)]
        (when (= i index)
          (assignable! ctx arg actual (t/ref-t spec)))))
    (report! ctx node "TSP2554" (str sym " expects " arity " argument(s), got " (count args))))
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
      (nil? h) {:t :coll :kind :seq :of t/never :jvm-class "clojure.lang.IPersistentList"}
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
            spec-contract (when-not (:guards-disabled? ctx)
                            (get-in ctx [:env :spec-macro-contracts contract-sym]))
            r (cond contract (check-macro-body ctx node contract)
                    has-rule? (special ctx node head)
                    spec-contract (check-spec-macro ctx node contract-sym spec-contract args)
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
          (let [{:keys [type]} (read-var ctx sym h :call)]
            (if type
              (call-fn ctx node (str sym) type args)
              (check-unknown-call ctx args))))))))

(defn- literal-element-type [ctx types]
  (apply t/union (if (:preserve-literals? ctx) types (map widen types))))

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
                 :else (or (:type (read-var ctx s node :value)) t/unknown)))
    :vector (let [items (mapv #(dissoc (infer ctx %) :fresh) (:children node))]
              (t/tuple items (literal-element-type ctx items)))
    :set (let [items (:children node)
               size (count items)
               minimum (if (every? literal-node? items)
                         (count (set (map :val items)))
                         (if (zero? size) 0 1))]
           (length/with-bounds {:t :coll :kind :set :of (literal-element-type ctx (map #(infer ctx %) items))}
                               minimum size))
    :map (let [children (:children node)
               pairs (partition 2 children)
               static-keys (map static-map-key (map first pairs))]
           (when (:missing-directive-target (peek children))
             (literal-directives ctx (peek children)))
           (if (every? some? static-keys)
             {:t :map :fresh true :inferred true :complete-keys? true :opt {}
              :req (into (array-map)
                         (map (fn [[k v] kk]
                           (let [_ (when (some literal-directive? (:directives k)) (infer ctx k))
                                 vty (infer ctx v)]
                             (check-key-value! ctx kk v vty)
                             (when (and (qualified-keyword? kk) (not (get-in (:env ctx) [:specs kk]))
                                        (contains? (:key-nss (:env ctx)) (namespace kk))
                                        (or (:strict (:opts ctx)) (similar kk (keys (:specs (:env ctx))))))
                               (report! ctx k "TSP2304" (str "no spec for " kk (hint-suffix kk (keys (:specs (:env ctx)))))))
                             ;; a wrong value is reported once; after that the key has the type of its spec
                             [kk (if (and (qualified-keyword? kk) (get-in (:env ctx) [:specs kk])
                                          (not (t/sub? (:env ctx) (eff ctx vty) (t/ref-t kk))))
                                   (t/ref-t kk)
                                   (dissoc vty :fresh))]))
                              pairs static-keys))}
             (do (run! #(infer ctx %) (:children node)) (t/prim :map))))
    :quote (quoted-type (first (:children node)))
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
    "TSP295-01" "TSP295-02" "TSP330-01" "TSP330-02" "TSP410-01" "TSP430-01" "TSP430-02" "TSP430-03"
    "TSP2304" "TSP2305" "TSP2322" "TSP2339" "TSP2345" "TSP2353"
    "TSP2531" "TSP2554" "TSP2578" "TSP2678" "TSP2741" "TSP2790"
    "TSP7010" "TSP7020" "TSP7021" "TSP7023" "TSP7025" "TSP7031" "TSP7032" "TSP721-01" "TSP722-01"
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
        guard-env (assoc env0 :macro-contracts (:contracts compiled)
                             :spec-macro-contracts (macros/infer-spec-contracts env0 files)
                             :strict-nil (boolean (:strict-nil opts))
                             :nss (set (map :ns files)))
        guard-result (compile-guards guard-env opts)
        env (assoc guard-env :guards (:guards guard-result))
        guard-trust (count (filter #(= :trusted (:mode %)) (vals (:guards env))))]
    (binding [*diags* (atom (vec (map #(merge {:severity :error} %) (concat env-diags (:diagnostics guard-result)))))
              *types* (atom [])
              *var-uses* (when (:track-uses opts) (atom #{}))
              *spec-uses* (when (:track-uses opts) (atom #{}))
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
         :uses (vec (sort-by (juxt :file (comp :row :pos) (comp :col :pos) :var :kind) (when *var-uses* @*var-uses*)))
         :spec-uses (vec (sort-by (juxt :file (comp :row :pos) (comp :col :pos) :spec) (when *spec-uses* @*spec-uses*)))
         :sigs (into {} (for [[sym v] (:vars env)]
                          [sym (var-type {:env env :opts opts} sym)]))}))))
