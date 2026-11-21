(ns typespec.solver
  "Pure, bounded rank-1 constraint solving. Fresh identity is local to each opening."
  (:require [typespec.length :as length]
            [typespec.types :as t]))

(defn substitute
  "Capture-free substitution by variable identity, visiting type positions only."
  [ty substitution]
  (let [s #(substitute % substitution)]
    (case (:t ty)
      (:var :ivar :rigid) (get substitution (:id ty) ty)
      :all (let [sub (apply dissoc substitution (map :id (:binders ty)))]
             (update ty :body #(substitute % sub)))
      :fn (update ty :arities #(mapv (fn [a] (cond-> (-> a (update :params (fn [ps] (mapv s ps))) (update :ret s))
                                               (:rest a) (update :rest s)
                                               (:kwargs a) (update :kwargs s))) %))
      :coll (cond-> (update ty :of s) (:items ty) (update :items #(mapv s %)))
      :map-of (-> ty (update :k s) (update :v s))
      :map (-> ty (update :req #(into {} (map (fn [[k v]] [k (s v)]) %)))
                 (update :opt #(into {} (map (fn [[k v]] [k (s v)]) %))))
      (:union :merge) (update ty :types #(mapv s %))
      ty)))

(defn- opening [all kind]
  (when-not (= :all (:t all)) (throw (ex-info "Expected all type" {:type all})))
  (let [vars (mapv #(assoc % :t kind :id (gensym (name kind))) (:binders all))]
    {:type (substitute (:body all) (zipmap (map :id (:binders all)) vars))
     :vars vars :constraints []}))

(defn instantiate [all] (opening all :ivar))
(defn open-rigid [all] (:type (opening all :rigid)))

(defn constrain
  "Append actual <: expected, preserving caller provenance."
  [state actual expected origin]
  (update state :constraints conj {:left actual :right expected :origin origin :path []}))

(defn- children [ty]
  (case (:t ty)
    :coll (or (:items ty) [(:of ty)])
    :map-of [(:k ty) (:v ty)]
    :fn (mapcat #(concat (:params %) [(:ret %)] (when (:rest %) [(:rest %)])) (:arities ty))
    (:union :merge) (:types ty)
    :map (concat (vals (:req ty)) (vals (:opt ty)))
    []))

(defn- uncertain? [ty]
  (or (#{:unknown :any :ivar :var :all} (:t ty)) (some uncertain? (children ty))))

(defn- opaque?
  "Contains unknown/any or an unopened variable: no evidence, unlike an ivar."
  [ty]
  (or (#{:unknown :any :var :all} (:t ty)) (some opaque? (children ty))))

(defn- ivars-in [ty]
  (if (= :ivar (:t ty)) #{(:id ty)} (into #{} (mapcat ivars-in (children ty)))))

(defn- lower-bounded-ivars
  "Variables a source could give a lower bound to through this target: the ones
  in output positions. Callback parameters only constrain a variable from above."
  [ty positive?]
  (case (:t ty)
    :ivar (if positive? #{(:id ty)} #{})
    :fn (into #{} (mapcat (fn [a]
                            (concat (mapcat #(lower-bounded-ivars % (not positive?))
                                            (concat (:params a) (when (:rest a) [(:rest a)])))
                                    (lower-bounded-ivars (:ret a) positive?)))
                          (:arities ty)))
    (into #{} (mapcat #(lower-bounded-ivars % positive?) (children ty)))))

(defn- absorb-literals
  "0 | int? is int?: a literal next to its own primitive adds nothing."
  [ty]
  (if-not (= :union (:t ty)) ty
    (let [prims (set (map t/strip (filter #(= :prim (:t %)) (:types ty))))]
      (apply t/union (remove #(and (= :lit (:t %)) (prims (t/lit-prim (:val %)))) (:types ty))))))

(defn- fixed? [ty]
  (and (= :fn (:t ty)) (= 1 (count (:arities ty)))
       (let [a (first (:arities ty))] (and (vector? (:params a)) (:ret a) (not (:rest a)) (not (:kwargs a))))))

(defn- normalize
  "Expand env :specs entries without trusting resolve-t's permissive unknown fallback.
  Both structural descent and reference expansion consume budget; seen is per branch.
  fuel (a volatile) bounds the total number of visited nodes of one solve: shared
  named maps expand as a tree, so depth alone does not bound the work."
  [env ty budget seen fuel]
  (let [ty (t/effective-type env ty)
        step #(normalize env % (dec budget) seen fuel)]
    (cond
      (neg? budget) (assoc t/unknown :solver-reason :depth)
      (neg? (vswap! fuel dec)) (assoc t/unknown :solver-reason :size)
      (= :ref (:t ty))
      (let [k (:name ty)]
        (cond
          (contains? seen k) (assoc t/unknown :solver-reason :ref-cycle)
          (nil? (get-in env [:specs k :type])) (assoc t/unknown :solver-reason :missing-ref)
          :else (normalize env (get-in env [:specs k :type]) (dec budget) (conj seen k) fuel)))
      :else
      (case (:t ty)
        :coll (if (:items ty)
                (let [items (mapv step (:items ty))] (assoc ty :items items :of (apply t/union items)))
                (update ty :of step))
        :map-of (-> ty (update :k step) (update :v step))
        :map (-> ty (update :req #(into {} (map (fn [[k v]] [k (step v)]) %)))
                   (update :opt #(into {} (map (fn [[k v]] [k (step v)]) %))))
        :fn (update ty :arities #(mapv (fn [a]
                                       (cond-> (-> a (update :params (fn [ps] (mapv step ps)))
                                                   (update :ret step))
                                         (:rest a) (update :rest step)
                                         (:kwargs a) (update :kwargs step))) %))
        (:union :merge) (update ty :types #(mapv step %))
        ty))))

(defn- normalize-constraint [env c budget fuel]
  (-> c (update :left #(normalize env % budget #{} fuel))
        (update :right #(normalize env % budget #{} fuel))))

(defn- obligations [c budget]
  (let [{l :left r :right path :path} c
        child (fn [a b p] (assoc c :left a :right b :path (conj path p)))
        recur-all (fn [cs] (mapcat #(obligations % (dec budget)) cs))]
    (cond
      (or (:failure c) (:mismatch c)) [c]
      (neg? budget) [(assoc c :failure :depth)]
      (or (= :ivar (:t l)) (= :ivar (:t r))) [c]
      (= :union (:t l)) (recur-all (map-indexed #(child %2 r [:union %1]) (:types l)))
      ;; l <: (closed... | alternative-with-a-variable), e.g. (s/nilable a).
      ;; Proved by the closed part: nothing left. Definitely outside it and one
      ;; open alternative: it must fit that one, which yields its lower bound.
      ;; Anything else needs disjunctive search: not checked, never a mismatch.
      (and (= :union (:t r)) (some #(seq (ivars-in %)) (:types r)) (not (opaque? l)))
      (let [{open true closed false} (group-by #(boolean (seq (ivars-in %))) (:types r))
            closed-status (cond
                            ;; a failed reference expansion is not a top type
                            (some opaque? closed) :inconclusive
                            (seq closed)
                            (:status (binding [t/*strict-shapes* true]
                                       (t/sub-result {} l (apply t/union closed)))))]
        (cond
          (= :proved closed-status) []
          (and (= 1 (count open)) (contains? #{nil :mismatch} closed-status))
          (recur-all [(child l (first open) [:union-right])])
          :else [(assoc c :failure :unsupported)]))
      (and (= :map (:t l)) (= :map (:t r)))
      (concat
       (mapcat (fn [[k v]]
                 (if (contains? (:req l) k)
                   (recur-all [(child (get-in l [:req k]) v [:key k])])
                   [(assoc (child t/never v [:key k]) :mismatch :missing-required-key)]))
               (:req r))
       (mapcat (fn [[k v]]
                 (let [fields (merge (:opt l) (:req l))]
                   (when (contains? fields k)
                     (recur-all [(child (get fields k) v [:key k])]))))
               (:opt r)))
      ;; Right unions need disjunctive search; leave them to concrete subtyping.
      (and (= :coll (:t l) (:t r))
           (or (= (:kind l) (:kind r)) (and (nil? (:kind r)) (nil? (:items r))))
           (or (nil? (:items r)) (:items l) (length/constrained? l)))
      (cond
        (not (length/subset? l r))
        [(assoc c :mismatch (if (:items r) :tuple-length :collection-length))]
        (:items r)
        (recur-all (map-indexed #(child (get (:items l) %1 (:of l)) %2 [:index %1]) (:items r)))
        (= 0 (:max-count l)) []
        :else (recur-all [(child (:of l) (:of r) :of)]))
      (and (fixed? l) (fixed? r)
           (= (count (:params (first (:arities l)))) (count (:params (first (:arities r))))))
      (let [a (first (:arities l)) b (first (:arities r))]
        (recur-all (concat (map-indexed #(child %2 (nth (:params a) %1) [:param %1]) (:params b))
                           [(child (:ret a) (:ret b) :ret)])))
      ;; Two single fixed arities of different length can never be compatible.
      (and (fixed? l) (fixed? r)) [(assoc c :mismatch :arity)]
      :else [c])))

(defn- supported? [ty]
  (and (contains? #{:prim :lit :nil :never :rigid :coll :union :fn :map} (:t ty))
       (or (not= :fn (:t ty)) (fixed? ty))
       (every? supported? (children ty))))

(defn- evidence [env c]
  (let [a (:left c) b (:right c)]
    (cond
      (:failure c) (assoc c :status :inconclusive :reasons #{(:failure c)})
      (:mismatch c) (assoc c :status :mismatch :reasons #{(:mismatch c)})
      ;; A declared any?/t/unknown target is top: everything fits. An unknown made
      ;; by a failed reference expansion carries :solver-reason and stays unproved.
      (and (#{:any :unknown} (:t b)) (not (:solver-reason b)))
      (assoc c :status :proved :reasons #{})
      (or (uncertain? a) (uncertain? b))
      (assoc c :status :inconclusive
             :reasons (into #{:unresolved}
                            (keep :solver-reason (mapcat #(tree-seq (comp seq children) children %) [a b]))))
      (or (not (supported? a)) (not (supported? b)))
      (assoc c :status :inconclusive :reasons #{:unsupported})
      ;; Strict shapes: vector? says nothing about elements at any depth, so it
      ;; is never proof of (s/coll-of T), also below a union or a map field.
      :else (merge c (binding [t/*strict-shapes* true] (t/sub-result env a b))))))

(defn solve
  "Solve a snapshot; unresolved variables remain variables, never arbitrary unknown.
  :budget in state (default 12) bounds reference normalization and decomposition.
  Named refs use env [:specs name :type]; missing/cyclic/deep refs are inconclusive.
  Keys maps require target required keys in source :req; present optional fields and
  required fields are covariant. Extra source fields are allowed. Returns partial
  substitution even on failure; final substituted evidence is normalized again.
  :max-nodes in state (default 50000) bounds the total expansion work; beyond it
  the result is inconclusive (:size). :uncertain is the set of variable ids whose
  candidate may be incomplete (an unknown/any lower bound, or evidence hidden in
  an undecomposed target): callers must not use those candidates as types."
  [env state]
  (let [budget (get state :budget 12)
        fuel (volatile! (get state :max-nodes 50000))
        cs (vec (mapcat #(obligations (normalize-constraint env % budget fuel) budget) (:constraints state)))
        vars (:vars state)
        ;; Repeated propagation resolves acyclic links without mutating the state.
        sub (loop [sub {} n (inc (count vars))]
              (let [next-sub
                    (reduce (fn [acc v]
                              (let [low (for [c cs :when (and (not (:failure c)) (not (:mismatch c)) (= (:id v) (get-in c [:right :id])) (= :ivar (get-in c [:right :t])))]
                                            (substitute (:left c) acc))
                                      good (remove uncertain? low)]
                                  (if (seq good) (let [candidate (absorb-literals (apply t/union good))
                                          canonical (if (= :union (:t candidate))
                                                      (update candidate :types #(vec (sort-by pr-str %))) candidate)]
                                      (assoc acc (:id v) canonical)) acc))) sub vars)]
                (if (or (zero? n) (= next-sub sub)) next-sub (recur next-sub (dec n)))))
        direct (into #{} (mapcat (fn [c]
                                   (let [r (:right c)]
                                     (if (= :ivar (:t r))
                                       (when (or (:failure c) (opaque? (:left c))) [(:id r)])
                                       (lower-bounded-ivars r true)))) cs))
        deps (reduce (fn [m c]
                       (if (and (= :ivar (get-in c [:right :t])) (not (:mismatch c)))
                         (update m (get-in c [:right :id]) (fnil into #{}) (ivars-in (:left c)))
                         m)) {} cs)
        uncertain (loop [bad direct]
                    (let [more (into bad (for [[v ws] deps
                                               :when (some #(or (bad %) (not (contains? sub %))) ws)] v))]
                      (if (= more bad) bad (recur more))))
        check (fn [c] (map #(evidence env %)
                           (obligations (normalize-constraint env c budget fuel) budget)))
        checks (mapcat #(check (-> % (update :left substitute sub) (update :right substitute sub))) cs)
        bounds (mapcat (fn [v]
                         (check {:left (get sub (:id v)) :right (:bound v) :origin :bound :path []}))
                       (filter #(and (get sub (:id %)) (not= :unknown (:t (:bound %)))) vars))
        unresolved (for [v vars :when (not (contains? sub (:id v)))]
                     {:status :inconclusive :reasons #{:unresolved-variable} :variable v})
        issues (vec (remove #(= :proved (:status %)) (concat checks bounds unresolved)))
        status (cond (some #(= :mismatch (:status %)) issues) :mismatch
                     (seq issues) :inconclusive :else :proved)]
    {:status status :substitution sub :uncertain uncertain :issues issues
     :type (substitute (:type state) sub)}))
