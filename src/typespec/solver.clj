(ns typespec.solver
  "Pure, bounded rank-1 constraint solving. Fresh identity is local to each opening."
  (:require [typespec.types :as t]))

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
      :coll (update ty :of s)
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
    :coll [(:of ty)]
    :map-of [(:k ty) (:v ty)]
    :fn (mapcat #(concat (:params %) [(:ret %)] (when (:rest %) [(:rest %)])) (:arities ty))
    (:union :merge) (:types ty)
    :map (concat (vals (:req ty)) (vals (:opt ty)))
    []))

(defn- uncertain? [ty]
  (or (#{:unknown :any :ivar :var :all} (:t ty)) (some uncertain? (children ty))))

(defn- fixed? [ty]
  (and (= :fn (:t ty)) (= 1 (count (:arities ty)))
       (let [a (first (:arities ty))] (and (vector? (:params a)) (:ret a) (not (:rest a)) (not (:kwargs a))))))

(defn- obligations [c budget]
  (let [{l :left r :right path :path} c
        child (fn [a b p] (assoc c :left a :right b :path (conj path p)))
        recur-all (fn [cs] (mapcat #(obligations % (dec budget)) cs))]
    (cond
      (neg? budget) [(assoc c :failure :depth)]
      (or (= :ivar (:t l)) (= :ivar (:t r))) [c]
      (= :union (:t l)) (recur-all (map-indexed #(child %2 r [:union %1]) (:types l)))
      ;; Right unions need disjunctive search; leave them to concrete subtyping.
      (and (= :coll (:t l)) (= :coll (:t r))
           (or (nil? (:kind r)) (= (:kind l) (:kind r))))
      (recur-all [(child (:of l) (:of r) :of)])
      (and (fixed? l) (fixed? r)
           (= (count (:params (first (:arities l)))) (count (:params (first (:arities r))))))
      (let [a (first (:arities l)) b (first (:arities r))]
        (recur-all (concat (map-indexed #(child %2 (nth (:params a) %1) [:param %1]) (:params b))
                           [(child (:ret a) (:ret b) :ret)])))
      :else [c])))

(defn- supported? [ty]
  (and (contains? #{:prim :lit :nil :never :rigid :coll :union :fn} (:t ty))
       (or (not= :fn (:t ty)) (fixed? ty))
       (every? supported? (children ty))))

(defn- evidence [env c]
  (let [a (:left c) b (:right c)]
    (cond
      (:failure c) (assoc c :status :inconclusive :reasons #{(:failure c)})
      (or (uncertain? a) (uncertain? b)) (assoc c :status :inconclusive :reasons #{:unresolved})
      (or (not (supported? a)) (not (supported? b))
          (and (= :prim (:t a)) (= :coll (:t b))))
      (assoc c :status :inconclusive :reasons #{:unsupported})
      :else (merge c (t/sub-result env a b)))))

(defn solve
  "Solve a snapshot; unresolved variables remain variables, never arbitrary unknown.
  :budget in state bounds decomposition. Returns partial substitution even on failure."
  [env state]
  (let [cs (vec (mapcat #(obligations % (get state :budget 12)) (:constraints state)))
        vars (:vars state)
        ;; Repeated propagation resolves acyclic links without mutating the state.
        sub (loop [sub {} n (inc (count vars))]
              (let [next-sub
                    (reduce (fn [acc v]
                              (let [low (for [c cs :when (and (not (:failure c)) (= (:id v) (get-in c [:right :id])) (= :ivar (get-in c [:right :t])))]
                                            (substitute (:left c) acc))
                                      good (remove uncertain? low)]
                                  (if (seq good) (let [candidate (apply t/union good)
                                          canonical (if (= :union (:t candidate))
                                                      (update candidate :types #(vec (sort-by pr-str %))) candidate)]
                                      (assoc acc (:id v) canonical)) acc))) sub vars)]
                (if (or (zero? n) (= next-sub sub)) next-sub (recur next-sub (dec n)))))
        checks (mapv #(evidence env (-> % (update :left substitute sub) (update :right substitute sub))) cs)
        bounds (for [v vars :when (and (get sub (:id v)) (not= :unknown (:t (:bound v))))]
                 (evidence env {:left (get sub (:id v)) :right (:bound v) :origin :bound :path []}))
        unresolved (for [v vars :when (not (contains? sub (:id v)))]
                     {:status :inconclusive :reasons #{:unresolved-variable} :variable v})
        issues (vec (remove #(= :proved (:status %)) (concat checks bounds unresolved)))
        status (cond (some #(= :mismatch (:status %)) issues) :mismatch
                     (seq issues) :inconclusive :else :proved)]
    {:status status :substitution sub :issues issues :type (substitute (:type state) sub)}))
