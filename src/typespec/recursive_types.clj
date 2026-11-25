(ns typespec.recursive-types
  "Regular recursive specs and structural subtyping over a finite graph."
  (:require [clojure.string :as str]
            [typespec.length :as length]))

(def ^:private empty-queue clojure.lang.PersistentQueue/EMPTY)
(def ^:dynamic *max-obligations* 8192)

(defn- structural-children [ty]
  (case (:t ty)
    :map (concat (vals (:req ty)) (vals (:opt ty))
                 (when-let [index (:map-index ty)] [(:k index) (:v index)]))
    :coll (or (:items ty) [(:of ty)])
    :union (:types ty)
    []))

(defn- references [ty]
  (into #{} (keep #(when (= :ref (:t %)) (:name %)))
        (tree-seq coll? #(if (= :coll (:t %)) (structural-children %) (seq %)) ty)))

(defn- unguarded-references [ty]
  (into #{} (keep #(when (= :ref (:t %)) (:name %)))
        (tree-seq #(= :union (:t %)) :types ty)))

(defn- reverse-graph [graph]
  (reduce-kv (fn [result from tos]
               (reduce #(update %1 %2 (fnil conj #{}) from) result tos))
             {} graph))

(defn- reachable [graph starts blocked]
  (loop [pending (into empty-queue starts) seen #{}]
    (if-let [node (peek pending)]
      (let [pending (pop pending)]
        (if (or (contains? seen node) (contains? blocked node))
          (recur pending seen)
          (recur (into pending (graph node)) (conj seen node))))
      seen)))

(defn- finish-order [graph]
  (second
   (reduce (fn [[seen order] start]
             (loop [stack [[start false]] seen seen order order]
               (if-let [[node finished?] (peek stack)]
                 (let [stack (pop stack)]
                   (cond
                     finished? (recur stack seen (conj order node))
                     (contains? seen node) (recur stack seen order)
                     :else (recur (into (conj stack [node true]) (map #(vector % false) (graph node)))
                                  (conj seen node) order)))
                 [seen order])))
           [#{} []] (keys graph))))

(defn- cyclic-components [graph reverse-edges]
  (loop [order (rseq (finish-order graph)) seen #{} cycles []]
    (if-let [node (first order)]
      (if (contains? seen node)
        (recur (next order) seen cycles)
        (let [component (reachable reverse-edges [node] seen)
              members (sort component)]
          (recur (next order) (into seen component)
                 (cond-> cycles
                   (or (next members) (contains? (graph node) node)) (conj members)))))
      (sort-by first cycles))))

(defn- cycle-witness [graph component]
  (let [start (first component)
        within? (set component)]
    (loop [queue (conj empty-queue [start]) seen #{}]
      (let [path (peek queue)
            node (peek path)
            successors (filter within? (sort (graph node)))]
        (if (some #{start} successors)
          (conj path start)
          (recur (into (pop queue) (map #(conj path %)) (remove (conj seen node) successors))
                 (conj seen node)))))))

(defn- opaque-shape? [ty]
  (let [data (meta ty)]
    (or (:typespec.types/unchecked-contract data)
        (:typespec.types/shape-conflicts data)
        (= 's/keys (:typespec.types/shape-form data)))))

(defn- finite? [specs inhabitants ty optimistic?]
  (if (opaque-shape? ty)
    optimistic?
    (case (:t ty)
      :ref (if (contains? specs (:name ty)) (contains? inhabitants (:name ty)) optimistic?)
      :never false
      (:nil :prim :lit :any) true
      :map (every? #(finite? specs inhabitants % optimistic?) (vals (:req ty)))
      :coll (if-let [items (:items ty)]
              (every? #(finite? specs inhabitants % optimistic?) items)
              (or (zero? (first (length/bounds ty))) (finite? specs inhabitants (:of ty) optimistic?)))
      :union (some #(finite? specs inhabitants % optimistic?) (:types ty))
      optimistic?)))

(defn- productive-specs [specs reverse-edges optimistic?]
  (loop [queue (into empty-queue (keys specs)) productive #{}]
    (if-let [k (peek queue)]
      (let [queue (pop queue)]
        (if (or (contains? productive k) (not (finite? specs productive (get-in specs [k :type]) optimistic?)))
          (recur queue productive)
          (recur (into queue (reverse-edges k)) (conj productive k))))
      productive)))

(defn analyze [specs]
  (let [raw-references (update-vals specs #(references (:type %)))
        graph-of (fn [refs]
                   (update-vals specs #(into #{} (filter (fn [k] (contains? specs k))) (refs (:type %)))))
        graph (update-vals raw-references #(into #{} (filter (fn [k] (contains? specs k))) %))
        reverse-edges (reverse-graph graph)
        cycles (cyclic-components graph reverse-edges)]
    (if (empty? cycles)
      {:reachable #{} :invalid {} :diagnostics []}
      (let [unguarded (graph-of unguarded-references)
            bare-cycles (cyclic-components unguarded (reverse-graph unguarded))
            bare-names (set (mapcat identity bare-cycles))
            productive (productive-specs specs reverse-edges false)
            possible (productive-specs specs reverse-edges true)
            no-base (remove #(or (some bare-names %) (every? productive %)) cycles)
            recursive (reachable reverse-edges (mapcat identity cycles) #{})
            referenced (reachable graph recursive #{})
            issue (fn [component code severity message]
                    (merge (select-keys (specs (first component)) [:file :pos])
                           {:code code :severity severity :message message}))
            base-reason (fn [k] (if (contains? possible k) :unknown-recursive-base :unproductive-recursion))]
        {:reachable recursive
         :invalid (into (zipmap bare-names (repeat :unguarded-recursion))
                        (for [component no-base k component :when (not (contains? productive k))]
                          [k (base-reason k)]))
         :diagnostics (into
                       (mapv (fn [component]
                               (issue component "TSP295-01" :error
                                      (str "unguarded recursive spec: "
                                           (str/join " -> " (cycle-witness unguarded component)))))
                             bare-cycles)
                       (concat
                        (for [component no-base
                              :let [unchecked (remove productive component)]]
                          (issue unchecked "TSP295-02" :warning
                                 (str (if (some possible unchecked)
                                        "finite base not proved for recursive spec: "
                                        "recursive spec has no finite base: ")
                                      (str/join ", " unchecked))))
                        (for [k (sort referenced)
                              :let [missing (sort (remove #(contains? specs %) (raw-references k)))]
                              :when (and (seq missing) (not= :extern (:source (specs k))))]
                          (issue [k] "TSP9003" :error
                                 (str "spec " k " refers to undefined types: " (str/join ", " missing))))))}))))

(def ^:private supported-heads #{:ref :union :map :coll :class :prim :nil :lit :any :unknown :never})

(defn contains-structural? [pred & types]
  (some pred (mapcat #(tree-seq (comp seq structural-children) structural-children %) types)))

(defn candidate? [env a b]
  (let [recursive (get-in env [:recursive-types :reachable])]
    (and (seq recursive)
         (contains-structural? #(and (= :ref (:t %)) (contains? recursive (:name %))) a b))))

(def ^:private proved {:status :proved})
(defn- unknown-proof [reason] {:status :inconclusive :reasons #{reason}})
(defn- mismatch [a b] {:status :mismatch :actual a :expected b})

(defn- map-obligation [a b strict-contracts?]
  (let [required (sort (keys (:req b)))
        optional (sort (keys (:opt b)))
        source-field #(or (get (:req a) %) (get (:opt a) %))]
    (if-let [missing (first (remove #(contains? (:req a) %) required))]
      (assoc (mismatch a b) :path [missing] :missing-key missing)
      {:all (into (mapv (fn [k] [k (get (:req a) k) (get (:req b) k)]) required)
                  (for [k optional
                        :let [source (or (source-field k) (when strict-contracts? {:t :any}))]
                        :when source]
                    [k source (get (:opt b) k)]))})))

(defn- collection-obligation [a b leaf]
  (let [actual (:items a)
        expected (:items b)]
    (cond
      (and expected (nil? actual) (not (length/constrained? a))) (leaf a b)
      (and expected (not= (:kind a) (:kind b))) (mismatch a b)

      (and (not expected) (:kind b) (not= (:kind a) (:kind b))) (mismatch a b)
      (not (length/subset? a b)) (assoc (mismatch a b) :length? true)
      actual {:all (mapv (fn [i ty] [i ty (if expected (nth expected i) (:of b))]) (range) actual)}
      expected {:all (mapv (fn [i ty] [i (:of a) ty]) (range) expected)}
      (= 0 (:max-count a)) proved
      :else {:all [[::element (:of a) (:of b)]]})))

(defn- head-kind [specs ty]
  (loop [ty ty seen #{}]
    (if (= :ref (:t ty))
      (let [name (:name ty)]
        (when-not (seen name)
          (recur (get-in specs [name :type]) (conj seen name))))
      (:t ty))))

(defn- obligation [env a b leaf strict-contracts?]
  (let [st (:t a)
        tt (:t b)
        specs (:specs env)
        invalid (get-in env [:recursive-types :invalid])]
    (cond
      (or (= :ref st) (= :ref tt))
      (let [name (:name (if (= :ref st) a b))]
        (if-let [reason (get invalid name)]
          (unknown-proof reason)
          (if-let [type (get-in specs [name :type])]
            {:all [(if (= :ref st) [nil type b] [nil a type])]}
            (unknown-proof :missing-ref))))

      (or (not (supported-heads st)) (not (supported-heads tt))
          (opaque-shape? a) (opaque-shape? b))
      (unknown-proof :unsupported-recursive-type)
      (= :union st) {:all (mapv #(vector nil % b) (:types a))}
      (= :union tt) {:any (mapv #(vector nil a %) (:types b))
                     :preferred (keep-indexed #(when (= st (head-kind specs %2)) %1) (:types b))}
      (and (= :map st) (= :map tt))
      (if (or (:map-index a) (:map-index b))
        (unknown-proof :unsupported-recursive-type)
        (map-obligation a b strict-contracts?))
      (and (= :coll st) (= :coll tt)) (collection-obligation a b leaf)
      :else (leaf a b))))

(defn- build-obligations [env a b leaf strict-contracts? effective]
  (let [ids (java.util.IdentityHashMap.)
        expansions (java.util.IdentityHashMap.)
        expand (fn [ty]
                 (if (and (:strict-nil env) (:platform ty))
                   (or (.get expansions ty)
                       (let [expanded (effective ty)]
                         (.put expansions ty expanded)
                         expanded))
                   ty))
        id (fn [ty]
             (or (.get ids ty)
                 (let [id (.size ids)]
                   (.put ids ty id)
                   id)))
        pair-key (fn [s t] [(id s) (id t)])
        a (expand a)
        b (expand b)
        root (pair-key a b)]
    (loop [pending (conj empty-queue [root a b]) graph {} reverse-edges {} fuel *max-obligations*]
      (if-let [[key a b] (peek pending)]
        (let [pending (pop pending)]
          (if (contains? graph key)
            (recur pending graph reverse-edges fuel)
            (let [node (obligation env a b leaf strict-contracts?)
                  edges (or (:all node) (:any node))
                  fuel (- fuel 1 (count edges))]
              (when (not (neg? fuel))
                (let [children (mapv (fn [[step s t]]
                                       (let [s (expand s) t (expand t)]
                                         [step (pair-key s t) s t]))
                                     edges)
                      graph (assoc graph key (if edges
                                               (let [edges (mapv #(subvec % 0 2) children)]
                                                 (cond-> {:op (if (:all node) :all :any) :edges edges}
                                                   (seq (:preferred node))
                                                   (assoc :explain-edges (mapv edges (:preferred node)))))
                                               node))]
                  (recur (into pending (map #(subvec % 1) children)) graph
                         (reduce (fn [reverse [_ child]] (update reverse child (fnil conj #{}) key))
                                 reverse-edges children)
                         fuel))))))
        {:root root :graph graph :dependents reverse-edges}))))

(def ^:private rank {:mismatch 0 :inconclusive 1 :proved 2})

(defn- solve-obligations [graph dependents]
  ;; Guarded recursion starts with a provisional proof. Propagate terminal
  ;; failures and incomplete checks through the graph before publishing it.
  (loop [queue (into empty-queue (keys graph)) states {}]
    (if-let [key (peek queue)]
      (let [{:keys [op edges status]} (graph key)
            inputs (map #(get states (second %) 2) edges)
            result (case op :all (reduce min 2 inputs) :any (reduce max 0 inputs) (rank status))]
        (if (< result (get states key 2))
          (recur (into (pop queue) (dependents key)) (assoc states key result))
          (recur (pop queue) states)))
      states)))

(defn- explain [root graph states answer]
  (loop [queue (conj empty-queue [root []]) seen #{} fallback nil reasons #{}]
    (if-let [[key path] (peek queue)]
      (let [queue (pop queue)
            {:keys [status edges explain-edges] :as node} (graph key)]
        (cond
          (contains? seen key) (recur queue seen fallback reasons)
          status (let [path (into path (:path node))]
                   (if (and (zero? answer) (seq path))
                     (assoc node :path path)
                     (recur queue (conj seen key) (or fallback node) (into reasons (:reasons node)))))
          :else (recur (into queue (for [[step next] (if (and (zero? answer) (seq explain-edges)) explain-edges edges)
                                       :when (= answer (get states next 2))]
                                     [next (cond-> path (some? step) (conj step))]))
                       (conj seen key) fallback reasons)))
      (if (zero? answer) (or fallback {:status :mismatch}) {:status :inconclusive :reasons reasons}))))

(defn compare-types [env a b leaf strict-contracts? effective]
  (if-let [{:keys [root graph dependents]} (build-obligations env a b leaf strict-contracts? effective)]
    (let [states (solve-obligations graph dependents)
          answer (get states root 2)]
      (if (= 2 answer) proved (explain root graph states answer)))
    (unknown-proof :recursive-budget)))
