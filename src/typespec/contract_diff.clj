(ns typespec.contract-diff
  "Compare public source contracts against an earlier revision."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [typespec.project-contracts :as project]
            [typespec.spec-conform :as conform]
            [typespec.types :as t]))

(defn- opaque [reason]
  (assoc t/unknown :contract-reason reason))

(defn- resolve-shape [env ty]
  (let [budget (volatile! 512)]
    (letfn [(shape [ty active]
              (let [data (meta ty)]
                (cond
                  (neg? (vswap! budget dec)) (opaque :shape-budget)
                  (t/assumption ty) (opaque :assumed-type)
                  (or (::t/unchecked-contract data) (::t/shape-conflicts data)
                      (= 's/keys (::t/shape-form data))) (opaque :unchecked-spec)
                  (:platform ty) {:t :union :types [t/nil-t (shape (t/strip ty) active)]}
                  :else
                  (let [child #(shape % active)]
                    (case (:t ty)
                      :ref (let [name (:name ty)
                                 target (get-in env [:specs name :type])]
                             (cond
                               (active name) (opaque :recursive-spec)
                               target (shape target (conj active name))
                               :else (opaque :missing-spec)))
                      :unknown (opaque :unknown-type)
                      :prim (case (:name ty)
                              :boolean (t/union (t/lit true) (t/lit false))
                              :map {:t :map :req {} :opt {}}
                              (t/strip ty))
                      :class (if (t/load-class (:name ty))
                               (t/strip ty)
                               (opaque :missing-class))
                      (:any :nil :never :lit) (t/strip ty)
                      :union (update ty :types #(mapv child %))
                      :map (-> ty
                               (update :req #(update-vals % child))
                               (update :opt #(update-vals % child)))
                      :atom (update ty :of child)
                      :coll (if-let [items (:items ty)]
                              (let [items (mapv child items)]
                                (assoc ty :items items :of (apply t/union items)))
                              (update ty :of child))
                      (opaque :unsupported-type))))))]
      (shape ty #{}))))

(defn- view-arities [env type]
  (when (and (= :fn (:t type)) (seq (:arities type)) (not-any? :kwargs (:arities type)))
    (mapv (fn [arity]
            (cond-> (-> (select-keys arity [:params :rest :rest-min :ret])
                        (update :params #(mapv (partial resolve-shape env) %))
                        (update :ret #(resolve-shape env %)))
              (:rest arity) (update :rest #(resolve-shape env %))))
          (:arities type))))

(defn- proof-issue [result kind message]
  (case (:status result)
    :proved nil
    :mismatch {:status :breaking :kind kind :message message}
    :inconclusive {:status :inconclusive :kind kind
                   :reasons (vec (sort (:reasons result))) :message message}))

(defn- parameter-type [arity i]
  (get (:params arity) i (:rest arity)))

(defn- parameter-proof [before after n]
  (let [comparisons (mapv #(t/contract-sub-result (parameter-type before %) (parameter-type after %)) (range n))]
    (or (first (filter #(= :mismatch (:status %)) comparisons))
        (first (filter #(= :inconclusive (:status %)) comparisons))
        {:status :proved})))

(defn- sample-call [before after n]
  (let [candidates (filterv #(t/arity-fits? % n) after)
        wrap #(when % (assoc % :arity n))]
    (wrap
     (case (count candidates)
       0 {:status :breaking :kind :arity :message (str "no longer accepts " n " argument(s)")}
       1 (let [candidate (first candidates)]
           (or (proof-issue (parameter-proof before candidate n)
                            :arguments "previous argument types are not covered")
               (proof-issue (t/contract-sub-result (:ret candidate) (:ret before))
                            :return "return type does not preserve the previous guarantee")))
       (if (and (some #(= :proved (:status (parameter-proof before % n))) candidates)
                (every? #(= :proved (:status (t/contract-sub-result (:ret %) (:ret before)))) candidates))
         nil
         {:status :inconclusive :kind :overload :reasons [:overlapping-arities]
          :message "compatibility between overlapping overloads was not proved"})))))

(defn- compare-callables [before after]
  (if (and (seq before) (seq after))
    (let [;; Once all fixed slots and arity minima are past, only homogeneous
          ;; rest slots can change. One sample beyond the largest minimum covers them.
          limit (inc (apply max (map t/arity-minimum (concat before after))))]
      (if (> limit 65)
        [{:status :inconclusive :kind :signature :reasons [:arity-budget]
          :message "argument arity exceeds the comparison budget"}]
        (->> (for [arity before
                   n (if (:rest arity)
                       (range (t/arity-minimum arity) (inc limit))
                       [(t/arity-minimum arity)])
                   :when (not-any? #(= :never (:t (parameter-type arity %))) (range n))
                   :let [issue (sample-call arity after n)]
                   :when issue]
               issue)
             (reduce (fn [issues issue]
                       (if (some #(= ((juxt :status :kind) %) ((juxt :status :kind) issue)) issues)
                         issues
                         (conj issues issue))) []))))
    [{:status :inconclusive :kind :signature :reasons [:missing-callable-contract]
      :message "a positional callable contract is unavailable"}]))

(defn- declared-entry [res sym]
  (let [entry (get-in res [:env :vars sym])
        opaque (get-in res [:env :opaque-fdefs sym])]
    (when-not (:private? entry)
      (cond
        (= :sig (:source entry)) entry
        opaque (assoc opaque :source :fdef)
        (or (= :fdef (:source entry)) (and (= :hints (:source entry)) (:tagged? entry))) entry))))

(defn- declaration [revision entry]
  (when-let [file (:file entry)]
    (let [prefix (some-> (:root revision) (str "/"))]
      {:file (if (and prefix (str/starts-with? file prefix)) (subs file (count prefix)) file)
       :pos (:pos entry)})))

(defn- signature-description [revision entry index]
  (when (or entry index)
    {:signature (if entry (some-> entry :type t/show) (:signature index))
     :declaration (or (declaration revision (:contract entry))
                      (declaration revision entry)
                      (:declaration index))}))

(defn- spec-description [revision entry]
  (when entry
    (cond-> {:type (some-> entry :type t/show) :declaration (declaration revision entry)}
      (:spec-form entry) (assoc :form (pr-str (:spec-form entry))))))

(defn- changed-specs [before after prefix before-type after-type]
  (let [specs-before (get-in before [:env :specs])
        specs-after (get-in after [:env :specs])
        paths (merge (project/dependency-paths specs-after after-type)
                     (project/dependency-paths specs-before before-type))
        contract-data (juxt :type :spec-form #(select-keys (meta (:type %)) [::t/unchecked-contract ::t/spec-tags]))]
    (vec (for [[k path] (sort-by key paths)
               :let [old (get specs-before k) new (get specs-after k)]
               :when (not= (contract-data old) (contract-data new))]
           {:path (into prefix (map str) path)
            :before (spec-description before old)
            :after (spec-description after new)}))))

(defn- compare-vars [before after old-index new-index]
  (let [vars (sort (set/union (set (keys (get-in before [:env :vars])))
                              (set (keys (get-in before [:env :opaque-fdefs])))))]
    (for [sym vars
          :let [prior (declared-entry before sym)]
          :when prior
          :let [next (declared-entry after sym)
                old (get old-index (str sym))
                new (get new-index (str sym))
                current-var (get-in after [:env :vars sym])
                exists? (contains? (get-in after [:env :ns-defs (symbol (namespace sym))])
                                   (symbol (name sym)))
                issues (if (and (nil? next)
                                (or (:private? current-var)
                                    (and (nil? current-var) (not exists?))))
                         [{:status :breaking :kind :removed-var :message "public callable was removed"}]
                         (compare-callables (view-arities (:env before) (:type prior))
                                            (view-arities (:env after) (:type next))))]
          issue issues]
      (assoc issue :var (str sym)
             :before (signature-description before prior old)
             :after (signature-description after (or next current-var) new)
             :spec-changes (changed-specs before after [(str sym)] (:type prior) (:type next))
             :uses (vec (or (seq (:uses new)) (:uses old)))))))

(defn- tagged-payloads [ty]
  (vec (for [arm (t/members {} ty) :when (:map-entry? arm)]
         [(get-in arm [:items 0 :val]) (get-in arm [:items 1])])))

(defn- conform-issue [env-before env-after before after]
  (if-not after
    {:status :inconclusive :kind :conform :reasons [:unknown-conform]
     :message "previously tagged conformation no longer has a known tagged result"}
    (let [old-branches (tagged-payloads before)
          new-branches (tagged-payloads after)
          old-tags (mapv first old-branches)
          new-tags (mapv first new-branches)
          added (sort (set/difference (set new-tags) (set old-tags)))
          removed (sort (set/difference (set old-tags) (set new-tags)))]
      (cond
        (or (seq added) (seq removed))
        {:status :breaking :kind :conform-tags :added-tags (mapv str added) :removed-tags (mapv str removed)
         :message "s/or conformation tags changed"}
        (not= old-tags new-tags)
        {:status :inconclusive :kind :conform-tags :reasons [:tag-order]
         :message "s/or branch order changed"}
        :else
        (let [issues (keep (fn [[[tag old] [_ new]]]
                             (proof-issue (t/contract-sub-result (resolve-shape env-after new)
                                                                (resolve-shape env-before old))
                                          :conform (str "conformed payload for " tag
                                                        " does not preserve the previous guarantee")))
                           (map vector old-branches new-branches))]
          (or (first (filter #(= :breaking (:status %)) issues)) (first issues)))))))

(defn- compare-conform [before after]
  (let [old-env (:env before)
        new-env (:env after)
        old-uses (group-by :spec (:spec-uses before))
        new-uses (group-by :spec (:spec-uses after))
        affected (reduce (fn [by-spec {:keys [var specs]}]
                           (reduce (fn [m {:keys [name]}]
                                     (update m name (fnil conj []) var))
                                   by-spec specs))
                         {} (:contracts after))]
    (for [[k spec] (sort-by key (:specs old-env))
          :when (:spec-context spec)
          :let [old (conform/tagged-result old-env nil k)]
          :when old
          :let [current (get-in new-env [:specs k])
                new (conform/tagged-result new-env nil k)
                issue (if current
                        (conform-issue old-env new-env old new)
                        {:status :breaking :kind :removed-spec :message "named tagged spec was removed"})]
          :when issue]
      (assoc issue :spec (str k)
             :before {:type (t/show old) :declaration (declaration before spec)}
             :after (when current {:type (when new (t/show new)) :declaration (declaration after current)})
             :spec-changes (changed-specs before after [] (t/ref-t k) (when current (t/ref-t k)))
             :affected-vars (get affected (str k) [])
             :uses (mapv #(dissoc % :spec) (or (seq (get new-uses k)) (get old-uses k)))))))

(defn compare-projects [before after]
  (let [index (fn [r] (into {} (map (juxt :var identity)) (:contracts r)))]
    (vec (concat (compare-vars before after (index before) (index after))
                 (compare-conform before after)))))
