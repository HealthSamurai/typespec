(ns typespec.project-contracts
  "Assemble public contract declarations, referenced specs, and source uses after a full check."
  (:require [typespec.spec-conform :as conform]
            [typespec.types :as t]))

(defn- references [ty]
  (into #{} (keep #(when (and (map? %) (= :ref (:t %))) (:name %)))
        (tree-seq coll? seq ty)))

(defn- dependencies [specs ty]
  (loop [pending (seq (references ty)) seen #{}]
    (if-let [k (first pending)]
      (if (contains? seen k)
        (recur (next pending) seen)
        (recur (concat (references (get-in specs [k :type])) (next pending)) (conj seen k)))
      (sort seen))))

(defn- location [relative {:keys [file pos]}]
  (when file {:file (relative file) :pos pos}))

(defn index [{:keys [env sigs uses]} relative]
  (let [by-var (group-by :var uses)
        specs (:specs env)]
    (binding [t/*cur-ns* nil]
      (vec
       (for [[sym {:keys [private? protocol source contract] :as entry}] (sort-by key (:vars env))
             :when (not private?)
             :let [signature (get sigs sym (:type entry))]]
         (cond-> {:var (str sym)
                  :source source
                  :signature (t/show signature)
                  :declaration (or (location relative contract) (location relative entry))
                  :specs (mapv (fn [key]
                                 (let [spec (get specs key)
                                       output (conform/tagged-result env nil key)]
                                   (cond-> {:name (str key) :type (some-> spec :type t/show)
                                            :declaration (location relative spec)}
                                     output (assoc :conform (t/show output)))))
                               (dependencies specs signature))
                  :uses (mapv #(assoc (location relative %) :kind (:kind %)) (get by-var sym))}
           protocol (assoc :protocol (str protocol))))))))
