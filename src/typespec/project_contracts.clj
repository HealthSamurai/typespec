(ns typespec.project-contracts
  "Assemble public contract declarations, referenced specs, and source uses after a full check."
  (:require [typespec.spec-conform :as conform]
            [typespec.types :as t]))

(defn- references [ty]
  (into #{} (keep #(when (and (map? %) (= :ref (:t %))) (:name %)))
        (tree-seq coll? seq ty)))

(defn dependency-paths
  "Shortest paths from a signature to each referenced spec."
  [specs ty]
  (loop [pending (into clojure.lang.PersistentQueue/EMPTY (map vector (sort (references ty))))
         paths (sorted-map)]
    (if-let [path (peek pending)]
      (let [k (peek path)
            pending (pop pending)]
        (if (contains? paths k)
          (recur pending paths)
          (recur (into pending (map #(conj path %) (sort (references (get-in specs [k :type])))))
                 (assoc paths k path))))
      paths)))

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
                               (keys (dependency-paths specs signature)))
                  :uses (mapv #(assoc (location relative %) :kind (:kind %)) (get by-var sym))}
           protocol (assoc :protocol (str protocol))))))))
