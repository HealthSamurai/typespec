(ns typespec.length
  "Cardinality intervals for immutable collections. A nil upper bound is unbounded.")

(defn bounds [ty]
  (if-let [items (:items ty)]
    [(count items) (count items)]
    [(get ty :min-count 0) (:max-count ty)]))

(defn constrained? [ty]
  (or (contains? ty :min-count) (contains? ty :max-count)))

(defn with-bounds [ty lower upper]
  (cond-> (dissoc ty :min-count :max-count)
    (pos? lower) (assoc :min-count lower)
    (some? upper) (assoc :max-count upper)))

(defn subset? [source target]
  (let [[lo hi] (bounds source)
        [minimum maximum] (bounds target)]
    (and (>= lo minimum)
         (or (nil? maximum) (and (some? hi) (<= hi maximum))))))

(defn position [ty index]
  (let [[lower upper] (bounds ty)]
    (cond
      (or (neg? index) (and upper (>= index upper))) :absent
      (< index lower) :present
      :else :possible)))

(defn describe [ty]
  (let [[lower upper] (bounds ty)]
    (cond (= lower upper) (str lower)
          (nil? upper) (str "at least " lower)
          (zero? lower) (str "at most " upper)
          :else (str lower " to " upper))))
