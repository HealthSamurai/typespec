(ns probe.atom-cache
  (:require [clojure.spec.alpha :as s]
            [uui.heroicons :as icons]))

(s/fdef snapshots :args (s/cat) :ret (s/tuple map? map?))
(defn snapshots [] (swap-vals! icons/cache assoc :checked "svg"))
(reset-vals! icons/cache {})
(compare-and-set! icons/cache :different {})

(reset-vals! icons/cache 42)
(swap-vals! icons/cache (fn [_] 42))
(compare-and-set! icons/cache @icons/cache 42)
