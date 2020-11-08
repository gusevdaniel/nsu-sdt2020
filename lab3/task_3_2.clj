(ns task-3-2.core
  (:gen-class))

(defn heavy [sleep f]
  (fn [& coll]
    (Thread/sleep sleep)
    (apply f coll)))

(defn delimiter [number-colls coll]
  (loop [new-coll coll, size-coll (/ (count coll) number-colls), result []]
    (if (> (count new-coll) 0)
      (recur (drop size-coll new-coll) size-coll (conj result (take size-coll new-coll)))
      result)))

(defn my-lazy-filter [f coll number-futures future_size]
   (lazy-cat
    (let [batch (take (* number-futures future_size) coll)]
      (->>
       batch
       (delimiter number-futures)
       (map #(future (doall (filter f %))))
       (doall)
       (map deref)
       (doall)
       (apply concat)))
    (if (> (count (take future_size coll)) 0)
      (my-lazy-filter f (drop (* number-futures future_size) coll) number-futures future_size)
      ())))
;(take 10 (my-lazy-filter (fn [n] (= 0 (mod n 3))) (range) 10 5))

(defn -main []
  (let
   [f (fn [n] (= 0 (mod n 3))) heavy-f (heavy 100 f) coll (range 30)]
    (println "For limited sequence")
    (time (doall (filter heavy-f coll)))
    (time (doall (my-lazy-filter heavy-f coll 1 5)))
    (time (doall (my-lazy-filter heavy-f coll 5 5)))
    (time (doall (my-lazy-filter heavy-f coll 10 5)))
    (println "For endless sequence")
    (time (doall (take 10 (filter heavy-f (range)))))
    (time (doall (take 10 (my-lazy-filter heavy-f (range) 1 5))))
    (time (doall (take 10 (my-lazy-filter heavy-f (range) 5 5))))
    (time (doall (take 10 (my-lazy-filter heavy-f (range) 10 5))))
    ))
(-main)