(ns task-3-2.core
  (:gen-class))

(defn heavy [sleep f]
  (fn [& coll]
    (Thread/sleep sleep)
    (apply f coll)))

(defn my-lazy-filter [f coll number-futures future_size]
   (lazy-cat
    (let [batch (take (* number-futures future_size) coll)]
      (->>
       batch
       (partition-all (/ (count batch) number-futures))
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
  (let [f (fn [n] (= 0 (mod n 3))) heavy-f (heavy 100 f) 
        coll (range 50) future_size 3 
        true-elements (count (filter f coll))]
    (println "For limited sequence")
    (time (doall (filter heavy-f coll)))
    (time (doall (my-lazy-filter heavy-f coll 1 future_size)))
    (time (doall (my-lazy-filter heavy-f coll 5 future_size)))
    (time (doall (my-lazy-filter heavy-f coll 10 future_size)))
    (println "For endless sequence")
    (time (doall (take true-elements (filter heavy-f (range)))))
    (time (doall (take true-elements (my-lazy-filter heavy-f (range) 1 future_size))))
    (time (doall (take true-elements (my-lazy-filter heavy-f (range) 5 future_size))))
    (time (doall (take true-elements (my-lazy-filter heavy-f (range) 10 future_size))))
    ))
(-main)