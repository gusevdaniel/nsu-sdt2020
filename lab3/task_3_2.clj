(ns task-3-2.core
  (:gen-class))

(defn heavy [sleep f]
  (fn [& coll]
    (Thread/sleep sleep)
    (apply f coll)))

(defn lazy-delimiter [size-coll coll]
  (lazy-cat
   (if (> (count (take size-coll coll)) 0)
     (cons (take size-coll coll) (lazy-delimiter size-coll (drop size-coll coll)))
     ())))
;(lazy-delimiter 2 (range 10))

(defn perform-batch [f]
  (fn [coll]
    (->>
     coll
     (map #(future (doall (filter f %))))
     (doall)
     (map deref)
     (apply concat)
     (lazy-seq))))

(defn my-lazy-filter [f coll size_chank size_batch]
  (->>
   coll
   (lazy-delimiter size_chank)
   (lazy-delimiter size_batch)
   (map #((perform-batch f) %))
   (apply concat)))
;(take 20 (my-lazy-filter (fn [n] (= 0 (mod n 3))) (range) 5 10))

(defn -main []
  (let [f (fn [n] (= 0 (mod n 3))) heavy-f (heavy 100 f)
        coll (range 50) size_chank 3
        true-elements (count (filter f coll))]
    (println "For limited sequence")
    (time (doall (filter heavy-f coll)))
    (time (doall (my-lazy-filter heavy-f coll size_chank 1)))
    (time (doall (my-lazy-filter heavy-f coll size_chank 5)))
    (time (doall (my-lazy-filter heavy-f coll size_chank 10)))
    (println "For endless sequence")
    (time (doall (take true-elements (filter heavy-f (range)))))
    (time (doall (take true-elements (my-lazy-filter heavy-f (range) size_chank 1))))
    (time (doall (take true-elements (my-lazy-filter heavy-f (range) size_chank 5))))
    (time (doall (take true-elements (my-lazy-filter heavy-f (range) size_chank 10))))))