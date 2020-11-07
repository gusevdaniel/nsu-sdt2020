(ns task-3-1.core
  (:gen-class))

(defn heavy [sleep f]
  (fn [& coll]
    (Thread/sleep sleep)
    (apply f coll)))

(defn delimiter [coll number-colls]
  (loop [new-coll coll, size-coll (/ (count coll) number-colls), result []]
    (if (> (count new-coll) 0)
      (recur (drop size-coll new-coll) size-coll (conj result (take size-coll new-coll)))
      result)))

(defn my-filter [f coll number-futures]
  (let [heavy-f (heavy 100 f)]
    (->>
     (delimiter coll number-futures)
     (map #(future (doall (filter heavy-f %))))
     (doall)
     (map deref)
     (doall)
     (apply concat))))

(defn -main []
  (time (my-filter (fn [n] (= 0 (mod n 3))) (range 1 (inc 10)) 1))
  (time (my-filter (fn [n] (= 0 (mod n 3))) (range 1 (inc 10)) 5))
  (time (my-filter (fn [n] (= 0 (mod n 3))) (range 1 (inc 10)) 10)))
(-main)