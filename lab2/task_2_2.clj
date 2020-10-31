(ns task-2-2.core
  (:gen-class))

(defn create-seq [f a step]
(map first (iterate
            (fn [[sum x]] [(+ sum (* (* (+ (f x) (f (+ x step))) 0.5) step)) (+ x step)]) [0.0 a])))

(defn get-integrator [f a step]
  (let [seq (create-seq f a step)]
    (fn [b]
      (nth seq (/ b step)))))

(defn -main []
  (let [integrator (get-integrator #(* % %) 0 0.01)]
    (time (integrator 500))
    (time (integrator 500))
    (time (integrator 501))
    (time (integrator 520))
    ))
(-main)