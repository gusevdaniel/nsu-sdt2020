(ns task-2-2.core
  (:gen-class))

(defn integral_flow [f step]
  (map first (iterate
              (fn [[sum x]] [(+ sum (* (* (+ (f x) (f (+ x step))) 0.5) step)) (+ x step)]) [0.0 0.0])))
;(nth (integral_flow #(* % %) 0.1) 50)

(defn integral [f x step]
  (nth (integral_flow f step) (/ x step)))
;(integral #(* % %) 5 0.1)

(defn -main []
  (let [f #(* % %) step 0.1]
    (time (integral f 5 step))
    (time (integral f 5 step))
    (time (integral f 6 step))))
(-main)