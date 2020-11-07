(ns task-2-1.core
  (:gen-class))

(defn trapezoid [f x0 x1]
  (* (+ (f x0) (f x1)) 1/2))
;(trapezoid #(* % %) 1 2)

(defn integral [f x0 x1 n]
  (let [step (/ (- x1 x0) n)]
  (loop [start x0 acc 0]
    (if (< start x1)
      (recur (+ start step) (+ acc (trapezoid f start (+ start step))))
      (* acc step)))))
;(integral #(* % %) 0 5 50)

(def integral-memo
  (memoize
   (fn [f x0 x1 step]
     (if (< x0 x1)
         (+ (integral-memo f (+ x0 step) x1 step) (* (* (+ (f x0) (f (+ x0 step))) 1/2) step))
         (- (* (* (+ (f x0) (f x1)) 1/2) step))))))
;(integral-memo #(* % %) 0 5 0.1)

(defn -main []
  (let [f #(* % %) x0 0 x1 5]
  (time (integral f x0 x1 50))
  (time (integral-memo f x0 x1 0.1))
  (time (integral-memo f x0 x1 0.1))
  (time (integral-memo f x0 x1 0.1))))
(-main)