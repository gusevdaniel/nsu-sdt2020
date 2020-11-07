(ns task-2-2-test
  (:use task-2-2.core)
  (:require [clojure.test :as test]))

; Ограничить до трех знаков после запятой
(defn format-value [value]
  (/ (Math/floor (* value 1000)) 1000))

(test/with-test (def integrator (get-integrator #(* % %) 0 0.01))
  (test/testing "task_2_2"
    (test/is (= (format-value (integrator 5)) 41.666))
    (test/is (= (format-value (integrator 10)) 333.333))))

(test/run-tests 'task-2-2-test)