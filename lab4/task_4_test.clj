(ns task-4.core-test
  (:use task-4.core)
  (:require [clojure.test :as test]))

(test/deftest constant-test
  (test/testing
   (test/is (= (constant true) `(:task-4.core/const true)))
   (test/is (= (constant false) `(:task-4.core/const false)))
   (test/is (constant? `(:task-4.core/const true)))
   (test/is (constant? `(:task-4.core/const false)))
   (test/is (= (constant-value `(:task-4.core/const true)) true))
   (test/is (= (constant-value `(:task-4.core/const false)) false))))

(test/deftest variable-test
  (test/testing
    (test/is (= (task-4.core/variable :x) `(:task-4.core/var :x)))
    (test/is (task-4.core/variable? `(:task-4.core/var :x)))
    (test/is (not (task-4.core/variable? `(:task-4.core/const true))))
    (test/is (= (task-4.core/variable-name `(:task-4.core/var :x)) :x))
    (test/is (task-4.core/same-variables? `(:task-4.core/var :x) `(:task-4.core/var :x)))
    (test/is (not (task-4.core/same-variables? `(:task-4.core/const true) `(:task-4.core/var :x))))))

(test/deftest disjunction-test
  (test/testing
    (test/is (= (task-4.core/disjunction `(:task-4.core/const true)) `(:task-4.core/const true)))
    (test/is (= (task-4.core/disjunction `(:task-4.core/const true) `(:task-4.core/const true)) `(:task-4.core/or (:task-4.core/const true) (:task-4.core/const true))))
    (test/is (= (task-4.core/disjunction `(:task-4.core/const true) `(:task-4.core/const false)) `(:task-4.core/or (:task-4.core/const true) (:task-4.core/const false))))
    (test/is (= (task-4.core/disjunction `(:task-4.core/const true) `(:task-4.core/var :x) `(:task-4.core/var :y)) `(:task-4.core/or (:task-4.core/const true) (:task-4.core/var :x) (:task-4.core/var :y))))
    (test/is (task-4.core/disjunction? `(:task-4.core/or (:task-4.core/const true) (:task-4.core/const true))))
    (test/is (not (task-4.core/disjunction? `(:task-4.core/const true))))))

(test/deftest to-dnf-test
  (test/testing
   (test/is (= (task-4.core/to-dnf (conjunction (variable :a) (invert (disjunction (variable :c) (variable :d))))) `(:task-4.core/and
                                                                                                                     (:task-4.core/var :a)
                                                                                                                     (:task-4.core/not (:task-4.core/var :c))
                                                                                                                     (:task-4.core/not (:task-4.core/var :d)))))
   (test/is (= (task-4.core/to-dnf (invert (disjunction (implication (variable :x) (variable :y)) (invert (implication (variable :y) (variable :z)))))) `(:task-4.core/or
                                                                                                                                                          (:task-4.core/and (:task-4.core/var :x) (:task-4.core/not (:task-4.core/var :y)))
                                                                                                                                                          (:task-4.core/and (:task-4.core/var :x) (:task-4.core/not (:task-4.core/var :y)) (:task-4.core/var :z)))))))
(test/deftest print-dnf-test
  (test/testing
   (test/is (= (task-4.core/print-dnf `(:task-4.core/and
                                        (:task-4.core/var :a)
                                        (:task-4.core/not (:task-4.core/var :c))
                                        (:task-4.core/not (:task-4.core/var :d)))) "(a * !c * !d)"))
   (test/is (= (task-4.core/print-dnf `(:task-4.core/or
                                        (:task-4.core/and (:task-4.core/var :x) (:task-4.core/not (:task-4.core/var :y)))
                                        (:task-4.core/and (:task-4.core/var :x) (:task-4.core/not (:task-4.core/var :y)) (:task-4.core/var :z)))) "((x * !y) + (x * !y * z))"))))

(test/run-tests 'task-4.core-test)