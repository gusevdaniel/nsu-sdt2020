(ns task-1-4.core
  (:gen-class))

(defn connect-symbol-alphabet [symbol alphabet]
  (map #(concat symbol %) (filter #(not= symbol [(first %)]) alphabet)))
;(connect-symbol-alphabet '("d") '(("a" "b") ("b" "c") ("d" "c")))

(defn combine-alphabets [alphabet original-alphabet]
  (reduce concat (map #(connect-symbol-alphabet % alphabet) original-alphabet)))
;(combine-alphabets '(("a") ("b") ("c")) '(("a") ("b") ("c")))

(defn count-n [alphabet original-alphabet n]
  (if (> n 1)
    (count-n (combine-alphabets alphabet original-alphabet) original-alphabet (dec n))
    alphabet))
;(count-n '(("a") ("b") ("c")) '(("a") ("b") ("c")) 3)

(defn list-to-list [alphabet]
  (map #(cons % '()) alphabet))
;(list-to-list '("a" "b" "c"))

(defn task-1-4 [alphabet n]
  (count-n (list-to-list alphabet) (list-to-list alphabet) n))
;(task-1-4 '("a" 1 :b) 2)