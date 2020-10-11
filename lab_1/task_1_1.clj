(ns my-stuff.core
  (:gen-class))

(defn connect-symbol-alphabet [symbol alphabet]
  (if (> (count alphabet) 0)
    (if (= symbol [(first (first alphabet))])
      (connect-symbol-alphabet symbol (rest alphabet))
      (cons (concat symbol (first alphabet)) (connect-symbol-alphabet symbol (rest alphabet))))
    ()))
;(connect-symbol-alphabet '("d") '(("a" "b") ("b" "c") ("d" "c")))

(defn combine-alphabets [alphabet original-alphabet]
  (if (> (count original-alphabet) 0)
    (concat (connect-symbol-alphabet (first original-alphabet) alphabet) (combine-alphabets alphabet (rest original-alphabet)))
    ()))
;(combine-alphabets '(("a") ("b") ("c")) '(("a") ("b") ("c")))

(defn count-n [alphabet original-alphabet n]
    (if (> n 1)
      (count-n (combine-alphabets alphabet original-alphabet) original-alphabet (dec n))
      alphabet))
;(count-n '(("a") ("b") ("c")) '(("a") ("b") ("c")) 3)

(defn list-to-list [alphabet]
  (if (> (count alphabet) 0)
    (cons (list (first alphabet)) (list-to-list (rest alphabet)))
    ()))
;(list-to-list '("a" "b" "c"))

(defn task-1-1 [alphabet n]
  (count-n (list-to-list alphabet) (list-to-list alphabet) n))
;(task-1-1 '("a" 1 :b) 2)