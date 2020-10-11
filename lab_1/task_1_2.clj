(ns task-1-2.core
  (:gen-class))

(defn connect-symbol-alphabet-rec 
  ([symbol alphabet acc]
  (if (> (count alphabet) 0)
    (if (= symbol [(first (first alphabet))])
      (recur symbol (rest alphabet) acc)
      (recur symbol (rest alphabet) (concat acc (list (concat symbol (first alphabet))))))
    acc))
  ([symbol alphabet] (connect-symbol-alphabet-rec symbol alphabet '())))
;(connect-symbol-alphabet-rec '("d") '(("a" "b") ("b" "c") ("d" "c")))

(defn combine-alphabets-rec 
  ([alphabet original-alphabet acc]
   (if (> (count original-alphabet) 0)
     (recur alphabet (rest original-alphabet) (concat acc (connect-symbol-alphabet-rec (first original-alphabet) alphabet )))
     acc))
  ([alphabet original-alphabet] (combine-alphabets-rec alphabet original-alphabet '())))
;(combine-alphabets-rec '(("a") ("b") ("c")) '(("a") ("b") ("c")))

(defn count-n-rec
  ([alphabet original-alphabet n acc]
   (if (> n 1)
     (recur alphabet original-alphabet (dec n) (combine-alphabets-rec acc original-alphabet))
     acc))
  ([alphabet original-alphabet n] (count-n-rec alphabet original-alphabet n alphabet)))
;(count-n-rec '(("a") ("b") ("c")) '(("a") ("b") ("c")) 3)

(defn list-to-list-rec
  ([alphabet acc]
   (if (> (count alphabet) 0)
     (recur (rest alphabet) (concat acc (list (list (first alphabet)))))
     acc))
  ([alphabet] (list-to-list-rec alphabet '())))
;(list-to-list-rec '("a" "b" "c"))

(defn task-1-2 [alphabet n]
  (count-n-rec (list-to-list-rec alphabet) (list-to-list-rec alphabet) n))
;(task-1-2 '("a" 1 :b) 2)