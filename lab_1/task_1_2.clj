(ns task-1-2.core
  (:gen-class))

(defn connect_symbol_alphabet_rec 
  ([symbol alphabet acc]
  (if (> (count alphabet) 0)
    (if (= symbol (list (last (last (list (first alphabet))))))
      (recur symbol (rest alphabet) acc)
      (recur symbol (rest alphabet) (concat acc (list (concat (first alphabet) symbol)))))
    acc))
  ([symbol alphabet] (connect_symbol_alphabet_rec symbol alphabet '())))
;(connect_symbol_alphabet_rec '("d") '(("a" "b") ("b" "c") ("c" "d")))

(defn combine_alphabets_rec 
  ([alphabet original_alphabet acc]
   (if (> (count original_alphabet) 0)
     (recur alphabet (rest original_alphabet) (concat acc (connect_symbol_alphabet_rec (first original_alphabet) alphabet )))
     acc))
  ([alphabet original_alphabet] (combine_alphabets_rec alphabet original_alphabet '())))
;(combine_alphabets_rec '(("a") ("b") ("c")) '(("a") ("b") ("c")))

(defn count_n_rec
  ([alphabet original_alphabet n acc]
   (if (> n 1)
     (recur alphabet original_alphabet (dec n) (combine_alphabets_rec acc original_alphabet))
     acc))
  ([alphabet original_alphabet n] (count_n_rec alphabet original_alphabet n alphabet)))
;(count_n_rec '(("a") ("b") ("c")) '(("a") ("b") ("c")) 3)

(defn list_to_list_rec
  ([alphabet acc]
   (if (> (count alphabet) 0)
     (recur (rest alphabet) (concat acc (list (list (first alphabet)))))
     acc))
  ([alphabet] (list_to_list_rec alphabet '())))
;(list_to_list_rec '("a" "b" "c"))

(defn task_1_2 [alphabet n]
  (count_n_rec (list_to_list_rec alphabet) (list_to_list_rec alphabet) n))
;(task_1_2 '("a" 1 :b) 2)