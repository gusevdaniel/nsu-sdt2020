(ns my-stuff.core
  (:gen-class))

(defn connect_symbol_alphabet [symbol alphabet]
    (if (> (count alphabet) 0)
      (if (= symbol (list (last (last (list (first alphabet))))))
        (connect_symbol_alphabet symbol (rest alphabet))
        (cons (concat (first alphabet) symbol) (connect_symbol_alphabet symbol (rest alphabet))))
      ()))
;(connect_symbol_alphabet '("d") '(("a" "b") ("b" "c") ("c" "d")))

(defn combine_alphabets [alphabet original_alphabet]
  (if (> (count original_alphabet) 0)
    (concat (connect_symbol_alphabet (first original_alphabet) alphabet) (combine_alphabets alphabet (rest original_alphabet)))
    ()))
;(combine_alphabets '(("a") ("b") ("c")) '(("a") ("b") ("c")))

(defn count_n [alphabet original_alphabet n]
    (if (> n 1)
      (count_n (combine_alphabets alphabet original_alphabet) original_alphabet (dec n))
      alphabet))
;(count_n '(("a") ("b") ("c")) '(("a") ("b") ("c")) 3)

(defn list_to_list [alphabet]
  (if (> (count alphabet) 0)
    (cons (list (first alphabet)) (list_to_list (rest alphabet)))
    ()))
;(list_to_list '("a" "b" "c"))

(defn task_1_1 [alphabet n]
  (count_n (list_to_list alphabet) (list_to_list alphabet) n))
;(task_1_1 '("a" 1 :b) 2)