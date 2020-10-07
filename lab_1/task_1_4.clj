(ns task-1-4.core
  (:gen-class))

(defn connect_symbol_alphabet [symbol alphabet]
  (map #(concat % symbol) (filter #(not= symbol (list (last (last (list %))))) alphabet)))
;(connect_symbol_alphabet '("d") '(("a" "b") ("b" "c") ("c" "d")))

(defn combine_alphabets [alphabet original_alphabet]
  (reduce concat (map #(connect_symbol_alphabet % alphabet) original_alphabet)))
;(combine_alphabets '(("a") ("b") ("c")) '(("a") ("b") ("c")))

(defn count_n [alphabet original_alphabet n]
  (if (> n 1)
    (count_n (combine_alphabets alphabet original_alphabet) original_alphabet (dec n))
    alphabet))
;(count_n '(("a") ("b") ("c")) '(("a") ("b") ("c")) 3)

(defn list_to_list [alphabet]
  (map #(cons % '()) alphabet))
;(list_to_list '("a" "b" "c"))

(defn task_1_4 [alphabet n]
  (count_n (list_to_list alphabet) (list_to_list alphabet) n))
;(task_1_4 '("a" 1 :b) 2)