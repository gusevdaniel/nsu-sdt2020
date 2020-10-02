(ns my-stuff.core
  (:gen-class))

(defn connect_for_n_more_two [symbol alphabet]
  (if (> (count alphabet) 0)
    (if (or (= symbol (str (last (last (list (first alphabet)))))) (= symbol (last (last (list (first alphabet))))))
      (connect_for_n_more_two symbol (rest alphabet))
      (cons (concat (first alphabet) (list symbol)) (connect_for_n_more_two symbol (rest alphabet))))
    ()))
;(connect_for_n_more_two "f" '(("a" "d") ("b" "d") ("c" "d")))
;(connect_for_n_more_two 1 '((1 "a") (:b "a") ("a" 1) (:b 1) ("a" :b) (1 :b)))

(defn connect_symbol_alphabet [symbol alphabet first_run]
  (if (= first_run true)
    (if (> (count alphabet) 0)
      (if (or (= symbol (str (last (list (first alphabet))))) (= symbol (last (list (first alphabet)))))
        (connect_symbol_alphabet symbol (rest alphabet) first_run)
        (cons (concat (list (first alphabet)) (list symbol)) (connect_symbol_alphabet symbol (rest alphabet) first_run)))
      ())
    (connect_for_n_more_two symbol alphabet)))
;(connect_symbol_alphabet "d" '("a" "b" "c") true)
;(connect_symbol_alphabet :b '("a" 1) true)

(defn combine_alphabets [alphabet original_alphabet first_run]
  (if (> (count original_alphabet) 0)
    (concat (connect_symbol_alphabet (first original_alphabet) alphabet first_run) (combine_alphabets alphabet (rest original_alphabet) first_run))
    ()))
;(combine_alphabets '("a" "b" "c") '("a" "b" "c") true)

(defn count_n [alphabet n original_alphabet first_run]
  (if (= first_run true)
    (count_n (combine_alphabets alphabet original_alphabet first_run) (dec n) original_alphabet false)
    (if (> n 1)
      (count_n (combine_alphabets alphabet original_alphabet first_run) (dec n) original_alphabet false)
      alphabet)))
;(count_n '("a" "b" "c") 3 '("a" "b" "c") true)

(defn task_1_1 [alphabet n]
  (count_n alphabet n alphabet true))
;(task_1_1 '("a" "b" "c") 2)
;(task_1_1 '("a" 1 :b) 2)