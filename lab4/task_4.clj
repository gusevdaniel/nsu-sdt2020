(ns task-4.core
  (:gen-class))

(defn constant [value] 
  (list ::const (if value true false)))

(defn constant? [expr] 
  (= (first expr) ::const))

(defn constant-value [expr] 
  (second expr))

(defn variable [name]
  {:pre [(keyword? name)]}
  (list ::var name))

(defn variable? [expr] 
  (= (first expr) ::var))

(defn variable-name [var] 
  (second var))

(defn same-variables? [v1 v2]
  (and
   (variable? v1)
   (variable? v2)
   (= (variable-name v1)
      (variable-name v2))))

(defn conjunction [expr & rest]
  (if (empty? rest)
    expr
    (cons ::and (cons expr rest))))

(defn conjunction? [expr] 
  (= (first expr) ::and))

(defn disjunction [expr & rest]
  (if (empty? rest)
    expr
    (cons ::or (cons expr rest))))

(defn disjunction? [expr] 
  (= (first expr) ::or))

(defn invert [expr] 
  (list ::not expr))

(defn invert? [expr] 
  (= (first expr) ::not))

(defn implication [expr-x expr-y] 
  (list ::impl expr-x expr-y))

(defn implication? [expr] 
  (= (first expr) ::impl))

(defn implication-first [expr]
  {:pre (implication? expr)}
  (first (rest expr)))

(defn implication-second [expr]
  {:pre (implication? expr)}
  (second (rest expr)))

(defn args [expr] 
  (rest expr))

(defn update-args [expr new-args]
  (if (> (count new-args) 1)
    (cons (first expr) new-args)
    (list (first expr) (first new-args))))

(defn apply-recur-downstream [f expr]
  (let [new-expr (f expr)]
    (cond
      (nil? new-expr) nil
      (or (variable? new-expr) (constant? new-expr)) new-expr
      :else (update-args new-expr (map #(apply-recur-downstream f %) (rest new-expr))))))

(defn collect-args [expr]
  (if (or (variable? expr) (constant? expr))
    (list expr)
    (->> (args expr)
         (map (fn [arg] (if (= (first expr) (first arg))
                          (collect-args arg)
                          (list arg))))
         (apply concat))))

(defn rm-brackets [expr]
  (if (and (not (or (variable? expr) (constant? expr)))
           (or
            (disjunction? expr)
            (conjunction? expr)))
    (update-args expr (map #(rm-brackets %) (collect-args expr)))
    expr))

(defn apply-recur-upstream [f expr]
  (if (or (variable? expr) (constant? expr))
    expr
    (rm-brackets (f (update-args expr (map #(apply-recur-upstream f %) (args expr)))))))

(defn remove-impl [expr]
  (if (implication? expr)
    (disjunction (invert (implication-first expr)) (implication-second expr))
    expr))

(defn open-not [expr]
  (if (invert? expr)
    (let [arg (first (args expr))]
      (cond
        (invert? arg) (first (args arg))
        (constant? arg) (if (= arg true) false true)
        (conjunction? arg) (apply disjunction (map (fn [x] (invert x)) (args arg)))
        (disjunction? arg) (apply conjunction (map (fn [x] (invert x)) (args arg)))
        :else expr))
    expr))

(defn print-dnf [expr]
  (cond
    (variable? expr) (name (first (args expr)))
    (constant? expr) (str (first (args expr)))
    (disjunction? expr) (str "(" (reduce #(str %1 " + " (print-dnf %2)) (print-dnf (first (args expr))) (rest (args expr))) ")")
    (conjunction? expr) (str "(" (reduce #(str %1 " * " (print-dnf %2)) (print-dnf (first (args expr))) (rest (args expr))) ")")
    (implication? expr) (str "(" (print-dnf (first (args expr))) " -> " (print-dnf (second (args expr))) ")")
    (invert? expr) (str "!" (print-dnf (second expr)))
    :else (throw "TODO")))

(defn cart
  ([colls]
   (if (empty? colls)
     '(())
     (for [more (cart (rest colls))
           x (first colls)]
       (if (empty? more)
         x
         (conjunction x more)))))
  ([a b] (cart (list (rest a) (rest b)))))

(defn distrib-multi [expr]
  (if (and (conjunction? expr) (every? #(or (disjunction? %) (variable? %) (constant? %) (invert? %)) (args expr)))
    (apply disjunction (cart (apply list (map #(if (or (constant? %) (variable? %) (invert? %)) (list %) (rest %)) (rest expr)))))
    expr))

(defn simplify-with-const [expr]
  (let [expr (rm-brackets expr)
        const-true-args (filter #(and (constant? %) (= (second %) true)) (args expr))
        const-false-args (filter #(and (constant? %) (= (second %) false)) (args expr))]
    (if (or (variable? expr) (constant? expr))
      expr
      (cond
        (and (conjunction? expr) (not (empty? const-false-args)))
        (constant false)
        (and (conjunction? expr) (not (empty? const-true-args)))
        (let [all-but-trues (filter #(not (= % (constant true))) (args expr))]
          (if (empty? all-but-trues)
            (constant true)
            (apply conjunction all-but-trues)))
        (and (disjunction? expr) (not (empty? const-true-args)))
        (constant true)
        (and (disjunction? expr) (not (empty? const-false-args)))
        (let [all-but-falses (filter #(not (= % (constant false))) (args expr))]
          (if (empty? all-but-falses)
            (constant false)
            (apply disjunction all-but-falses)))
        :else expr))))

(defn update-constants [expr]
  (if (and (invert? expr) (constant? (second expr)))
    (if (= (second (second expr)) true)
      (constant false)
      (constant true))
    expr))

(defn simplify-identity [expr]
  (if (or (conjunction? expr) (disjunction? expr))
    (update-args expr (distinct (args expr)))
    expr))

(defn simplify-singles [expr]
  (if (and (or (conjunction? expr) (disjunction? expr)) (= (count (args expr)) 1))
    (first (args expr))
    expr))

(defn to-dnf [expr]
  (->> expr
       (apply-recur-downstream remove-impl)
       (apply-recur-downstream open-not)
       (apply-recur-upstream distrib-multi)
       (distrib-multi)
       (apply-recur-upstream update-constants)
       (apply-recur-upstream simplify-identity)
       (apply-recur-upstream simplify-with-const)))

(defn substitute [subst expr]
  (if (or (not (variable? expr)) (empty? subst))
    expr
    (if (= (first (first subst)) (variable-name expr))
      (constant (second (first subst)))
      (substitute (rest subst) expr))))

(defn substitute-func [subst]
  (fn [expr] (substitute subst expr)))

(defn dnf-evaluate [subst expr]
  (->> expr
       (to-dnf)
       (apply-recur-downstream (substitute-func subst))
       (apply-recur-upstream update-constants)
       (apply-recur-upstream simplify-identity)
       (apply-recur-upstream simplify-singles)
       (apply-recur-upstream simplify-with-const)))