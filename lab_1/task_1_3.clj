(ns task-1-3.core
  (:gen-class))

(defn my-map [f coll]
  (reduce (fn [new_coll value] (concat new_coll [(f value)])) [] coll))

(defn my-filter [f coll]
  (reduce (fn [new_coll value] (if (f value) (concat new_coll [value]) new_coll)) [] coll))