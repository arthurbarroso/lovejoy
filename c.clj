(ns c.core
  (:require [clojure.string :as s]))

(defn my-map [f items]
  (if (first items)
    (cons (f (first items))
          (my-map f (rest items)))
    (list)))

(my-map inc [1 2 3])

(defn expand [f x count]
  (when (pos? count)
    (cons x (expand f (f x) (dec count)))))

(expand inc 0 10)

(defn is-palindrome [word]
  (= (s/reverse word) word))

(is-palindrome "wow")

(defn c-compare [accumulator letter]
  (if (= (str letter) "c")
    (+ 1 accumulator)
    accumulator))

(defn count-letters [word letter]
  (reduce c-compare 0
     word))

(count-letters "chiclete" "c")



(defn my-filter [predicate items]
  (reduce
    (fn [acc item]
      (if (predicate item)
        (conj acc item)
        acc)) [] items))

(my-filter odd? '(1 2 3 4 5 6))

(reduce (fn [accumulator item]
          (+ accumulator 1)) 0 '(1 2 3 3 1))

#(reduce (fn [acc item]
            (cons item acc)) '() %)

(reduce (fn [acc item]
          (+ acc 1)) 0 '(1 2 3))
