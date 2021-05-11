(ns consuming-extracted-data-with-apply.core)

(let [a 5
      b nil
      c 18]
  (+ a b c))

(let [a 5
      b nil
      c 18]
  (apply + (filter integer? [a b c])))

(apply min [])
(apply min 0 [])
