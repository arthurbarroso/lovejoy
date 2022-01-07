(ns binary-search)

(defn search-for [item vector]
  (loop [left 0
         right (dec (count vector))]
    (if (<= left right)
      (let [mid (quot (+ left right) 2)
            cur ((vec vector) mid)]
        (cond
          (= cur item) mid
          (< cur item) (recur (inc mid) right)
          (> cur item) (recur left (dec mid))))
      (throw (Exception. "not found")))))
