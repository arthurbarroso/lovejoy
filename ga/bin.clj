(ns ga.bin)

(defn b [items r]
  (loop [l items
         a nil]
    (if a
      a
      (let [middle (/ (count l) 2)
            i (nth l middle)]
        (cond (= i r)
              (recur [] i)

              (> i r)
              (recur (take middle l) nil)

              (> r i)
              (recur (drop middle l) nil))))))

