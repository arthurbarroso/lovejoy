(ns qcks)

(defn qcks [i]
  (if (<= (count i) 1)
    i
    (let [p (first i)
          o (rest i)]
      (concat
       (qcks
        (filter #(>= p %) o))
       [p]
       (qcks
        (filter #(< p %) o))))))
