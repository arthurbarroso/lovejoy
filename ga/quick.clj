(ns ga.quick)

(defn qsrt [items]
  (if (> 2 (count items))
    (flatten items)
    (let [pivot (first items)
          rst (rest items)
          less (filter #(>= pivot %) rst)
          greater (filter #(<= pivot %) rst)]
      (flatten (conj (qsrt greater) pivot (qsrt less))))))

(comment
  (clojure.pprint/pprint (qsrt [10 5 2 3]))
  (clojure.pprint/pprint (qsrt [1 2 3 4 5 6]))
  (clojure.pprint/pprint (qsrt [6 5 4 3 2 1]))
  (clojure.pprint/pprint (qsrt (distinct [10 10 10 3 3]))))
