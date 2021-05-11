(ns filters.clj)

(odd? 5)

(filter odd? [1 2 3 4 5 6])

(remove odd? [1 2 3 4 5 6])

(filter (constantly true) [1 2 3 4 5])

(take 3 [1 2 3 4 5])
(drop 1 [1 2 3 4 5])
(take-while #(> 10 %) [2 9 4 12 3 99 1])
;starts at the beginning of a list and returns all the items along they satisfy the predicate
;stopts at the first item that doesnt match the predicate
(drop-while #(> 10 %) [2 9 4 12 3 99 1])
;starts at the beginning of a list and removes all items that satisfy the predicate


;4.03 - partitioning a sequence with take-while and drop-while

(def students [{:name "Eliza" :year 1994}
               {:name "Salma" :year 1995}
               {:name "Jodie" :year 1997}
               {:name "Kaitlyn" :year 2000}
               {:name "Alice" :year 2001}
               {:name "Pippa" :year 2002}
               {:name "Fleur" :year 2002}])

(defn before-2000 [y]
  (< (:year y) 2000))

(take-while before-2000 students)

(drop-while before-2000 students)

;;;
(map #(* 10 %)
     (filter odd? [1 2 3 4 5]))

(def filtered (filter odd? [1 2 3 4 5]))
(map #(* 10 %) filtered)
(->> [1 2 3 4 5]
     (filter odd?)
     (map #(* 10 %)))
; ->> macro (thread-last) makes it possible to preserve the logical order
; of execution without having to name return values. it "rewrites" the code so that
; the result of each operation is inseted as the last argument of the following
; operation
; e.g.:
(->> [1 2 3 4 5]
     (filter odd?))

(filter odd? [1 2 3 4 5])

(= (->> [1 2 3 4 5] (filter odd?)) (filter odd? [1 2 3 4 5]))

(def apart (partial * 10))

(apart 5)
(partial * 10) ;equals #(* 10 %)

((partial * 10) 5)
