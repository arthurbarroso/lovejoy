(ns ga.sel)

(defn find-smallest [items]
  (loop [l items
         idx 0
         sm (first items)
         sm-idx 0]
    (if (= (count l) 0)
      ;; when there are no elements left, leave
      sm-idx
      (if (< (first l) sm)
        ;; if current element is smaller then
        ;; the current smallest, increment index and
        ;; set new smallest
        (recur (rest l) (inc idx) (first l) idx)
        ;; if current element is not smaller then
        ;; the current smallest, increment index and
        ;; leave current smallest
        (recur (rest l) (inc idx) sm sm-idx)))))

(defn rm-at-pos [pos sq]
  (into (subvec sq 0 pos)
        (subvec sq (inc pos))))

(defn s [items]
  (loop [l items
         n []]
    (if (= (count l) 0)
      n
      (let [sm (find-smallest l)
            new (rm-at-pos sm l)
            sml (nth l sm)]
        (recur new
               (conj n sml))))))

(comment
  (find-smallest [3 3 4 2 1 4 9 0 8])
  (find-smallest [3 3 4 2 2 4 9 5 8 0 3])
  (s [1 2 3 4 5 0])
  (s [3 3 4 2 1 4 9 0 8]))
