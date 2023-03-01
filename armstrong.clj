(ns armstrong)

(defn- digit-values [digits]
  (->> digits
       (map-indexed (fn [index item]
                      (println {:index index
                                :item item})
                      (Math/pow item (index + 1))))))

(defn armstrong? [num]
  (let [digits (map #(Character/digit % 10) (str num))]
    (println digits)))

(comment (digit-values (armstrong? 30)))
