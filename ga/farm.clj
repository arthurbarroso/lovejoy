(ns ga.farm)

(defn find-constraint [width height]
  (cond (> width height)
        {:status :width-is-bigger
         :constraint height}

        (> height width)
        {:status :height-is-bigger
         :constraint width}

        (= height width)
        {:status :equal-sizes
         :constraint height}

        :else
        {:status :error}))

(defn divide-and-conquer-hehe-xd [farm-width farm-height]
  (loop [width farm-width
         height farm-height
         area (* farm-width farm-height)
         squares []]
    (if (= 0 area)
      {:width width :height height :squares squares}
      (let [{:keys [constraint status]} (find-constraint width height)]
        (cond (= :equal-sizes status)
              (recur (- width constraint)
                     (- height constraint)
                     (- area (* constraint constraint))
                     (conj squares [{:size constraint}]))

              (= :width-is-bigger status)
              (recur (- width constraint)
                     height
                     (- area (* constraint constraint))
                     (conj squares [{:size constraint}]))

              (= :height-is-bigger status)
              (recur constraint
                     (- height constraint)
                     (- area (* constraint constraint))
                     (conj squares [{:size constraint}])))))))

(comment

  (clojure.pprint/pprint
   (:squares (divide-and-conquer-hehe-xd 1680 640))))
