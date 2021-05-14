(ns reducing.core)

(reduce #(+ %1 %2) [8 4000 10 300])
(reduce + [8 4000 10 300])

(def weather-days
  [{:max 31
    :min 27
    :description :sunny
    :date "2019-09-24"}
   {:max 28
    :min 25
    :description :cloudy
    :date "2019-09-25"}
   {:max 22
    :min 18
    :description :rainy
    :date "2019-09-26"}
   {:max 23
    :min 16
    :description :stormy
    :date "2019-09-27"}
   {:max 35
    :min 19
    :description :sunny
    :date "2019-09-28"}])

(apply max (map :max weather-days))
(reduce (fn [min-max-day-so-far this-day]
          (if (< (:max this-day) (:max min-max-day-so-far))
            this-day
            min-max-day-so-far))
        weather-days)

(reduce (fn [{:keys [min max]} new-n]
          {:min (if (and min (> new-n min))
                  min
                  new-n)
           :max (if (and max (< new-n max))
                  max
                  new-n)}) {} [5 23 5004 845 22])
