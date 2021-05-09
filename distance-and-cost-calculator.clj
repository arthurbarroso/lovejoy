(ns distance-and-cost-calculator.core)

(def driving-speed 70)
(def walking-speed 5)

(def cars {:sporche {:per-km 0.12 :cost 1.5}
           :tayato {:per-km 0.07 :cost 1.5}
           :sleta {:per-km 0.2 :cost 0.1}})

(def paris {:lat 48.856483 :lon 2.352413})
(def bordeaux {:lat 44.834999 :lon -0.575490})

(defn distance [{:keys [lat lon]} {lat2 :lat lon2 :lon}]
  (let [m (- lat2 lat)
        n (* m m)
        o (Math/cos lat)
        p (- lon2 lon)
        q (* p p)]
    (* 110.25 (Math/sqrt (+ n (* o q))))))

(distance paris bordeaux)

(defmulti itinerary :transport)
(defmethod itinerary :walking
  [i]
  (let [from (:from i)
        to (:to i)
        dis (distance from to)]
    {:distance dis
     :duration (/ dis walking-speed)
     :cost 0}))

(itinerary {:transport :walking :from paris :to bordeaux})

(defmethod itinerary :driving
  [i]
  (let [from (:from i)
        to (:to i)
        dis (distance from to)
        car-data ((:vehicle i) cars)
        cost-per-km (* (:per-km car-data) (:cost car-data))]
    (println cost-per-km)
    {:distance dis
     :duration (/ dis driving-speed)
     :cost (* dis cost-per-km)}))

(itinerary {:transport :driving :from paris :to bordeaux :vehicle :tayato})
