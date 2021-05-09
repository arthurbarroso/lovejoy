(ns multi-arity-destructuring.core)

(def weapon-damage
  {:fists 10.0 :staff 35.0 :sword 100.0 :cast-iron-saucepan 150.0})

(defn strike
  ([target weapon]
   (let [points (weapon weapon-damage)]
     (if (= :gnomes (:camp target))
       (update target :health + points)
       (update target :health - points)))))

(def enemy {:name "Javascript" :health 250 :camp :trolls})
(strike enemy :sword)

(def ally {:name "Clojure" :health 80 :camp :gnomes})
(strike ally :staff)
