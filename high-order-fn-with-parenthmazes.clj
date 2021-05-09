(ns high-order-fn-with-parenthmazes.core
  (:require [clojure.repl :as c]))

(def weapon-fn-map
  {:fists #(if (< % 100) (- % 10) %)
   :staff (partial + 35)
   :sword #(- % 100)
   :cast-iron-saucepan #(- % 100 (rand-int 50))
   :seet-potato identity})

((weapon-fn-map :fists) 150)
((weapon-fn-map :fists) 50)

((weapon-fn-map :staff) 150 30 25)

(c/doc partial)
(c/source partial)

(defn strike
  ([target] (strike target :fists))
  ([target weapon]
   (let [weapon-fn (weapon weapon-fn-map)]
     (update target :health weapon-fn))))

(def dummy {:name "Javascript" :health 250})

(strike dummy)
(strike dummy :sword)

(defn mighty-strike
  [target]
  (let [weapon-fn (apply comp (vals weapon-fn-map))]
    (update target :health weapon-fn)))

(mighty-strike dummy)
