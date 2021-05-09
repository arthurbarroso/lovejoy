(ns using-multimethods.core
  (:require [clojure.repl :as c]))

(def player {:name "Laura"
             :health 200 :position {:x 10 :y 10 :facing :north}})
(defn sample [coll] (first (shuffle coll)))
(sample [1 2 3])

(def sample-v2 (comp first shuffle))
((comp first shuffle) [1 2 3])

((comp :facing :position) player)
(defmulti move (comp :facing :position))
(defmethod move :north
  [entity]
  (update-in entity [:position :y] inc))

(move player)

(defmethod move :south
  [entity]
  (update-in entity [:position :y] dec))

(defmethod move :west
  [entity]
  (update-in entity [:position :x] inc))

(defmethod move :east
  [entity]
  (update-in entity [:position :x] dec))

(move {:position {:x 10 :y 10 :facing :west}})
(move {:position {:x 10 :y 10 :facing :east}})
(move {:position {:x 10 :y 10 :facing :south}})

(defmethod move :default [entity] entity)

(move {:position {:x 10 :y 10 :facing :wall}})
