(ns extracting-data.core
  (:require [clojure.string :as s]))

(def game-users
  [{:id 9342
    :username "speedy"
    :current-points 45
    :remaining-lives 2
    :experience-level 5
    :status :active}
   {:id 9854
    :username "stealthy"
    :current-points 1201
    :remaining-lives 1
    :experience-level 8
    :status :speed-boost}
   {:id 3014
    :username "sneaky"
    :current-points 725
    :remaining-lives 7
    :experience-level 3
    :status :active}
   {:id 2051
    :username "forgetful"
    :current-points 89
    :remaining-lives 4
    :experience-level 5
    :status :imprisoned}
   {:id 1032
    :username "wandering"
    :current-points 2043
    :remaining-lives 12
    :experience-level 7
    :status :speed-boost}
   {:id 7213
    :username "slowish"
    :current-points 143
    :remaining-lives 0
    :experience-level 1
    :status :speed-boost}
   {:id 5633
    :username "smarter"
    :current-points 99
    :remaining-lives 4
    :experience-level 4
    :status :terminated}
   {:id 3954
    :username "crafty"
    :current-points 21
    :remaining-lives 2
    :experience-level 8
    :status :active}
   {:id 7213
    :username "smarty"
    :current-points 290
    :remaining-lives 5
    :experience-level 12
    :status :terminated}
   {:id 3002
    :username "clever"
    :current-points 681
    :remaining-lives 1
    :experience-level 8
    :status :active}])

(map #(:current-points %) game-users)
(map :current-points game-users)

(def alpha-set (set [:a :b :c]))
(alpha-set :z)
(alpha-set :a)
(:a alpha-set)

(def animal-names ["turtle" "horse" "cat" "frog" "hawk" "worm"])

(remove #(or (= % "horse") (= % "cat")) animal-names)
(remove #{"horse" "cat"} animal-names)

; filtering on a keyword with comp and a Set


(def normalizer (comp s/trim s/lower-case))
(normalizer " javascript sucks")
; comp calls functions from right to left lower-case gets called first in the above

(def remove-words #{"and" "an" "a" "the" "of" "is"})

(remove (comp remove-words s/lower-case s/trim) ["February" " THE " "4th"])

(map :current-score game-users)

(def keep-statuses #{:active :imprisoned :speed-bost})

(filter #(keep-statuses (:status %)) game-users)

(filter #(comp keep-statuses :status %) game-users)

(->> game-users
     (filter (comp keep-statuses :status))
     (map :current-points))

(def alpha-lc ["a" "b" "c" "d" "e" "f"])
(mapcat (fn [l] [l (s/upper-case l)]) alpha-lc)

(map #(+ %1 %2) [5 8 3 1 2] [5 2 7 9 8])

(defn m-zipmap [xs ys]
  (->> (map (fn [x y] [x y]) xs ys)
       (into {})))

(m-zipmap [5 8 3 1 2] [5 2 7 9 8])
(m-zipmap [:a :b :c] [1 2 3])

(def meals ["breakfast" "lunch" "dinner" "midnight snack"])

(map (fn [id meal] (str (inc id) " -> " meal)) (range) meals)

(map-indexed (fn [id meal] (str (inc id) " -> " meal)) meals)
