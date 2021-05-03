(ns nested-data-structures.core
  [:use clojure.repl clojure.pprint])

(def gemstone-db {:ruby {:name "Ruby"
                         :stock 450
                         :sales [1990 3644 6376 4918 7882 6747 7495 8573 5097 1712]
                         :properties {:dispersion 0.018
                                      :hardness 9.0
                                      :refractive-index [1.77 1.78]
                                      :color "Red"}}
                  :diamond {:name "Diamond"
                            :stock 10
                            :sales [8295 329 5960 6118 4189 3436 9833 8870 9700 7182 7061 1579]
                            :properties {:dispersion 0.044
                                         :hardness 10
                                         :refractive-index [2.417 2.419]
                                         :color "Typically yellow, brown or gray to colorless"}}
                  :moissanite {:name "moissanite"
                               :stock 45
                               :sales [7761 3220]
                               :properties {:dispersion 0.104
                                            :hardness 9.5
                                            :refractive-index [2.65 2.69]
                                            :color "Colorless, green, yellow"}}})

(get gemstone-db :diamond)
(get gemstone-db :ruby)
(get (get gemstone-db :ruby) :properties)
(:ruby gemstone-db)
(:properties (:ruby gemstone-db))
(:hardness (:properties (:ruby gemstone-db)))
(:properties (:diamond gemstone-db))
(get-in gemstone-db [:ruby :properties :hardness])

(defn durability [db gemstone]
  (get-in db [gemstone :properties :hardness]))
(durability gemstone-db :ruby)
(durability gemstone-db :moissanite)
(durability gemstone-db :diamond)

(assoc (:ruby gemstone-db) :properties
       {:color "Near colorless through pink through all shades of red to a deep crimson"})

(update (:ruby gemstone-db) :properties into
        {:color "Near colorless through pink through all shades of red to a deep crimson"})

(assoc-in gemstone-db [:ruby :properties :color] "Near colorless")

(pprint *1)
(defn change-color [db gemstone new-color]
  (assoc-in db [gemstone :properties :color] new-color))
(change-color gemstone-db :ruby "red-ish")

(set! *print-level* nil)
(update-in gemstone-db [:diamond :stock] dec)
(update-in gemstone-db [:diamond :sales] conj 999)

(defn sell [db gemstone client-id]
  (let [clients-updated-db (update-in db [gemstone :sales] conj client-id)]
    (update-in clients-updated-db [gemstone :stock] dec)))

(sell gemstone-db :moissanite 12)
