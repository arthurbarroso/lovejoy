(ns querying-data-from-csv.core
  (:require
   [clojure.data.csv :as csv]
   [clojure.java.io :as io]
   [semantic-csv.core :as sc]))

(def filename "match_scores_1991-2016_unindexed_csv.csv")

(defn federer-wins [csv]
  (with-open [r (io/reader csv)]
    (->> (csv/read-csv r)
         sc/mappify
         (filter #(= "Roger Federer" (:winner_name %)))
         (map #(select-keys % [:winner_name
                               :loser_name
                               :winner_sets_won
                               :loser_sets_won
                               :winner_games_won
                               :loser_games_won
                               :tourney_year_id
                               :tourney_slug]))
         doall)))

(federer-wins filename)

(defn match-query [csv pred]
  (with-open [r (io/reader csv)]
    (->> (csv/read-csv r)
         sc/mappify
         (sc/cast-with {:winner_sets_won sc/->int
                        :loser_sets_won sc/->int
                        :winner_games_won sc/->int
                        :loser_games_won sc/->int})
         (filter pred)
         (map #(select-keys % [:winner_name
                               :loser_name
                               :winner_sets_won
                               :loser_sets_won
                               :winner_games_won
                               :loser_games_won
                               :tourney_year_id
                               :tourney_slug]))
         doall)))

(defn federer-check [i]
  (or (= "Roger Federer" (:winner_name i))
      (= "Roger Federer" (:loser_name i))))

(count (match-query filename federer-check))

(defn close-matches [i]
  (and (= (hash-set (:winner_name i) (:loser_name i))
          #{"Roger Federer" "Rafael Nadal"})
       (= 1 (- (:winner_sets_won i) (:loser_sets_won i)))))

(take 3 (match-query filename close-matches))
