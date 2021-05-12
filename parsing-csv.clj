(ns parsing-csv.core
  (:require
   [clojure.data.csv :as csv]
   [clojure.java.io :as io]
   [semantic-csv.core :as sc]))

(def filename "match_scores_1991-2016_unindexed_csv.csv")

(defn first-match [csv]
  (with-open [r (io/reader csv)]
    (->> (csv/read-csv r)
         sc/mappify
         first)))

(defn five-matches [csv]
  (with-open [r (io/reader csv)]
    (->> (csv/read-csv r)
         sc/mappify
         (map #(select-keys % [:tourney_year_id
                               :winner_name
                               :loser_name
                               :winner_sets_won
                               :loser_sets_won]))
         (take 5)
         doall)))

(comment
  (in-ns 'parsing-csv.core)
  (first-match "match_scores_1991-2016_unindexed_csv.csv")
  (five-matches "match_scores_1991-2016_unindexed_csv.csv"))

(defn five-matches-int-sets [csv]
  (with-open [r (io/reader csv)]
    (->> (csv/read-csv r)
         sc/mappify
         (map #(select-keys % [:tourney_year_id
                               :winner_name
                               :loser_name
                               :winner_sets_won
                               :loser_sets_won]))
         (sc/cast-with {:winner_sets_won sc/->int
                        :loser_sets_won sc/->int})
         (take 5)
         doall)))

(five-matches-int-sets filename)
