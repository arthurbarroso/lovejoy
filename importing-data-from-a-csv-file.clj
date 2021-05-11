(ns importing-data-from-a-csv-file.core)

(require '[clojure.data.csv :as csv])
(require '[clojure.java.io :as io])

(with-open [r (io/reader "match_scores_1991-2016_unindexed_csv.csv")]
  (->> (csv/read-csv r)
       (map #(nth % 7))
       (take 6)
       doall))
