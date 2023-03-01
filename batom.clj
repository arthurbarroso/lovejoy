(ns batom
  (:require [clojure.core.async :refer [chan >!!
                                        <!! thread
                                        take! put! go >! <!]]))

(comment
  (let [c (chan)]
    (future (doseq [x (range 1 5)]
              (>!! c x)))
    (future (doseq [x (range 1 5)]
              (println "from channel: " (<!! c))))))
;; Puts and takes values from channels in a blocking way. Runs in another thread because
;; of the promise.

(comment
  (let [c (chan)]
    (thread (doseq [x (range 1 5)]
              (>!! c x)))
    (thread (doseq [x (range 1 5)]
              (println "from channel: " (<!! c))))))
;; Puts and takes values from channels in a blocking way. Runs in another thread because
;; of `thread`

(comment
  (let [c (chan)]
    (thread (doseq [x (range 1 5)]
              (>!! c x)
              (println "putting value " x "  on channel")))))
;; Nothing is being put into the channel, since it isn't a buffered channel
;; and there isn't anything taking from it.

(comment
  (let [c (chan 5)]
    (thread (doseq [x (range 1 5)]
              (>!! c x)
              (println "putting value " x "  on channel")))))
