(ns xeye
  (:require [clojure.core.async :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]]))

(def echo-chan (chan))
(comment
  (go (do
        (Thread/sleep 4000)
        (println (<! echo-chan))))
  (>!! echo-chan "something")
  (println "after"))
  ;; the above only prints after `(println "something")`

(comment
  (def hi-chan (chan))
  (doseq [n (range 1000)]
    (go (>! hi-chan (str "hi " n)))))

(defn machine []
  (let [in (chan)
        out (chan)]
    (go (<! in)
        (>! out "hot dog"))
    [in out]))

(comment
  (let [[in out] (machine)]
    (>!! in "pocket lint")
    (<!! out)))
