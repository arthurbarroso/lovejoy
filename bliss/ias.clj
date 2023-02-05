(ns bliss.ias)

(defn prepare-pretty-print [signal-seq]
  (->> signal-seq
       (reduce (fn [acc item]
                 (assoc acc (:key item) {:symbols (:symbols item)}))
               {})))

(def symbol-conversion-map
  {:a "A"
   :b "B"
   :c "C"
   :not-a "!A"
   :not-b "!B"
   :not-c "!C"})

(defn symbol->str [symbols]
  (apply str (map #(get symbol-conversion-map %) symbols)))

(defn should-print-signal? [signal-key valid-signals]
  (if (get valid-signals signal-key)
    (str (symbol->str (-> valid-signals (get signal-key) :symbols)) " ")
    "000 "))

(defn pretty-printer [signal-map signals]
  (let [printer (map (fn [[signal-key _]]
                       (should-print-signal? signal-key
                                             (prepare-pretty-print
                                              signals)))
                     signal-map)
        lines (partition 4 printer)]
    (println (str
              "\n Lines: \n"
              (apply str (first lines)) "\n"
              (apply str (second lines))))
    signals))

(defn print-original-map-vals [original-map]
  (println "Starting map:")
  (doall (->> original-map
              (map second)
              (map :value)
              (map #(str "000" %))
              vec
              (partition 4)
              (map #(println %))))
  (println "\n"))

(defn print-original-map-symbols [original-map]
  (println "Starting map:")
  (doall (->> original-map
              (map second)
              (map :symbols)
              (map #(symbol->str %))
              vec
              (partition 4)
              (map #(println %))))
  (println "\n"))
