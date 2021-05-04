(ns destructuring.core)

(let [[a b c] '(1 2 3)]
  (println a b c))

(defn print-coords [airport]
  (let [lat (:lat airport)
        lon (:lon airport)
        name (:name airport)]
    (println (str name "is located at lat: " lat " - lon: " lon))))

(defn print-airport-coords [airport]
  (let [{lat :lat lon :lon airport-name :name} airport]
    (println (str airport-name "is located at lat: " lat " - lon: " lon))))

;; when all keys and symbols have the same name it is even easier
(defn print-airport-destructure-coords [airport]
  (let [{:keys [lat lon name]} airport]
    (println (str name "is located at lat: " lat " - lon: " lon))))
