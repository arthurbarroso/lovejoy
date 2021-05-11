(ns finding-average-weather-temperature.core)

(def temperature-by-day
  [18 23 24 23 27 24 22 21 21 20 32 33 30 29 35 28 25 24 28 29 30])

(let [t (apply + temperature-by-day)
      c (count temperature-by-day)]
  (/ t c))
