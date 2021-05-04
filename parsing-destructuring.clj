(ns parsing-destructuring.core)

(def booking
  [1425
   "Bob Smith"
   "allergic to javascript"
   [[48.9615 2.4372] [37.742 -25.6976]]
   [[37.742 -25.6976] [48.9165 2.4372]]])

(let [[id customer-name sensitive-data flight1 flight2 flight3] booking]
  (println id customer-name sensitive-data flight1 flight2 flight3))

(def mapjet-booking {:id 8773
                     :customer-name "Arthur"
                     :catering-notes "Hates javascript"
                     :flights [{:from {:lat 48.9615 :lon 2.4372 :name "Paris le Bourget airport"}
                                :to {:lat 37.742 :lon -25.6976 :name "Ponta Delgada airport"}}
                               {:from {:lat 37.742 :lon -25.6976 :name "Ponta Delgada airport"}
                                :to {:lat 48.9615 :lon 2.4372 :name "Paris le Bourget airport"}}]})

(let [{:keys [id customer-name]} mapjet-booking]
  (str id " - " customer-name))

(defn print-mapjet-flight [flight]
  (let [{:keys [from to]} flight
        {lat1 :lat lon1 :lon} from
        {lat2 :lat lon2 :lon} to]
    (str "Flying from: Lat " lat1 " Lon " lon1 " To: Lat " lat2 " Lon " lon2)))

(print-mapjet-flight (first (:flights mapjet-booking)))
