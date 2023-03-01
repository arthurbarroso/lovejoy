(ns vermelho)

(def error-map
  {:errors {:resource-type ["missing required key"
                            "invalid thing"]
            :object-id ["invalid type"
                        "expected string"]}})

(defn extract-error-keys [[key value]]
  {:type "validation"
   :field key :reasons value})

(->> error-map
     :errors
     (map extract-error-keys)
     clojure.pprint/pprint)
