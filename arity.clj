(ns arity.clj
  (:require [clojure.string :as s]))
; variadic
(defn greet [p]
  (str "Welcome " p "! "))

(defn welcome [p & friends]
  (if (seq friends)
    (str (greet p) "Sending " (count friends)
         " friend requests to the following users: " (s/join ", " friends))
    (greet p)))

(welcome "arthur")
(welcome "arthur" "laura")
