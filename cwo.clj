(ns cwo.core
  (:require [clojure.string :as s])
  (:use [clojure.repl]))

; 1.03 - The meditate function v2.0, page 33 - The clojure workshop
(defn meditate
  "Returns a transformed string `s` based on the `calmness-level`"
  [calmness-level s]
  (if (< calmness-level 4)
    (str (s/upper-case s) ", I TELL YA")
    (if (<= 4 calmness-level 9)
      (s/capitalize s)
      (if (= 10 calmness-level)
        (s/reverse s)
        nil))))

(defn meditate-cond
  "Returns a transformed string `s` based on the `calmness-level` using cond :)"
  [calmness-level s]
  (cond
    (< calmness-level 4) (str (s/upper-case s) ", I TELL YA!")
    (<= 4 calmness-level 9) (s/capitalize s)
    (= 10 calmness-level) (s/reverse s)
    :else nil))

(def test-string "what we do now echoes in eternity")
(meditate 1 test-string)
(meditate-cond 1 test-string)
(meditate 6 test-string)
(meditate-cond 6 test-string)
(meditate 10 test-string)
(meditate-cond 10 test-string)
