(ns obsfucation.core
  (:require [clojure.string :as s]))

(s/replace "Hello world" #"\w" "!")

(s/replace "Hello world" #"\w" (fn [l] (println l) "!"))
(int \a)
(first (char-array "a"))
(Math/pow (int (first (char-array "a"))) 2)

(defn encode-letter [s]
  (let [code (Math/pow (int (first (char-array s))) 2)]
    (str (int code))))

(defn multi-encode-letter
  ([s]
   (let [code (Math/pow (int (first (char-array s))) 2)]
     (str (int code))))
  ([s n]
   (let [code (Math/pow (+ n (int (first (char-array s)))) 2)]
     (str "#" (int code)))))
(encode-letter "a")
(multi-encode-letter "a")
(multi-encode-letter "a" 3)

(defn encode [s]
  (s/replace s #"\w" encode-letter))

(defn multi-encode [s]
  (let [word-num (count (s/split s #" "))]
    (s/replace s #"\w" (fn [l] (multi-encode-letter l word-num)))))

(multi-encode "hello world")
(encode "Hello world!")

(defn decode-letter [x y]
  (let [num (Integer/parseInt (subs x 1))
        letter (char (- (Math/sqrt num) y))]
    (str letter)))

(defn decode [s]
  (let [word-num (count (s/split s #" "))]
    (s/replace s #"\#\d+" (fn [l] (decode-letter l word-num)))))

(decode (multi-encode "arthur"))
(multi-encode "arthur")
