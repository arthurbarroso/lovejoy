(ns obsfucation.core
  (:require [clojure.string :as s])
  (:require [clojure.repl :refer [doc]]))

(s/replace "Hello world" #"\w" "!")

(s/replace "Hello world" #"\w" (fn [l] (println l) "!"))
(int \a)
(first (char-array "a"))
(Math/pow (int (first (char-array "a"))) 2)

(defn encode-letter [s]
  (let [code (Math/pow (int (first (char-array s))) 2)]
    (str (int code))))

(first "a")
(char-array "a")

(int (first "a"))
(first "a")
(int \a)

; (first `char`) returns \char
; (int \char) returns the corresponding integer for that chat.
; int is able to handle character literals, not strings,
; so we get the char literal using (first string)


(defn multi-encode-letter
  "Takes a character `s` and returns it's ASCII table number.
  Can also take a character `s` and an optional number `n` to
  return `s`'s ASCII table number + `n`"
  ([s]
   (let [code (Math/pow (int (first s)) 2)]
     (str (int code))))
  ([s n]
   (let [code (Math/pow (+ n (int (first s))) 2)]
     (str "#" (int code)))))

(doc multi-encode-letter)

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
