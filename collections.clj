(ns collections.core)
(def language {:name "Clojure" :creator "Rich Hickey"
               :platforms ["Java" "JavaScript" ".NET"]})

(count language)

(empty? language)

(empty? [])
(empty? '())
(seq language)
; maps are not sequential since there is no logical order between its elements
; it is possible to turn maps into sequenecs using `seq`
(first (seq language))

(rest (seq language))

(into [1 2 3 4] [5 6 7])
; into puts elements of a collection in another collections
(into [1 2 3] '(4 4 4))
(into [1] #{1 2 3})
; since hashsets are not ordered the operation wont insert the hash elements in
; the order these were defineed
(into #{} [1 2 2 2 3 3])
; using `into` on a set passing a vector would deduplicate the vector
; (id just run set tho)

; to put items into a map you'd need to pass a collection of tuples
; each tuple should represent key-value pairs
(into {} [[:a 1] [:b 2]])

; using `into` follows the convention of `conj`, so
(into '() [1 2 3 4])
; adds each element to the end of the list
(into [] [1 2 3 4])
; adds each element to the start of the vector

; concat also concatenate collections (and also follows conj's convention)
; concat returns a sequence no matter what the input type is
(concat '(1 2) '(3 4))
(concat [1 2] [3 4])
(concat #{1 2 3} #{1 2 3 4})

(def alphabet #{:a :b :c :d :e :f})
(sort alphabet)
(sort [3 7 5 9 12 3])

(into [] (sort [3 7 5 9 12 3]))

; conj also works on maps
(conj language [:created 2007])

; a vector is also an associative collection of key-value pairs where the key is the index
(assoc [:a :b :c :d] 2 :z)
