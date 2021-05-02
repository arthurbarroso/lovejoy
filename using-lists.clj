(ns using-lists.core)

'(1 2 3)
(+ 1 2 3)

'(+ 1 2 3)
(list :a :b :c)

(first '(:a :b :c))
(= '(:a :b :c) [:a :b :c])
(rest '(:a :b :c))
(nth '(:a :b :c :d :e) 2)

(def my-todo '("Feed the cat :)" "Clean my room" "Learn clojure"))
(cons "Work using javascript :( *cries*" my-todo)
(conj my-todo "Work")

(conj my-todo "Work" "Work" "Hate javascript")
(first my-todo)
(rest my-todo)
(nth my-todo 2)
;Using nth on a nil index throws an error instead of returning nil
