(ns using-sets.core)

(def supported-currencies #{"Dollar" "Japanese yen" "Euro" "Indian rupee" "British pound"})
(get supported-currencies "Dollar")
(get supported-currencies "Real")
(get supported-currencies "Real" "BRL")
(contains? supported-currencies "Real")
(supported-currencies "Dollar")
;("Dollar" suppoorted-currencies) throws an error

(conj supported-currencies "Real")
(disj supported-currencies "Dollar")
