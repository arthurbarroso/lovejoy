(ns match
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]))

(defn remove-whitespaces [input]
  (let [split-string (str/split input #"\s")]
    (->> split-string
         (filter #(not (str/blank? %)))
         (str/join ""))))

(def opening-parenthesis {:regex #"\("
                          :type :opening})
(def closing-parenthesis {:regex #"\)"
                          :type :closing})

(defn match-parenthesis [match-map paren-regex]
  (let [parens (->> match-map
                    :characters
                    (map #(re-matches (:regex paren-regex) %))
                    (filter identity)
                    count)]
    (assoc match-map
           (keyword (str (name (:type paren-regex)) "-count"))
           parens)))

(defn build-starting-map [character-list]
  {:characters character-list
   :opening-count 0
   :closing-count 0
   :valid? true})

(defn validator [input]
  (-> input
      remove-whitespaces
      (str/split #"")
      build-starting-map
      (match-parenthesis opening-parenthesis)
      (match-parenthesis closing-parenthesis)
      ((fn [match-map]
         (assoc match-map :valid? (= (:opening-count match-map)
                                     (:closing-count match-map)))))))

(def operations
  {"*" {:precendence 3
        :associativity :left
        :op *}
   "/" {:precendence 3
        :associativity :left
        :op /}
   "+" {:precendence 2
        :associativity :left
        :op +}
   "-" {:precendence 2
        :associativity :left
        :op -}})

(defn tokenizer [{:keys [characters]}]
  (reduce
   (fn [accumulator character]
     (cond (re-matches #"\d+" character)
           (conj accumulator {:type :number
                              :value (Integer/parseInt character)})

           (re-matches (:regex opening-parenthesis) character)
           (conj accumulator {:type :opening-parenthesis
                              :value character})

           (re-matches (:regex closing-parenthesis) character)
           (conj accumulator {:type :closing-parenthesis
                              :value character})

           (re-matches #"\+|\*|\/|\-" character)
           (conj accumulator {:type :operation
                              :value character})

           :else
           accumulator))
   [] characters))

(defn compare-operations [current-op last-op]
  (let [current (->> current-op :value (get operations))
        current-precendence (:precendence current)
        last (->> last-op :value (get operations))
        last-precendence (:precendence last)]
    (or (> current-precendence last-precendence)

        (and (= current-precendence last-precendence)
             (= :left (:associativity current))))))

(defn first-pass [token-list]
  (loop [tokens token-list
         output [] ;; queue, fifo
         operator-stack []] ;; stack, filo
    (if (= 0 (count tokens))
      {:output output
       :tokens tokens
       :operator-stack operator-stack}
      (let [current-token (first tokens)
            current-type (:type current-token)]
        (cond (= :number current-type)
              (recur (rest tokens) (conj output current-token) operator-stack)

              (= :operation current-type)
              (let [last-operation (last operator-stack)]
                (if (and last-operation (not (= :opening-parenthesis (:type last-operation))))
                  (if (compare-operations current-token last-operation)
                    (recur (rest tokens) (conj output last-operation)
                           (conj (pop operator-stack) current-token))
                    (recur (rest tokens) output (conj operator-stack current-token)))
                  (recur (rest tokens) output (conj operator-stack current-token))))

              (= :opening-parenthesis current-type)
              (recur (rest tokens) output (conj operator-stack current-token))

              (= :closing-parenthesis current-type)
              (let [last-operation (last operator-stack)]
                (if (not (= :opening-parenthesis (:type last-operation)))
                  (recur (rest tokens) (conj output last-operation) (pop operator-stack))
                  (recur (rest tokens) output (pop operator-stack))))

              :else (recur (rest tokens) output operator-stack))))))

(defn second-pass [{:keys [operator-stack output]}]
  (loop [-operator-stack operator-stack
         -output output]
    (if (= 0 (count -operator-stack))
      {:output -output
       :operator-stack -operator-stack})
    (let [last-op (last -operator-stack)]
      (if last-op
        (if (not (= :opening-parenthesis (:type last-op)))
          (recur (pop -operator-stack) (conj -output last-op))
          (recur (pop -operator-stack) -output))
        {:output -output :operator-stack -operator-stack}))))

(defn execute [{:keys [output]}]
  (loop [tokens output
         elements []]
    (if (= 0 (count tokens))
      {:result elements}
      (let [current-token (first tokens)]
        (cond (= :number (:type current-token))
              (recur (rest tokens) (conj elements (:value current-token)))

              (= :operation (:type current-token))
              (let [op (->> current-token :value (get operations) :op)
                    new-val (apply op elements)]
                (recur (rest tokens) [new-val])))))))

(defn basic-math [user-input]
  (let [sanitized-input (validator user-input)]
    (if (not (:valid? sanitized-input))
      (pprint/pprint
       {:error "Error running the program" :reason "Looks like your input contains unbalanced parentheses"})
      (->> sanitized-input
           tokenizer
           first-pass
           second-pass
           execute
           pprint/pprint))))

;; refer to the shanting yard algorithm
;; this intentionally does not support single arg functions such as sine/cosine and so on
;; unbalanced parentheses get handled at the start to avoid dealing with it further down the line
;; this was done to play around with stuff. id never write this thing manually if i really needed it -
;; instaparse's way better tool for parsing

(def user-input "((1 + 2 * 3) - 4 + 5) / 6")

(comment
  (basic-math user-input))
