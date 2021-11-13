(ns circuits
  "Utilities for dealing with logic gate circuits and such")

(defn opposite [n]
  (if (= 0 n) 1 0))

(defn exp-n [n x]
  (reduce * (repeat n x)))

(defn check-val-step
  "Decides whether the input's value should be it's
  current value or not based off of the input's step and current-count"
  [{:keys [step current-count value]}]
  (cond
    ;;resets count
    (= current-count step)
    {:value (opposite value)
     :current-count 1
     :step step}
    ;; while under the step
    (< current-count step)
    {:value value
     :current-count (inc current-count)
     :step step}))

(defn mount-input-column-values
  "Returns all values for an input based off of it's position on the table
   and the table's row number. Takes in an input, which is a map and the row
   number. The map *must* contain a property called step, which must be an Integer."
  [input row-number]
  (loop [parsed {:vals [] :count 0 :value 0}]
    (if (= row-number (count (:vals parsed)))
      (map :value (:vals parsed))
      (recur
       (let [result
             (check-val-step
              (merge
               input
               {:current-count
                (:count parsed)
                :value
                (:value parsed)}))]
         {:vals (conj (:vals parsed) result)
          :value (:value result)
          :count (:current-count result)})))))

(defn mount-item
  "Creates a map containing the input's 'name' and it's position
  on the table as a keyword and it's value as the value"
  [index value input-key]
  {(keyword (str input-key index)) value})

(defn mount-input-map
  "Creates a map for all the values an input may have"
  [input row-number]
  (let [input-key (:key input)]
    (->> row-number
         (mount-input-column-values input)
         (map-indexed #(mount-item %1 %2 input-key))
         (into {}))))

(defn mount-input-values
  "Runs mount-input-map for all of the inputs. Creates a single big map
  containing all the values"
  [inputs row-count]
  (->> inputs
       (map #(mount-input-map %1 row-count))
       (into {})))

(defn get-key [ik r vals]
  ((keyword (str ik r)) vals))

(defn run-results [r fnc table-values input-keys]
  {r
   (apply (:fnc fnc)
          (map
           (fn [ik]
             (get-key ik r table-values))
           input-keys))})

(defn get-results [table-values row-count inputs fnc]
  (let [input-keys (map :key (reverse inputs))]
    (->
     (->> row-count
          range
          (map
           (fn [r] (run-results r fnc table-values input-keys))))
     (into {}))))

(defn extract-result [results r]
  (str
   (nth (first (nth results r)) 1)))

(defn mount-table [values row-count inputs results fnc]
  (let [input-keys (map :key (reverse inputs))]
    [:table
     [:thead
      [:tr
       (for [ik input-keys]
         [:th ik])
       [:th
        (:tag fnc)]]]
     [:tbody
      (map
       (fn [r]
         [:tr
          (for [ik input-keys]
            [:td
             (get-key ik r values)])
          [:td
           (extract-result results r)]])
       (range row-count))]]))

(comment
  (let [inputs [{:step 1
                 :key "a"}
                {:step 2
                 :key "b"}]
        fun {:fnc bit-or
             :tag "or"}
        row-number (exp-n (count inputs) 2)
        table-values (mount-input-values inputs row-number)
        results (get-results table-values row-number inputs fun)]
    (mount-table table-values row-number inputs results fun)))
