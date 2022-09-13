(ns als
  (:require [clojure.zip :as zip]))

(def signal-maps-def-map2
  {"00" {:x 0 :y 0 :value 0}
   "10" {:x 1 :y 0 :value 0}
   "20" {:x 2 :y 0 :value 0}
   "30" {:x 3 :y 0 :value 1}
   "01" {:x 0 :y 1 :value 1}
   "11" {:x 1 :y 1 :value 1}
   "21" {:x 2 :y 1 :value 1}
   "31" {:x 3 :y 1 :value 1}})

(defn find-directions [signal-map]
  (let [previous-dir (:dir signal-map)
        can-go-up? {:dir :up :value (not (= (:y signal-map) 0))}
        can-go-down? {:dir :down :value (not (= (:y signal-map) 1))}
        can-go-left? {:dir :left :value (not (= (:x signal-map) 0))}
        can-go-right? {:dir :right :value (not (= (:x signal-map) 3))}]
    (if previous-dir
      [{:dir previous-dir}]
      (reduce
       (fn [acc possible-direction]
         (if (:value possible-direction)
           (conj acc possible-direction)
           acc))
       []
       [can-go-up? can-go-down?
        can-go-left? can-go-right?]))))

(defn is-out-of-bounds? [x y]
  (cond
    (> x 3) true
    (< x 0) true
    (> y 1) true
    (< y 0) true
    :else false))

(defn build-direction [direction x y]
  (let [dir (or (:dir direction) direction)]
    (condp = dir
      :up {:x x :y (dec y) :dir :up}
      :down {:x x :y (inc y) :dir :down}
      :left {:x (dec x) :y y :dir :left}
      :right {:x (inc x) :y y :dir :right}
      {:failure true})))

(defn draw-directions [signal-map possible-directions]
  (let [{:keys [x y]} signal-map]
    (reduce (fn [acc direction]
              (let [dir (build-direction (or (:dir signal-map) direction) x y)]
                (if (not (is-out-of-bounds? (:x dir) (:y dir)))
                  (conj acc dir)
                  acc)))
            []
            possible-directions)))

(defn is-in? [collection coordinates]
  (let [{:keys [x y]} coordinates]
    (some #(and (= x (:x %)) (= y (:y %))) collection)))

(defn find-in-direction [directions {:keys [x y]}]
  (filter
   (fn [direction]
     (and (= (:x direction) x)
          (= (:y direction) y)))
   directions))

(defn check-neighbors [signal-maps signal-map directions]
  (if (= (:value signal-map) 0)
    []
    (->> signal-maps
         (reduce (fn [acc [k v]]
                   (let [{:keys [x y]} v
                         found? (find-in-direction directions {:x x :y y})]
                     (if (seq found?)
                       (conj acc [(str x y)
                                  (assoc (first found?) :value (:value v))])
                       acc)))
                 [])
         (filter (fn [[k item]]
                   (when item) (= 1 (-> item :value)))))))

(defn build-neighbors [signal-maps signal-map neighbor-list]
  (assoc signal-map :neighbors (conj (map (fn [[_k signal]]
                                            {:signal signal
                                             :neighbors (->> signal
                                                             (find-directions (:dir signal)))})
                                          neighbor-list))))

(defn reversed-draw [direction mapft]
  (draw-directions mapft [{:dir direction}]))

(defn go-through-signal [signal signal-maps]
  (->> signal
       second
       find-directions
       (draw-directions (second signal))
       (check-neighbors signal-maps
                        (-> signal-maps vals reverse first))
       (assoc (second signal)
              :neighbors)))

(defn extract-signal-neighbors [signal]
  (->> signal
       :neighbors
       (map (fn [n] (first (vals n))))))

(defn scuffed-filter [thing]
  (not (nil? (first thing))))

(defn list->map [n]
  (let [filtered (filter scuffed-filter n)]
    (->> filtered
         (reduce
          (fn [acc [[k neighbor]]]
            (assoc acc k neighbor))
          {}))))

(defn keys-in
  [m]
  (letfn [(children [node]
            (let [v (get-in m node)]
              (if (map? v)
                (map (fn [x] (conj node x)) (keys v))
                [])))
          (branch? [node] (-> (children node) seq boolean))]
    (->> (keys m)
         (map vector)
         (mapcat #(tree-seq branch? children %)))))

(defn find-current-state [previous-state-key
                          raw-state]
  (->> raw-state
       keys-in
       (filter (fn [items]
                 (let [reversed-list (reverse items)]
                   (and (= previous-state-key (first reversed-list))
                        (= :neighbors (second reversed-list))))))))

(defn update-current-state [raw-state new-state
                            previous-state-key]
  (let [current-state-path (first (find-current-state previous-state-key raw-state))
        current-state (get-in raw-state [current-state-path])]
    (update-in raw-state
               (conj (first (find-current-state previous-state-key raw-state))
                     :neighbors)
               #(merge % new-state))))

(defn apply-visited [elements visited?]
  (map (fn [[k v]] [k (assoc v :visited visited?)])
       elements))

(defn extract-unvisited-neighbors [node]
  (->> node
       :neighbors
       (filter (fn [[k v]] (not (:visited v))))))

(defn loop-through-signals [signal signal-map]
  (loop [result {}
         start-signal signal
         prev-signal {}]
    (let [signal-neighbors (-> start-signal
                               (go-through-signal signal-map)
                               :neighbors)
          unvisited-neighbors (extract-unvisited-neighbors result)]
      (if (empty? signal-neighbors)
        (if (not (empty? unvisited-neighbors))
          (let [[first-unvisited-n & rest-unvisited-n] unvisited-neighbors
                updated-first-u-neighbor (apply-visited [first-unvisited-n]
                                                        true)]
            (recur (update result :neighbors #(merge %
                                                     (list->map
                                                      [updated-first-u-neighbor])))
                   first-unvisited-n first-unvisited-n))
          result)
        (let [[first-neighbor & rest-neighbors] signal-neighbors
              updated-first-neighbor (apply-visited [first-neighbor] true)
              updated-rest-neighbors (apply-visited rest-neighbors false)]
          (if (:neighbors result)
            (let [updated-state (update-current-state result
                                                      (list->map [updated-first-neighbor
                                                                  updated-rest-neighbors])
                                                      (first prev-signal))]
              (recur updated-state
                     first-neighbor first-neighbor))
            (recur (assoc (second start-signal) :neighbors
                          (list->map [updated-first-neighbor
                                      updated-rest-neighbors]))
                   first-neighbor first-neighbor)))))))

;; in a 3x2 grid there is a maximum of two unvisited neighbors/node
;; this happens because this will only check adjacent nodes that are in the same "direction"
;; as the direction the original node took to get to it's first neighbor
;; this means a node can't go left then down, for an example.
;; since we end up going through each ndoe individually, there is no need to think about the
;; unvisited nodes of a node's neighbors - these will be visited when the neighbor nodes are processed.
;; next steps: map through the `map` definition instead of going through a single key. then create a fn that checks for nodes that
;; are next to each other and can combine groups out of lines (so we can have 2x2, 3x2 stuff -- is that possible anyways? do not remember)
;; then figure out the best lines/groups, which should be somewhat simple

(comment
  (let [n (-> signal-maps-def-map2
              reverse
              first
              (loop-through-signals signal-maps-def-map2))]
    ;; (extract-unvisited-neighbors n)
    (clojure.pprint/pprint n)))
