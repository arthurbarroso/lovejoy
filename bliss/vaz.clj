(ns bliss.vaz
  (:require [clojure.zip :as zip]
            [bliss.ias :as ias]))

(def example-map
  {;;first-row
   "000" {:x 0 :y 0 :z 0 :value 0 :symbols [:not-a :not-b :not-c]}
   "100" {:x 1 :y 0 :z 0 :value 0 :symbols [:not-a :not-b :c]}
   "200" {:x 2 :y 0 :z 1 :value 0 :symbols [:not-a :b :c]}
   "300" {:x 3 :y 0 :z 0 :value 1 :symbols [:not-a :b :not-c]}
   ;; second-row
   "010" {:x 0 :y 1 :z 0 :value 0 :symbols [:a :not-bc]}
   "110" {:x 1 :y 1 :z 0 :value 1 :symbols [:a :not-b :c]}
   "210" {:x 2 :y 1 :z 0 :value 1 :symbols [:a :b :c]}
   "310" {:x 3 :y 1 :z 0 :value 1 :symbols [:a :b :not-c]}})

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

(defn build-direction [direction x y z]
  (let [dir (or (:dir direction) direction)]
    (condp = dir
      :up {:x x :y (dec y) :z z :dir :up}
      :down {:x x :y (inc y) :z z :dir :down}
      :left {:x (dec x) :y y :z z :dir :left}
      :right {:x (inc x) :y y :z z :dir :right}
      {:failure true})))

(defn draw-directions [signal-map possible-directions]
  (let [{:keys [x y z]} signal-map]
    (reduce (fn [acc direction]
              (let [dir (build-direction (or (:dir signal-map) direction) x y z)]
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
                   (let [{:keys [x y z]} v
                         found? (find-in-direction directions {:x x :y y})]
                     (if (seq found?)
                       (conj acc [(str x y z)
                                  (assoc (first found?) :value (:value v))])
                       acc)))
                 [])
         (filter (fn [[k item]]
                   (when item) (= 1 (-> item :value)))))))

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

(defn separate-node
  "Separates keys and values from both amps and lists"
  [node]
  (if (map? node)
    [(first (keys node)) (first (vals node))]
    [(first node) (second node)]))

(defn loop-neighbor
  "Traverses a signal's neighbors transforming nested maps into lists"
  [neighbor]
  (loop [result []
         new-node neighbor]
    (let [[node-key node-map] (separate-node new-node)
          node-neighbor (:neighbors node-map)]
      (if (not node-neighbor)
        (conj result (assoc (dissoc node-map :neighbors) :key node-key))
        (recur (conj result (assoc (dissoc node-map :neighbors) :key node-key))
               node-neighbor)))))

(defn split-lines
  "Takes in a parsed signal, traverses it's neighbors and group them into lists"
  [signal-and-neighbors]
  (->> signal-and-neighbors
       :neighbors
       (map loop-neighbor)
        ;; the only case possible for a signal map to have 3-length lines is
        ;; when all of it's spaces are value:1, which can be treated beforehand.
       ;; (filter #(not (odd? (count %))))
       (filter #(not (= 1 (count %))))
       first))

(defn reattach-symbols
  "Attaches the :symbol key back to a processed neighbor in a line"
  [signal-maps line-seq]
  (map (fn [line] (assoc line :symbols (get-in signal-maps [(:key line) :symbols])))
       line-seq))

(comment
  (->> example-map
       (map #(-> %
                 (loop-through-signals example-map)
                 split-lines))
       (filter #(not (nil? %)))
       (map #(reattach-symbols example-map %))
       (map #(ias/pretty-printer example-map %))))
       ;; clojure.pprint/pprint))
;; in a 3x2 grid there is a maximum of two unvisited neighbors/node
;; this happens because this will only check adjacent nodes that are in the same "direction"
;; as the direction the original node took to get to it's first neighbor
;; this means a node can't go left then down, for an example.
;; since we end up going through each ndoe individually, there is no need to think about the
;; unvisited nodes of a node's neighbors - these will be visited when the neighbor nodes are processed.
;;
;; next steps:
;; - [] map through the `map` definition instead of going through a single key.
;; - [x] create a function that generates lines from valid nodes
;; - [] create a fn that checks for nodes that are next to each other and can combine groups out of lines
;;      (so we can have 2x2, 3x2 stuff -- is that possible anyways? do not remember)
;; then figure out the best lines/groups, which should be somewhat simple
