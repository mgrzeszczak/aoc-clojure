(ns aoc-clojure.lib.lib)
(require '[clojure.data.priority-map :refer [priority-map]])

(defn abs [n] (max n (-' n)))

(defn print-points [points]
  (let [max-x (apply max (map first points))
        max-y (apply max (map second points))]
    (dotimes [y (inc max-y)]
      (dotimes [x (inc max-x)]
        (print (if (points [x y]) "#" " ")))
      (println))))

(defn grid-size [grid]
  (let [xs (map first (keys grid))
        ys (map second (keys grid))
        x-min (reduce min xs)
        x-max (reduce max xs)
        y-min (reduce min ys)
        y-max (reduce max ys)]
    [x-min x-max y-min y-max]))

(defn print-grid[grid]
  (let [[x-min x-max y-min y-max] (grid-size grid)]
    (dotimes [dy (inc (- y-max y-min))]
      (dotimes [dx (inc (- x-max x-min))]
        (print (format "%3s" (or (grid [(+ x-min dx) (+ y-min dy)]) "")))
        )
      (println)
      )
    )
  )


(defn parse-grid
  ([lines]
   (into {} (reduce concat (map-indexed (fn [row line] (map-indexed #(vector [row %1] (str %2)) line)) lines))))
  ([lines value-fn]
   (into {} (reduce concat (map-indexed (fn [row line] (map-indexed #(vector [row %1] (value-fn (str %2))) line)) lines)))))

(defn parse-grid-2
  ([lines]
   (parse-grid-2 lines identity))
  ([lines value-fn]
   (into {} (reduce concat (map-indexed (fn [row line] (map-indexed #(vector [%1 row] (value-fn (str %2))) line)) lines)))))

(defn grid-neighbors-4 [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (and (< (+ (abs dx) (abs dy)) 2) (not (and (= dx 0) (= dy 0))))]
    [(+ x dx) (+ y dy)]))

(defn grid-neighbors-8 [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not (and (= dx 0) (= dy 0)))]
    [(+ x dx) (+ y dy)]))

(defn shortest-path ([from to grid heuristic-fn]
                     (loop [q (priority-map from (heuristic-fn from to))
                            g {from 0}]
                       (if (empty? q)
                         nil
                         (let [[current v] (peek q)]
                           (if (= current to)
                             v
                             (let [ns (filter grid (grid-neighbors-4 current))
                                   to-update (filter (fn [[p score]] (< score (or (g p) ##Inf))) (map (fn [p] [p (+ (g current) (grid p))]) ns))
                                   new-g (into g to-update)
                                   new-q (into (pop q) (map (fn [[p score]] [p (+ score (heuristic-fn p to))]) to-update))]
                               (recur new-q new-g))))))))

(defn dijkstra
  [start neighbor-fn]
  (loop [queue (priority-map start 0)
         cost-map {start 0}]
    (if (empty? queue)
      cost-map
      (let [[current cost] (peek queue)]
        (let [ns (neighbor-fn current)
              to-update (filter (fn [[n d]] (< d (or (cost-map n) ##Inf))) (map (fn [[n d]] [n (+ d cost)]) ns))
              updated-cost-map (into cost-map to-update)
              new-queue (into (pop queue) to-update)]
          (recur new-queue updated-cost-map))))))