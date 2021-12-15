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

(defn print-grid [grid]
  (let [max-x (apply max (map ffirst grid))
        max-y (apply max (map #(second (first %)) grid))]
    (dotimes [x (inc max-x)]
      (dotimes [y (inc max-y)]
        (print (format "%3d" (grid [x y]))))
      (println))))

(defn parse-grid
  ([lines]
   (into {} (reduce concat (map-indexed (fn [row line] (map-indexed #(vector [row %1] (str %2)) line)) lines))))
  ([lines value-fn]
   (into {} (reduce concat (map-indexed (fn [row line] (map-indexed #(vector [row %1] (value-fn (str %2))) line)) lines)))))

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