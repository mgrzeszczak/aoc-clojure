(ns aoc-clojure.lib.lib)

(defn abs [n] (max n (-' n)))

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