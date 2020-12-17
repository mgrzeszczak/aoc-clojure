(ns aoc-2020-clojure.day17
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-input [file]
  (as-> (slurp file) x
        (str/split-lines x)
        (vec (map #(str/split % #"") x))

        (map-indexed #(vector %1 %2) x)
        (map (fn [[row vals]] (map-indexed #(vector [%1 row 0] %2) vals)) x)
        (reduce concat x)
        (filter #(= (second %) "#") x)
        (into {} x)))

(defn neighbors [[x y z]]
  (filter #(not= [x y z] %)
          (map #(vec (map + % [x y z]))
               (for [dx [-1 0 1]
                     dy [-1 0 1]
                     dz [-1 0 1]]
                 [dx dy dz]))))

(defn iteration [active]
  (let [coords (keys active)
        minX (apply min (map first coords))
        maxX (apply max (map first coords))
        minY (apply min (map second coords))
        maxY (apply max (map second coords))
        minZ (apply min (map #(nth % 2) coords))
        maxZ (apply max (map #(nth % 2) coords))]
    (into {}
          (filter
            #(= "#" (second %))
            (for [x (range (dec minX) (+ maxX 2))
                  y (range (dec minY) (+ maxY 2))
                  z (range (dec minZ) (+ maxZ 2))]
              (let [key [x y z]
                    nkeys (neighbors key)
                    cellActive (not (nil? (active key)))
                    activeNeighbors (count (filter #((comp not nil?) (active %)) nkeys))]
                [key
                 (if cellActive
                   (if (#{2, 3} activeNeighbors) "#" ".")
                   (if (= 3 activeNeighbors) "#" ".")
                   )]))))))

(defn solve [initial iterations]
  (reduce (fn [v _] (iteration v)) initial (range iterations)))

; part 1

(time (as-> (parse-input "data/input_d17") x
            (solve x 6)
            (count x)))


; part 2

(defn parse-input-p2 [file]
  (as-> (slurp file) x
        (str/split-lines x)
        (vec (map #(str/split % #"") x))
        (map-indexed #(vector %1 %2) x)
        (map (fn [[row vals]] (map-indexed #(vector [%1 row 0 0] %2) vals)) x)
        (reduce concat x)
        (filter #(= (second %) "#") x)
        (into {} x)))

(defn neighbors-p2 [[x y z w]]
  (filter #(not= [x y z w] %)
          (map #(vec (map + % [x y z w]))
               (for [dx [-1 0 1]
                     dy [-1 0 1]
                     dz [-1 0 1]
                     dw [-1 0 1]]
                 [dx dy dz dw]))))

(defn iteration-p2 [active]
  (let [coords (keys active)
        minX (apply min (map first coords))
        maxX (apply max (map first coords))
        minY (apply min (map second coords))
        maxY (apply max (map second coords))
        minZ (apply min (map #(nth % 2) coords))
        maxZ (apply max (map #(nth % 2) coords))
        minW (apply min (map #(nth % 3) coords))
        maxW (apply max (map #(nth % 3) coords))]
    (into {}
          (filter
            #(= "#" (second %))
            (for [x (range (dec minX) (+ maxX 2))
                  y (range (dec minY) (+ maxY 2))
                  z (range (dec minZ) (+ maxZ 2))
                  w (range (dec minW) (+ maxW 2))]
              (let [key [x y z w]
                    nkeys (neighbors-p2 key)
                    cellActive (not (nil? (active key)))
                    activeNeighbors (count (filter #((comp not nil?) (active %)) nkeys))]
                [key
                 (if cellActive
                   (if (#{2, 3} activeNeighbors) "#" ".")
                   (if (= 3 activeNeighbors) "#" ".")
                   )]))))))

(defn solve-p2 [initial iterations]
  (reduce (fn [v _] (iteration-p2 v)) initial (range iterations)))

; part 1

(time (as-> (parse-input-p2 "data/input_d17") x
            (solve-p2 x 6)
            (count x)))