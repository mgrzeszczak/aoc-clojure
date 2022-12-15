(ns aoc-clojure.2022.day15
  (:require [aoc-clojure.lib.lib :as lib]
            [clojure.set :as set]
            [clojure.string :as str]))
(use 'clojure.java.io)

(defn parse-line [line]
  (let [[a b c d] (map #(Integer/parseInt %) (take-last 4 (first (re-seq #"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)" line))))]
    [[a b] [c d]]
    ))

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split-lines x)
        (map parse-line x)
        x))

(defn manhattan-dist [p1 p2]
  (reduce + (map #(lib/abs (- %1 %2)) p1 p2)))

(def row-y 2000000)

(defn scan [[sensor beacon]]
  (let [dist (manhattan-dist sensor beacon)
        [sx sy] sensor
        [bx by] beacon
        h (manhattan-dist sensor [sx row-y])
        diff (- dist h)]
    (if (> h dist)
      #{}
      (let [beacon-free (into #{} (map #(+ sx %) (range (- diff) (inc diff))))]
        (disj beacon-free bx)
        ))
    )
  )

(defn p1 []
  (as-> (parse-input "data/2022/d15") x
        (reduce set/union (map scan x))
        (count x)
        x))



(def max-coord 4000000)

(defn diamond-shape [[sx sy] d]
  (let [outside (inc d)
        offsets (range 0 (inc outside))
        points (mapcat #(vector
                          [(+ sx %) (+ sy (- outside %))]
                          [(+ sx %) (- sy (- outside %))]
                          [(- sx %) (+ sy (- outside %))]
                          [(- sx %) (- sy (- outside %))]
                          ) offsets)]
    (filter (fn [[x y]] (and (>= (min x y) 0) (<= (max x y) max-coord))) points)
    )
  )

(defn p2 []
  (as-> (parse-input "data/2022/d15") x
        (map (fn [[p1 p2]] [p1 p2 (manhattan-dist p1 p2)]) x)
        (let [shape-points (apply concat (map #(diamond-shape (first %) (nth % 2)) x))
              [bx by] (first (filter (fn [p] (every? #(> (manhattan-dist p (first %)) (nth % 2)) x)) shape-points))
              ]
          (+ (* bx 4000000) by)
          )
        x))

(time (p1))
(time (p2))