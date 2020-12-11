(ns aoc-2020-clojure.day11
  (:require [clojure.set :as set])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(defn to-index [[x y] w]
  (+ x (* y w)))

(defn from-index [i w]
  [(mod i w) (int (/ i w))])

(defn iteration-p1 [index seat width height seats]
  (let [
        [sx sy] (from-index index width)
        adj-x (map #(+ sx %) [-1 0 1])
        adj-y (map #(+ sy %) [-1 0 1])
        nx (filter #(and (>= % 0) (< % width)) adj-x)
        ny (filter #(and (>= % 0) (< % height)) adj-y)
        ns (filter #(not= % [sx sy]) (for [x nx y ny] [x y]))
        ni (map #(to-index % width) ns)
        neighbors (map #(seats %) ni)
        occupied (count (filter #(= % "#") neighbors))]
    (case seat
      "." "."
      "L" (if (= 0 occupied) "#" "L")
      "#" (if (>= occupied 4) "L" "#"))
    )
  )

(defn solve-p1 [data width height]
  (loop [x data]
    (let [new-data (vec (map-indexed #(iteration-p1 %1 %2 width height x) x))]

      (if (= x new-data) x (recur new-data)))

    )
  )

; part 1
(time (as-> (slurp "data/input_d11") x
            (str/split-lines x)
            (map #(str/split % #"") x)
            {:data (vec (flatten x)) :width (count (first x)) :height (count x)}
            (solve-p1 (:data x) (:width x) (:height x))
            (count (filter #(= % "#") x))
            ))

; part 2

(defn find-visible-seat [[x y] [dx dy] data w h]
  (loop [[px py] [x y]]
    (let [np [(+ px dx) (+ py dy)]
          [nx ny] np]
      (if (or (< nx 0) (< ny 0) (>= nx w) (>= ny h))
        nil
        (if (= "L" (data (to-index np w)))
          np
          (recur np))))))

(defn dirs []
  (filter #(not= % [0 0])
          (for [x [-1 0 1] y [-1 0 1]] [x y])))

(defn visible-seats [index data w h]
  (let [[x y] (from-index index w)
        ds (dirs)]
    (filter #(not= nil %) (map #(find-visible-seat [x y] % data w h) ds))))


(defn iteration-p2 [index seat w h data visible-seats]
  (let [occupied (count (filter #(= "#" %) (map #(data (to-index % w)) visible-seats)))]
    (case seat
      "." "."
      "L" (if (= 0 occupied) "#" "L")
      "#" (if (>= occupied 5) "L" "#"))))

(defn solve-p2 [data width height visible-seats]
  (loop [x data i 0]
    (println i)
    (let [new-data (vec (map-indexed #(iteration-p2 %1 %2 width height x (visible-seats %1)) x))]
      (if (= x new-data) x (recur new-data (inc i))))))


; part 2
(time (as-> (slurp "data/input_d11") x
            (str/split-lines x)
            (map #(str/split % #"") x)
            {:data (vec (flatten x)) :width (count (first x)) :height (count x)}
            (assoc x :visible-seats
                     (into {} (map-indexed (fn [i v] [i (visible-seats i (:data x) (:width x) (:height x))]) (:data x))))
            (solve-p2 (:data x) (:width x) (:height x) (:visible-seats x))
            (count (filter #(= % "#") x))))

