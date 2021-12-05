(ns aoc-clojure.2020.day09
  (:require [clojure.set :as set])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

; part 1
(defn solve-p1 [pos sums nums data]
  (if (< (count nums) 25)
    (let [value (data pos)
          newSums (map #(+ % value) nums)]
      (solve-p1
        (inc pos)
        (reduce #(assoc %1 %2 (inc (or (%1 %2) 0))) sums newSums)
        (conj nums value)
        data))
    (let [value (data pos)
          last (last nums)
          l24 (drop-last 1 nums)
          newSums (map #(+ % value) l24)]
      (if (and (or (nil? (sums value)) (= (sums value) 0)) (= (count nums) 25))
        value
        (solve-p1
          (inc pos)
          (let [m (reduce #(assoc %1 %2 (dec (%1 %2))) sums (map #(+ last %) l24))]
            (reduce #(assoc %1 %2 (inc (or (%1 %2) 0))) m newSums))
          (conj l24 value)
          data)))))

(defn solve-p2
  ([target data]
   (solve-p2 0 0 0 target data))
  ([currentPosition startPos sum target data]
   (let [nSum (+ sum (data currentPosition))]
     (cond
       (= nSum target) (let [vals (map data (range startPos (inc currentPosition)))]
                         (+ (apply min vals) (apply max vals)))
       (< nSum target) (recur (inc currentPosition) startPos nSum target data)
       (> nSum target) (recur currentPosition (inc startPos) (- sum (data startPos)) target data)))))

(time (as-> (slurp "data/2020/input_d09") x
            (str/split-lines x)
            (map #(Long/parseLong %) x)
            (vec x)
            (list (solve-p1 0 {} '() x) x)
            (list (first x) (solve-p2 (first x) (second x)))))



