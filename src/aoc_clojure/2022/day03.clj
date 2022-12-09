(ns aoc-clojure.2022.day03
  (:require [clojure.string :as str]
            [clojure.set :as sets]))
(use 'clojure.java.io)

(defn split-rucksack[items]
  (map #(into #{} %) (partition-all (/ (count items) 2) items)))

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split x #"\n")
        x))

(defn item-priority[^Character item]
  (if (Character/isUpperCase item)
    (+ (- (long item) 65) 27)
    (+ (- (long item) 97) 1)
    )
  )

(defn solve-p1[]
  (as-> (parse-input "data/2022/d3") x
        (map split-rucksack x)
        (map #(apply sets/intersection %) x)
        (map #(reduce + (map item-priority %)) x)
        (reduce + x)
        )
  )

(defn solve-p2[]
  (as-> (parse-input "data/2022/d3") x
        (map #(into #{} %) x)
        (partition-all 3 x)
        (map #(apply sets/intersection %) x)
        (reduce + (map #(item-priority (first %)) x))
        )
  )

(solve-p1)

(solve-p2)