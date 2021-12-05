(ns aoc-clojure.2020.day25
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split-lines x)
        (map #(Long/parseLong %) x)))

(defn crack [v]
  (loop [i 0
         current 1]
    (if (= current v)
      i
      (recur (inc i) (mod (* 7 current) 20201227)))))

(defn transform [subject loop-size]
  (reduce #(mod (* %1 %2) 20201227) 1 (repeat loop-size subject)))

(defn solve [[v1 v2]]
  (transform v2 (crack v1)))

(time (as-> (parse-input "data/2020/input_d25") x
            (solve x)))
