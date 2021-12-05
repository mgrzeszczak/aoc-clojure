(ns aoc-clojure.2021.day05
  (:require [clojure.string :as str]))
(use 'clojure.java.io)

(defn parse-line [line]
  (as-> (re-seq #"(\d+),(\d+)\s+->\s+(\d+),(\d+)" line) x
        (first x)
        (drop 1 x)
        (map #(Integer/parseInt %) x)
        (vec x)))

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split-lines x)
        (map parse-line x)))

(defn abs [n] (max n (-' n)))

(defn determine-path [p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2
        [d1 d2] [(- x2 x1) (- y2 y1)]
        div (max (abs d1) (abs d2))
        dir (if (= 0 div) [d1 d2] [(/ d1 div) (/ d2 div)])]
    (for [n (range (inc div))]
      (map + p1 (map * dir [n n])))))

(defn update-state [state p1 p2]
  (reduce #(assoc %1 %2 (inc (or (%1 %2) 0))) state (determine-path p1 p2)))

(defn solve [lines]
  (loop [[c & rest] lines
         state {}]
    (if c
      (let [[x1 y1 x2 y2] c]
        (recur rest (update-state state [x1 y1] [x2 y2])))
      (count (filter #(>= (second %) 2) state)))))

(time (as-> (parse-input "data/2021/d5") x
            (solve (filter #(or (= (nth % 0) (nth % 2)) (= (nth % 1) (nth % 3))) x))))

(time (as-> (parse-input "data/2021/d5") x
            (solve x)))