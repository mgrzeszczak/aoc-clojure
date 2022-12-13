(ns aoc-clojure.2022.day13
  (:require [clojure.string :as str]))
(use 'clojure.java.io)

(defn parse-pair [x]
  (map read-string (str/split x #"\n")))

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split x #"\n\n")
        (map parse-pair x)
        x))

(defn order [l r]
  (let [left-number (instance? Long l)
        right-number (instance? Long r)]
    (if left-number
      (if right-number
        (- l r)
        (order [l] r)
        )
      (if right-number
        (order l [r])
        (loop [[cl & lrem] l
               [cr & rrem] r]
          (if (not cl)
            (if (not cr) 0 -1)
            (if (not cr)
              1
              (let [res (order cl cr)]
                (if (not= 0 res) res (recur lrem rrem))))))))))

(defn p1 []
  (as-> (parse-input "data/2022/d13") x
        (map-indexed (fn [i [l r]] [(inc i) [l r]]) x)
        (filter (fn [[_ [l r]]] (<= (order l r) 0)) x)
        (map first x)
        (reduce + x)))

(defn p2 []
  (as-> (parse-input "data/2022/d13") x
        (apply concat x)
        (concat x [[[2]] [[6]]])
        (sort order x)
        (map-indexed #(vector (inc %1) %2) x)
        (filter #(#{[[2]] [[6]]} (second %)) x)
        (reduce * (map first x))))

[(p1) (p2)]