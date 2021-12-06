(ns aoc-clojure.2021.day06
  (:require [clojure.string :as str]))
(use 'clojure.java.io)

(defn parse-line [line]
  (map #(Long/parseLong %) (str/split line #",")))

(defn parse-input [file-name]
  (->> (slurp file-name)
        (str/split-lines)
        (map parse-line)
        (first)))

(defn next[[zeros ones twos threes fours fives sixes sevens eights]]
  [ones twos threes fours fives sixes (+ sevens zeros) eights zeros])

(defn solve [xs days]
  (let [data (group-by identity xs)
        state (map #(or (count (data %)) 0) (range 9))]
    (reduce + (reduce (fn [s _] (next s)) state (range days)))))

(time (as-> (parse-input "data/2021/d6") x
            [(solve x 80) (solve x 256)]))