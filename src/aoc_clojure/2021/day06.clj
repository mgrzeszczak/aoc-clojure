(ns aoc-clojure.2021.day06
  (:require [clojure.string :as str]))
(use 'clojure.java.io)

(defn parse-line [line]
  (map #(Long/parseLong %) (str/split line #",")))

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split-lines x)
        (map parse-line x)
        (first x)))

(defn solve [nums days]
  (loop [i 0
         state (into {} (map #(vec (list (first %) (count (second %)))) (group-by identity nums)))]
    (if (= i days)
      (reduce + (map second state))
      (recur
        (inc i)
        (let [zeros (or (state 0) 0)
              partial-state (map #(vec (list (dec (first %)) (second %))) (filter #(not= 0 (first %)) state))
              with-new (into {} (if (> zeros 0) (cons [8 zeros] partial-state) partial-state))
              six-count (or (with-new 6) 0)]
          (assoc with-new 6 (+ six-count zeros)))))))

(time (as-> (parse-input "data/2021/d6") x
            [(solve x 80) (solve x 256)]))