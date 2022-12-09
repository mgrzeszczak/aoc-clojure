(ns aoc-clojure.2022.day01
  (:require [clojure.string :as str]))
(use 'clojure.java.io)

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split x #"\n\n")
        (map str/split-lines x)
        (map #(map (fn [i] (Integer/parseInt i)) %) x)
        x))

(defn solve []
  (as-> (parse-input "data/2022/d1") x
        (map #(reduce + %) x)
        [(reduce max x) (reduce + (take-last 3 (sort x)))]
        ))

(solve)
