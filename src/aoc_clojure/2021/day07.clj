(ns aoc-clojure.2021.day07
  (:require [clojure.string :as str]))
(use 'clojure.java.io)

(defn abs [n] (max n (-' n)))

(defn parse-line [line]
  (map #(Long/parseLong %) (str/split line #",")))

(defn parse-input [file-name]
  (->> (slurp file-name)
        (str/split-lines)
        (map parse-line)
        (first)))

(defn cost-p1[a b]
  (abs (- a b)))

(defn cost-p2[a b]
  (let [diff (abs (- a b))]
    (/ (* (inc diff) diff) 2)
    )
  )

(defn solve-p1[xs cost-fn]
  (let [positions (range (inc (reduce max xs)))
        costs (map vector positions (map #(reduce + (map (fn [x] (cost-fn x %)) xs)) positions))]
    (apply min-key second costs)))


(time (as-> (parse-input "data/2021/d7") x
            [(solve-p1 x cost-p1) (solve-p1 x cost-p2)]))