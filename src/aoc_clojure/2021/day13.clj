(ns aoc-clojure.2021.day13
  (:require [clojure.string :as str]
            [clojure.set :as sets]
            [aoc-clojure.lib.lib :as lib]))
(use 'clojure.java.io)

(defn parse-fold [fold]
  (let [[coord value] (drop 1 (first (re-seq #"fold along (x|y)=(\d+)" fold)))]
    [coord (Integer/parseInt value)]))

(defn parse-folds [folds]
  (map parse-fold folds))

(defn parse-dot [dot]
  (vec (map #(Integer/parseInt %) (drop 1 (first (re-seq #"(\d+),(\d+)" dot))))))

(defn parse-dots [dots]
  (map parse-dot dots))

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split-lines x)
        (split-with #(not= "" %) x)
        {:dots (into #{} (parse-dots (first x))) :folds (parse-folds (drop 1 (second x)))}))

(defn fold-coord [x fx]
  (if (<= x fx) x (- fx (- x fx))))

(defn fold [dots [coord val]]
  (into #{} (map (fn [[x y]] (if (= "x" coord) [(fold-coord x val) y] [x (fold-coord y val)])) dots)))


(time (as-> (parse-input "data/2021/d13") x
            (count (fold (:dots x) (first (:folds x))))))

(time (as-> (parse-input "data/2021/d13") x
            (reduce fold (:dots x) (:folds x))
            (lib/print-points x)))