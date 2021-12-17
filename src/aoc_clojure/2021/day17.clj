(ns aoc-clojure.2021.day17
  (:require [clojure.string :as str]
            [clojure.set :as sets]
            [aoc-clojure.lib.lib :as lib]))
(use 'clojure.java.io)
(require '[clojure.data.priority-map :refer [priority-map]])

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (drop 1 (first (re-seq #"target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)" x)))
        (map #(Integer/parseInt %) x)
        ))

(defn check [[x y] [xmin xmax ymin ymax]]
  (and (<= xmin x xmax) (<= ymin y ymax)))

(defn too-late [[x y] [dx dy] [xmin xmax ymin ymax]]
  (or
    (and (> x xmax) (> dx 0))
    (and (< y ymin) (< dy 0))))

(defn launch [init-velocity area]
  (loop [[x y] [0 0]
         [dx dy] init-velocity
         highest-y 0]
    (if (check [x y] area)
      highest-y
      (if (too-late [x y] [dx dy] area)
        nil
        (recur [(+ x dx) (+ y dy)] [(+ dx (cond (> dx 0) -1 (< dx 0) 1 :else 0)) (- dy 1)] (max (+ y dy) highest-y))
        ))))

(time (as-> (parse-input "data/2021/d17") x
            (let [area x
                  results (filter #(not= nil %)
                                  (for [dx (range 200)
                                        dy (range -400 400)]
                                    (launch [dx dy] area)
                                    ))]
              [(apply max results) (count results)])))