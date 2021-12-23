(ns aoc-clojure.2021.day22
  (:require [clojure.string :as str]
            [clojure.set :as sets]
            [aoc-clojure.lib.lib :as lib]))
(use 'clojure.java.io)
(require '[clojure.data.priority-map :refer [priority-map]])

(defn parse-line [line]
  (let [[switch & rest] (drop 1 (first (re-seq #"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)" line)))]
    (concat (map #(Integer/parseInt %) rest) [(if (= "on" switch) 1 -1)])))

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split-lines x)))

(defn intersection-1d [[a b] [c d]]
  (let [l (max a c)
        r (min b d)]
    (if (<= l r)
      [l r]
      nil)))

(defn intersects [[x1-from x1-to y1-from y1-to z1-from z1-to on] [x2-from x2-to y2-from y2-to z2-from z2-to _]]
  (let [xs (intersection-1d [x1-from x1-to] [x2-from x2-to])
        ys (intersection-1d [y1-from y1-to] [y2-from y2-to])
        zs (intersection-1d [z1-from z1-to] [z2-from z2-to])]
    (if (every? identity [xs ys zs])
      (reduce concat [xs ys zs [(- on)]])
      nil)))

(defn include-p1? [cuboid]
  (every? #(<= -50 % 50) (take 6 cuboid)))

(defn size [[x1-from x1-to y1-from y1-to z1-from z1-to _]]
  (reduce * [(inc (- x1-to x1-from)) (inc (- y1-to y1-from)) (inc (- z1-to z1-from))]))

(defn solve [rules]
  (loop [cuboids []
         [current & rest] rules]
    (if (nil? current)
      (reduce + (map #(* (size %) (last %)) cuboids))
      (let [new-cuboids (filter identity (map
                                           (fn [c] (intersects c current))
                                           cuboids))]
        (recur (concat cuboids (if (= 1 (last current)) (concat new-cuboids [current]) new-cuboids)) rest)))))

(time (as-> (parse-input "data/2021/d22") x
            (map parse-line x)
            (let [for-p2 x
                  for-p1 (filter include-p1? for-p2)]
              [(solve for-p1) (solve for-p2)])))