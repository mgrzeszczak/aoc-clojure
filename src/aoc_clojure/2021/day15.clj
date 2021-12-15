(ns aoc-clojure.2021.day15
  (:require [clojure.string :as str]
            [clojure.set :as sets]
            [aoc-clojure.lib.lib :as lib]))
(use 'clojure.java.io)
(require '[clojure.data.priority-map :refer [priority-map]])

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split-lines x)
        (lib/parse-grid x #(Integer/parseInt %))))

(defn min-dist [[x1 y1] [x2 y2]]
  (+ (lib/abs (- x1 x2)) (lib/abs (- y1 y2))))

(defn shortest-path [from to grid]
  (loop [q (priority-map from (min-dist from to))
         g {from 0}]
    (if (empty? q)
      nil
      (let [[current v] (peek q)]
        (if (= current to)
          v
          (let [ns (filter grid (lib/grid-neighbors-4 current))
                to-update (filter (fn [[p score]] (< score (or (g p) ##Inf))) (map (fn [p] [p (+ (g current) (grid p))]) ns))
                new-g (into g to-update)
                new-q (into (pop q) (map (fn [[p score]] [p (+ score (min-dist p to))]) to-update))]
            (recur new-q new-g)))))))

(defn full-grid [grid mx my]
  (let [wrap (fn [v] (if (> v 9) (mod v 9) v))]
    (into {} (for [x (range (* 5 (inc mx)))
                   y (range (* 5 (inc my)))]
               (let [rx (inc mx)
                     ry (inc my)
                     ox (mod x rx)
                     oy (mod y ry)
                     dx (quot x rx)
                     dy (quot y ry)]
                 [[x y] (wrap (+ (grid [ox oy]) dx dy))]
                 )))))

(time (as-> (parse-input "data/2021/d15") x
            (let [ks (keys x)
                  mx (apply max (map first ks))
                  my (apply max (map second ks))
                  start [0 0]
                  dest1 [mx my]
                  dest2 [(dec (* 5 (inc mx))) (dec (* 5 (inc my)))]
                  grid (full-grid x mx my)]
              [(shortest-path start dest1 x)
               (shortest-path start dest2 grid)])))