(ns aoc-clojure.2021.day20
  (:require [clojure.string :as str]
            [clojure.set :as sets]
            [aoc-clojure.lib.lib :as lib]))
(use 'clojure.java.io)
(require '[clojure.data.priority-map :refer [priority-map]])

(defn parse-algorithm [alg]
  (into {} (map-indexed #(vector %1 (str %2)) alg)))

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split x #"\n\n")
        (let [[alg-txt input] x
              alg (parse-algorithm alg-txt)
              grid (lib/parse-grid (str/split-lines input))
              fixed-grid (into {} (map (fn [[[x y] v]] [[y x] v]) grid))]
          [fixed-grid alg])))

(def eval-map {"." "0" "#" "1"})

(defn window [[x y] grid default alg]
  (let [binary (apply str
                      (for [
                            dy [-1 0 1]
                            dx [-1 0 1]
                            ]
                        (eval-map (or (grid [(+ x dx) (+ y dy)]) default))))
        ]
    ;(println [x y] binary (Integer/parseInt binary 2) (alg (Integer/parseInt binary 2)))
    (Integer/parseInt binary 2)))

(defn size [img]
  (let [xs (map first (keys img))
        ys (map second (keys img))
        x-min (reduce min xs)
        x-max (reduce max xs)
        y-min (reduce min ys)
        y-max (reduce max ys)]
    [x-min x-max y-min y-max]))

(defn next-default [default alg]
  (alg (Integer/parseInt (apply str (repeat 9 (eval-map default))) 2)))

(defn enhance [img default alg]
  (let [[x-min x-max y-min y-max] (size img)
        new-img (into {} (for [
                               x (range (- x-min 2) (inc (+ x-max 2)))
                               y (range (- y-min 2) (inc (+ y-max 2)))
                               ]
                           [[x y] (alg (window [x y] img default alg))]
                           ))
        new-default (next-default default alg)
        ]
    [new-img new-default]))

(defn lit-pixel-count[img]
  (count (filter #(= "#" %) (vals img))))

(defn enhance-n[img alg n]
  (loop [i 0
         default "."
         img img
         alg alg]
    (println i)
    (if
      (= i n)
      (lit-pixel-count img)
      (let [[new-img new-default] (enhance img default alg)]
        (recur (inc i) new-default new-img alg)
        ))))

(time (as-> (parse-input "data/2021/d20") x
            (let [[img alg] x]
              [(enhance-n img alg 2) (enhance-n img alg 50)])))
