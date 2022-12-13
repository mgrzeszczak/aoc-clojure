(ns aoc-clojure.2022.day12
  (:require [clojure.string :as str]
            [aoc-clojure.lib.lib :as lib]
            [clojure.data.priority-map :refer [priority-map]]))
(use 'clojure.java.io)





(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split-lines x)
        (lib/parse-grid-2 x)
        x))

(defn is-neighbor [a b]
  (let [av (int (if (= a "S") \a (first a)))
        bv (int (if (= b "E") \z (first b)))]
    ;(<= (lib/abs (- av bv)) 1)
    (<= (- bv av) 1)
    )
  )

(defn neighbors [[x y] grid]
  (let [current (grid [x y])]
    (filter
      (fn [pos] (and (grid pos) (is-neighbor current (grid pos))))
      (lib/grid-neighbors-4 [x y]))
    )

  )

(defn shortest-path ([from
                      to
                      grid
                      neighbor-fn
                      heuristic-fn]
                     (loop [q (priority-map from (heuristic-fn from to))
                            g {from 0}]
                       (if (empty? q)
                         nil
                         (let [[current v] (peek q)]
                           (if (= current to)
                             v
                             (let [ns (filter grid (neighbor-fn current grid))
                                   to-update (filter (fn [[p score]] (< score (or (g p) ##Inf))) (map (fn [p] [p (+ (g current) 1)]) ns))
                                   new-g (into g to-update)
                                   new-q (into (pop q) (map (fn [[p score]] [p (+ score (heuristic-fn p to))]) to-update))]
                               (recur new-q new-g))))))))

(defn solve []
  (as-> (parse-input "data/2022/d12") x
        (let [start (ffirst (filter #(= "S" (second %)) x))
              end (ffirst (filter #(= "E" (second %)) x))
              starts (map first (filter #(#{"a" "S"} (second %)) x))
              ]
          [
           (shortest-path start end x neighbors (fn [a b] 0))
           (reduce min (filter identity (map #(shortest-path % end x neighbors (fn [a b] 0)) starts)))
           ]
          )))

(solve)
