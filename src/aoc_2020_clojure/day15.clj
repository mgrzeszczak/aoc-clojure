(ns aoc-2020-clojure.day15
  (:require [clojure.string :as str]))

(def input [0, 5, 4, 1, 10, 14, 7])

(defn solve [prev record iter target]
  (loop [p prev
         r record
         i iter]
    (if (= i (inc target))
      p
      (let [last-turn (r p)
            prev-turn (dec i)]
        (if (nil? last-turn)
          (recur 0 (assoc r p prev-turn) (inc i))
          (recur (- prev-turn last-turn) (assoc r p prev-turn) (inc i)))))))

(time (as-> input x
            {:part1 (solve (last x) (into {} (map-indexed #(vector %2 (inc %1)) x)) (inc (count x)) 2020)
             :part2 (solve (last x) (into {} (map-indexed #(vector %2 (inc %1)) x)) (inc (count x)) 30000000)}))