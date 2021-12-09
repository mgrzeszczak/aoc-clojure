(ns aoc-clojure.2021.day09
  (:require [clojure.string :as str]
            [clojure.set :as sets]))
(use 'clojure.java.io)

(defn parse-row [row index]
  (map-indexed #(vector [index %1] (Integer/parseInt (str %2))) row))

(defn parse-grid [lines]
  (into {} (reduce concat (map-indexed #(parse-row %2 %1) lines))))

(defn parse-input [file-name]
  (->> (slurp file-name)
       (str/split-lines)
       (parse-grid)))

(defn neighbors [[x y]]
  (map (fn [[dx dy]] [(+ x dx) (+ y dy)]) [[0 1], [0 -1], [1 0], [-1 0]]))

(defn is-low-point [[p v] grid]
  (every? #(< v (grid %)) (filter #(grid %) (neighbors p))))


(defn find-basin [point grid]
  (if (= 9 (grid point))
    nil
    (loop [[q & rest] #{point}
           visited #{}]
      (if (or (nil? q))
        visited
        (let [ns (filter #(and (grid %) (not (visited %)) (not= 9 (grid %))) (neighbors q))]
          (recur (sets/union rest (into #{} ns)) (conj visited q)))))))

; p1
(time (as-> (parse-input "data/2021/d9") x
            (filter #(is-low-point % x) x)
            (reduce + (map #(inc (second %)) x))))
; p2
(time (as-> (parse-input "data/2021/d9") x
            (loop [[[p _] & rest] x
                   basins []
                   visited #{}]
              (if p
                (if (visited p)
                  (recur rest basins visited)
                  (let [basin (find-basin p x)]
                    (recur rest (if basin (conj basins basin) basins) (sets/union visited basin))))
                (reduce * (map count (take-last 3 (sort-by count basins))))))))