(ns aoc-clojure.2022.day14
  (:require [clojure.string :as str]))
(use 'clojure.java.io)

(defn parse-line [line]
  (let [points (vec (map #(vec %) (map #(map (fn [x] (Integer/parseInt x)) (str/split % #",")) (str/split line #" -> "))))]
    (map vector (take (dec (count points)) points) (drop 1 points))
    )
  )

(defn generate-rocks[pairs]
  (into #{} (mapcat (fn[[[x1 y1] [x2 y2]]]
                      (for [x (range (min x1 x2) (inc (max x1 x2)))
                            y (range (min y1 y2) (inc (max y1 y2)))]
                        [x y])
                      ) pairs))


  )

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split-lines x)
        (mapcat parse-line x)
        (generate-rocks x)
        x))

(def sand-source [500 0])

(defn sand-moves[[x y]]
  [[x (inc y)] [(dec x) (inc y)] [(inc x) (inc y)]])

(defn p1 []
  (as-> (parse-input "data/2022/d14") rocks
        (let [max-y (apply max (map second rocks))]
          (loop [sand #{}
                 blocked rocks
                 [x y] sand-source]
            (if (> y max-y)
              sand
              (let [new-pos (first (filter #(not (blocked %)) (sand-moves [x y])))]
                (if new-pos
                  (recur sand blocked new-pos)
                  (recur (conj sand [x y]) (conj blocked [x y]) sand-source)
                  )
                )
            )))))

(count (p1))

(defn p2 []
  (as-> (parse-input "data/2022/d14") rocks
        (let [max-y (apply max (map second rocks))
              floor-level (inc max-y)]
          (loop [sand #{}
                 blocked rocks
                 [x y] sand-source]
            (if (sand sand-source)
              (count sand)
              (let [new-pos (first (filter #(not (blocked %)) (sand-moves [x y])))]
                (if new-pos
                  (if (= (second new-pos) floor-level)
                    (recur (conj sand new-pos) (conj blocked new-pos) sand-source)
                    (recur sand blocked new-pos)
                  )
                  (recur (conj sand [x y]) (conj blocked [x y]) sand-source)
                  )
                )
            )
            ))))

(p2)