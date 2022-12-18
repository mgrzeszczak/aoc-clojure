(ns aoc-clojure.2022.day18
  (:require [aoc-clojure.lib.lib :as lib]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))
(use 'clojure.java.io)

(defn parse-line[line]
  (vec (map #(Integer/parseInt %) (str/split line #","))))

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split-lines x)
        (map parse-line x)
        ))

(defn add-vec[[x1 y1 z1] [x2 y2 z2]]
  [(+ x1 x2) (+ y1 y2) (+ z1 z2)])

(defn negate-vec[[a b c]]
  [(- a) (- b) (- c)])

(def x-axis[1 0 0])
(def y-axis[0 1 0])
(def z-axis[0 0 1])

(defn map-to-surface[cube-pos]
  (let [a cube-pos
        b (add-vec cube-pos x-axis)
        c (add-vec b z-axis)
        d (add-vec a z-axis)
        e (add-vec a y-axis)
        f (add-vec b y-axis)
        g (add-vec c y-axis)
        h (add-vec d y-axis)]
    #{
     #{a b c d} ; bottom
     #{e f g h} ; top
     #{a b e f} ; front
     #{c d g h} ; back
     #{a d e h} ; left
     #{b c f g} ; right
      }
    )
  )

(defn find-surfaces[data]
  (reduce #(set/difference (set/union %1 %2) (set/intersection %1 %2)) (map map-to-surface data)))

(defn p1[data]
  (count (find-surfaces data))
  )

(defn p2[data]
  (let [x-min (dec (apply min (map first data)))
        x-max (inc (apply max (map first data)))
        y-min (dec (apply min (map second data)))
        y-max (inc (apply max (map second data)))
        z-min (dec (apply min (map #(nth % 2) data)))
        z-max (inc (apply max (map #(nth % 2) data)))
        surfaces (find-surfaces data)
        cubes (into #{} data)
        valid (fn [[x y z]] (and (>= x x-min) (<= x x-max) (>= y y-min) (<= y y-max) (>= z z-min) (<= z z-max)))
        ]
    (loop [[curr & rem] #{[x-min y-min z-min]}
           visited #{}
           seen-surfaces #{}]
      (if (not curr)
        (count seen-surfaces)
        (let [axes [x-axis y-axis z-axis]
              moves (vec (concat axes (map negate-vec axes)))
              to-visit (filter #(and (not (visited %)) (not (cubes %))) (filter valid (map #(add-vec curr %) moves)))
              current-surfaces (map-to-surface curr)
              new-surfaces (filter surfaces current-surfaces)
              ]
          (recur (into rem to-visit) (into visited [curr]) (into seen-surfaces new-surfaces))
          )

        )
      )
    ))

(defn solve []
  (let [data (parse-input "data/2022/d18")]
    (p2 data)
    )
  )

(time (solve))