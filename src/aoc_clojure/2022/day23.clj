(ns aoc-clojure.2022.day23
  (:require [aoc-clojure.lib.lib :as lib]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            ))
(use 'clojure.java.io)

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (lib/parse-grid-2 (str/split-lines x))
        (into #{} (map first (filter #(= "#" (second %)) x)))
        )
  )

(defn dir-dx [dir]
  (case dir
    :north [0 -1]
    :south [0 1]
    :west [-1 0]
    :east [1 0]
    ))

(defn result [elves]
  (let [min-x (apply min (map first elves))
        max-x (apply max (map first elves))
        min-y (apply min (map second elves))
        max-y (apply max (map second elves))]
    (- (* (inc (- max-x min-x)) (inc (- max-y min-y))) (count elves))
    )
  )

(defn add-vec [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)]
  )

(def north-ns [[-1 -1] [0 -1] [1 -1]])
(def south-ns [[-1 1] [0 1] [1 1]])
(def west-ns [[-1 -1] [-1 0] [-1 1]])
(def east-ns [[1 -1] [1 0] [1 1]])

(defn check-neighbors [elf elves]
  {
   :north (if (some #(elves %) (map #(add-vec elf %) north-ns)) false true)
   :south (if (some #(elves %) (map #(add-vec elf %) south-ns)) false true)
   :west (if (some #(elves %) (map #(add-vec elf %) west-ns)) false true)
   :east (if (some #(elves %) (map #(add-vec elf %) east-ns)) false true)
   }
  )

(defn propose-move [elf elves directions]
  (if (not (some elves (lib/grid-neighbors-8 elf)))
    nil
    (let [ns-check (check-neighbors elf elves)]
      (first (map #(add-vec elf (dir-dx %)) (filter ns-check directions)))
      )
    )
  )

(defn next-directions[[f & rem]]
  (conj (vec rem) f)
  )

(defn print-elves[elves round]
  (println "After round" round)
  (lib/print-grid (into {} (map #(vector % "#") elves)))
  (println)
  )

(defn p1 [elves rounds]
  (loop [directions [:north :south :west :east]
         elves elves
         round 0]
    (print-elves elves round)
    (println directions)
    (if (= round rounds)
      (result elves)
      (let [proposed-moves (into {} (map #(vector % (propose-move % elves directions)) elves))
            target-counted (into {} (map #(vector (first %) (count (second %))) (group-by identity (filter #(not (nil? %)) (map second proposed-moves)))))
            next-elves (into #{} (map (fn[elf] (let [move (proposed-moves elf)]
                                       (if (= 1 (target-counted move)) move elf)
                                       )) elves))
            ]
        (recur
          (next-directions directions)
          next-elves
          (inc round)
          )

        )
      )
    )
  )

(defn p2 [elves]
  (loop [directions [:north :south :west :east]
         elves elves
         round 1]
    (if (= 0 (mod round 100)) (println round))
      (let [proposed-moves (into {} (map #(vector % (propose-move % elves directions)) elves))
            target-counted (into {} (map #(vector (first %) (count (second %))) (group-by identity (filter #(not (nil? %)) (map second proposed-moves)))))
            next-elves (into #{} (map (fn[elf] (let [move (proposed-moves elf)]
                                                 (if (= 1 (target-counted move)) move elf)
                                                 )) elves))
            ]
        (if (= next-elves elves)
          round
          (recur
            (next-directions directions)
            next-elves
            (inc round)
            )
          )



      )
    )
  )

(defn solve []
  (let [elves (parse-input "data/2022/d23")
        ]
    ;(p1 elves 10)
    (p2 elves)
    )
  )

(time (solve))