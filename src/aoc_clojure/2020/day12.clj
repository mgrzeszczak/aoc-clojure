(ns aoc-clojure.2020.day12
  (:require [clojure.string :as str]))

(defn parse-data [inp]
  (as-> (str/split-lines inp) x
        (map #(re-matches #"(\w)(\d+)" %) x)
        (map #(vector (nth % 1) (Integer/parseInt (nth % 2))) x)))

(def dirs ["N", "E", "S", "W"])
(def moves {"N" [0 -1]
            "E" [1 0]
            "S" [0 1]
            "W" [-1 0]})

(defn rotate [curr dir deg]
  (dirs (mod (+ (.indexOf dirs curr) (* (int (/ deg 90)) (case dir "L" -1 "R" 1))) (count dirs))))

(defn move [position dir n]
  (vec (map + position (map #(* % n) (moves dir)))))

(defn manhattan-dist [p1 p2]
  (reduce + (map #(Math/abs (- %1 %2)) p1 p2)))

(defn rotate-waypoint [[wx wy] dir deg]
  (if (= deg 0)
    [wx wy]
    (rotate-waypoint
      (case dir
        "L" [wy (- wx)]
        "R" [(- wy) wx])
      dir
      (- deg 90))))

(defn solve-p1 [instructions]
  (loop [[x y] [0 0]
         curr "E"
         ind 0]
    (if (>= ind (count instructions))
      (manhattan-dist [0 0] [x y])
      (let [[instr n] (instructions ind)]
        (case instr
          "F" (recur (move [x y] curr n) curr (inc ind))
          "L" (recur [x y] (rotate curr instr n) (inc ind))
          "R" (recur [x y] (rotate curr instr n) (inc ind))
          (recur (move [x y] instr n) curr (inc ind)))))))

(defn solve-p2 [instructions]
  (loop [[x y] [0 0]
         [wx wy] [10 -1]
         ind 0]
    (if (>= ind (count instructions))
      (manhattan-dist [0 0] [x y])
      (let [[instr n] (instructions ind)]
        (case instr
          "F" (recur [(+ x (* wx n)) (+ y (* wy n))] [wx wy] (inc ind))
          "L" (recur [x y] (rotate-waypoint [wx wy] instr n) (inc ind))
          "R" (recur [x y] (rotate-waypoint [wx wy] instr n) (inc ind))
          (recur [x y] (move [wx wy] instr n) (inc ind)))))))

(time (as-> (slurp "data/input_d12") x
            (parse-data x)
            (vec x)
            (vector (solve-p1 x) (solve-p2 x))))