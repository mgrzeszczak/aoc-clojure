(ns aoc-2020-clojure.day11
  (:require [clojure.string :as str]))

(defn to-index [[x y] w]
  (+ x (* y w)))

(defn from-index [i w]
  [(mod i w) (int (/ i w))])

(defn dirs []
  (filter #(not= % [0 0])
          (for [x [-1 0 1] y [-1 0 1]] [x y])))

(defn seat-occupied? [seat] (= "#" seat))
(defn seat-empty? [seat] (= "L" seat))

(defn find-visible-seat
  ([[x y] [dx dy] data w h] (find-visible-seat [x y] [dx dy] data w h 1))
  ([[x y] [dx dy] data w h maxJumps]
   (loop [[px py] [x y]
          i 1]
     (let [np [(+ px dx) (+ py dy)]
           [nx ny] np]
       (if (or (< nx 0) (< ny 0) (>= nx w) (>= ny h))
         nil
         (if (seat-empty? (data (to-index np w)))
           np
           (if (= i maxJumps) nil (recur np (inc i)))))))))

(defn visible-seats
  ([index data w h] (visible-seats index data w h 1))
  ([index data w h maxJumps]
   (let [[x y] (from-index index w)
         ds (dirs)]
     (filter #(not= nil %) (map #(find-visible-seat [x y] % data w h maxJumps) ds)))))

(defn iteration [seat width data visible-seats occupiedLimit]
  (let [occupied (count (filter seat-occupied? (map #(data (to-index % width)) visible-seats)))]
    (case seat
      "L" (if (= 0 occupied) "#" "L")
      "#" (if (>= occupied occupiedLimit) "L" "#")
      seat)))

(defn solve [data width visible-seats occupiedLimit]
  (count (filter seat-occupied?
                 (loop [x data i 0]
                   (let [new-data (vec (map-indexed #(iteration %2 width x (visible-seats %1) occupiedLimit) x))]
                     (if (= x new-data) x (recur new-data (inc i))))))))

(defn parse-data [input]
  (as-> input x
        (str/split-lines x)
        (map #(str/split % #"") x)
        {:data (vec (flatten x)) :width (count (first x)) :height (count x)}))

(defn cache-visible-seats [x maxJumps]
  (into {} (map-indexed (fn [i v] [i (visible-seats i (:data x) (:width x) (:height x) maxJumps)]) (:data x))))

(def parts {:part1 {:occupiedLimit 4 :maxJumps 1} :part2 {:occupiedLimit 5 :maxJumps -1}})

(defn prepare-data [x maxJumps]
  (assoc x :visible-seats (cache-visible-seats x maxJumps)))

(time (as-> (slurp "data/input_d11") x
            (parse-data x)
            (map #(list (first %) (as-> (prepare-data x (:maxJumps (second %))) x
                                        (solve (:data x) (:width x) (:visible-seats x) (:occupiedLimit (second %))))) parts)))