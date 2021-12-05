(ns aoc-clojure.2020.day24
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def dir-map {"e" :e "w" :w "ne" :ne "nw" :nw "se" :se "sw" :sw})

(def move-map {:e [1 -1 0] :w [-1 1 0] :ne [1 0 -1] :nw [0 1 -1] :se [0 -1 1] :sw [-1 0 1]})

(defn parse-line [line]
  (let [matcher (re-matcher #"(e)|(w)|(ne)|(nw)|(se)|(sw)" line)]
    (loop [output '()]
      (let [match (re-find matcher)]
        (if match (recur (conj output (first match)))
                  (map dir-map (reverse output)))))))

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split-lines x)
        (map parse-line x)))

(defn move [a b]
  (vec (map + a b)))

(defn flip [value]
  (case value :black :white :black))

(defn solve [lines]
  (let [center [0 0 0]]
    (loop [map {[0 0 0] :white}
           [line & rest-lines] lines]
      (if line
        (let [tile (reduce #(move %1 (move-map %2)) center line)]
          (recur (assoc map tile (flip (or (map tile) :white))) rest-lines))
        map))))

(defn neighbors [p]
  (map #(move p %) (vals move-map)))

(defn day-color [tile data]
  (let [c (count (filter #(not (nil? (data %))) (neighbors tile)))]
    (case (if (data tile) :black :white)
      :black (if (or (= c 0) (> c 2)) :white :black)
      :white (if (= c 2) :black :white))))

(defn game-of-life [initial-map days]
  (loop [day 1
         black-tiles (into #{} (map first (filter #(= (second %) :black) initial-map)))]
    (println day)
    (if (> day days)
      (count black-tiles)
      (let [tiles-with-neighbors (distinct (reduce concat (map #(conj (neighbors %) %) black-tiles)))
            new-black-tiles (into #{} (filter #(= :black (day-color % black-tiles)) tiles-with-neighbors))]
        (recur (inc day) new-black-tiles)))))

(time (as-> (parse-input "data/input_d24") x
            (solve x)
            (count (filter #(= % :black) (vals x)))))

(time (as-> (parse-input "data/input_d24") x
            (solve x)
            (game-of-life x 100)))
