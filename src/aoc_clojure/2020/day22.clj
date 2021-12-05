(ns aoc-clojure.2020.day22
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-deck [lines]
  (vec (map #(Integer/parseInt %) (drop 1 lines))))

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split x #"\n\n")
        (map #(str/split-lines %) x)
        (map parse-deck x)))

; part 1

(defn solve-p1 [d1 d2]
  (loop [[top1 & rest1 :as all1] d1
         [top2 & rest2 :as all2] d2]
    (if (or (nil? top1) (nil? top2))
      (if (nil? top1) all2 all1)
      (if (> top1 top2)
        (recur (concat rest1 [top1 top2]) rest2)
        (recur rest1 (concat rest2 [top2 top1]))))))

(time (as-> (parse-input "data/2020/input_d22") x
            (solve-p1 (first x) (second x))
            (reduce + (map-indexed #(* (inc %1) %2) (reverse x)))))

; part 2

(defn solve-p2 [d1 d2]
  (loop [[top1 & rest1 :as all1] d1
         [top2 & rest2 :as all2] d2
         history #{}]
    (if (history [all1 all2])
      [:p1 all1]
      (let [count1 (count rest1)
            count2 (count rest2)
            new-history (conj history [all1 all2])]
        (if (and (> (min count1 count2) 0) (>= count1 top1) (>= count2 top2))
          (let [[winner _] (solve-p2 (take top1 rest1) (take top2 rest2))]
            (recur
              (if (= :p1 winner) (concat rest1 [top1 top2]) rest1)
              (if (= :p2 winner) (concat rest2 [top2 top1]) rest2)
              new-history))
          (if (or (nil? top1) (nil? top2))
            (if (nil? top1) [:p2 all2] [:p1 all1])
            (if (> top1 top2)
              (recur (concat rest1 [top1 top2]) rest2 new-history)
              (recur rest1 (concat rest2 [top2 top1]) new-history))))))))

(time (as-> (parse-input "data/2020/input_d22") x
            (solve-p2 (first x) (second x))
            (second x)
            (reduce + (map-indexed #(* (inc %1) %2) (reverse x)))))
