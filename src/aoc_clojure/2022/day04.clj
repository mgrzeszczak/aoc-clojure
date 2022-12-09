(ns aoc-clojure.2022.day04
  (:require [clojure.string :as str]
            [clojure.set :as sets]))
(use 'clojure.java.io)

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split x #"\n")
        (map #(re-seq #"(\d+)-(\d+)" %) x)
        (map #(vector
                (vec (map (fn [a] (Integer/parseInt a)) (drop 1 (first %))) )
                (vec (map (fn [a] (Integer/parseInt a)) (drop 1 (second %))) )
                ) x)
        x))

(defn contains[[a b] [c d]]
  (or
    (and (>= a c) (<= b d))
    (and (>= c a) (<= d b))
    ))

(defn overlaps[[a b] [c d]]
  (and (<= a d) (>= b c))
  ;(or
    ;(and (>= a c) (<= b d))
    ;(and (>= c a) (<= d b))
    ;)
  )


(defn solve-p1[]
  (as-> (parse-input "data/2022/d4") x
        (count (filter #(apply contains %) x))
        ))

(solve-p1)

(defn solve-p2[]
  (as-> (parse-input "data/2022/d4") x
        (count (filter #(apply overlaps %) x))
        ))

(solve-p2)