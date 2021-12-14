(ns aoc-clojure.2021.day14
  (:require [clojure.string :as str]
            [clojure.set :as sets]
            [aoc-clojure.lib.lib :as lib]))
(use 'clojure.java.io)

(defn parse-rule [rule]
  (vec (drop 1 (first (re-seq #"([A-Z]{2})\s+\-\>\s+([A-Z])" rule)))))

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split-lines x)
        (split-with #(not= "" %) x)
        [(ffirst x) (into {} (map parse-rule (drop 1 (second x))))]
        ))

(defn adjacent-pairs [template]
  (let [chars (str/split template #"")]
    (map str (take (- (count chars) 1) chars) (drop 1 chars))))

(defn step [pairs rules]
  (map
    #(vector (first %) (reduce + (map second (second %))))
    (group-by first (apply concat (map (fn [[pair count]]
                                         (let [[l r] pair
                                               rule (rules pair)]
                                           (if rule [[(str l rule) count] [(str rule r) count]] [[pair count]])
                                           )
                                         ) pairs)))))

(defn steps [template rules n]
  (let [pairs (map #(vector (first %) (count (second %))) (group-by identity (adjacent-pairs template)))]
    (reduce (fn [tmpl _] (step tmpl rules)) pairs (range n))))

(defn solve [template rules n]
  (let [pairs (steps template rules n)
        counts (into {} (map #(vector (first %) (reduce + (map second (second %)))) (group-by first (map #(vector (ffirst %) (second %)) pairs))))
        c (vals (assoc counts (last template) (inc (second (first (filter #(= (first %) (last template)) counts))))))]
    (- (apply max c) (apply min c))))

(time (as-> (parse-input "data/2021/d14") x
            (let [template (first x)
                  rules (second x)]
              [(solve template rules 10) (solve template rules 40)])))