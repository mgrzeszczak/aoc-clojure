(ns aoc-clojure.2020.day21
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-line [line]
  (let [[ingredients-str allergens-str] (str/split line #"\s\(")]
    (let [ingredients (str/split ingredients-str #"\s")
          allergens (as-> allergens-str x
                          (str/replace x ")" "")
                          (str/replace x "contains " "")
                          (str/split x #", "))]
      {
       :ingredients (into #{} ingredients)
       :allergens   (into #{} allergens)
       })))

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split-lines x)
        (map parse-line x)))


(defn solve [data all-ingredients all-allergens]
  (let [bindings (map #(vector % (reduce set/intersection (map :ingredients (filter (fn [x] ((:allergens x) %)) data)))) all-allergens)
        solved (filter #(= 1 (count (second %))) bindings)
        remaining (filter #(not= 1 (count (second %))) bindings)]
    (let [solved-allergens (loop [solution (into {} (map #(vector (first %) (first (second %))) solved))
                                  found (reduce set/union (map second solved))
                                  rest remaining]
                             (if (= (count solution) (count all-allergens))
                               solution
                               (let [filtered (map #(vector (first %) (set/difference (second %) found)) rest)
                                     new-solved (filter #(= 1 (count (second %))) filtered)
                                     new-solved-allergens (into #{} (map first new-solved))
                                     new-found (reduce set/union (map second new-solved))
                                     new-rest (filter #(nil? (new-solved-allergens (first %))) filtered)]
                                 (recur (reduce #(assoc %1 (first %2) (first (second %2))) solution new-solved) new-found new-rest))))]
      {
       :part1 (reduce + (map #(count (filter (fn [x] ((:ingredients x) %)) data)) (set/difference all-ingredients (into #{} (vals solved-allergens)))))
       :part2 (str/join "," (map solved-allergens (sort (keys solved-allergens))))
       })))

(time (as-> (parse-input "data/input_d21") x
            {
             :data            x
             :all-ingredients (reduce set/union (map :ingredients x))
             :all-allergens   (reduce set/union (map :allergens x))
             }
            (solve (:data x) (:all-ingredients x) (:all-allergens x))
            ))
