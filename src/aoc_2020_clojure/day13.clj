(ns aoc-2020-clojure.day13
  (:require [clojure.string :as str]))

; part 1
(time (as-> (slurp "data/input_d13") x
            (str/split-lines x)
            (list (Integer/parseInt (first x)) (map #(Integer/parseInt %) (map first (re-seq #"(\d+)" (second x)))))
            (map #(hash-map :rem (- % (mod (first x) %)) :mod (mod (first x) %) :bus %) (second x))
            (apply min-key :rem x)
            (* (:rem x) (:bus x))))

; part2
(defn solve-p2 [input]
  (loop [[bus next :as all] input
         current 0
         currentStep (second bus)]
    (if (nil? next)
      current
      (let [[offset step] next]
        (if (= 0 (mod (+ current offset) step))
          (recur (drop 1 all) current (* step currentStep))
          (recur all (+ current currentStep) currentStep))))))

(time (as-> (slurp "data/input_d13") x
            (str/split-lines x)
            (filter #(not= (second %) "x") (map-indexed #(vector %1 %2) (str/split (second x) #",")))
            (map #(vector (first %) (Integer/parseInt (second %))) x)
            (solve-p2 x)))

