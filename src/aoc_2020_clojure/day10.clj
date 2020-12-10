(ns aoc-2020-clojure.day10
  (:require [clojure.set :as set])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

; part 1
(time (as-> (slurp "data/input_d10") x
            (str/split-lines x)
            (map #(Long/parseLong %) x)
            (conj x 0)
            (sort x)
            (map vector x (concat (rest x) [(+ (apply max x) 3)]))
            (map #(- (second %) (first %)) x)
            (* (count (filter #(= % 3) x)) (count (filter #(= % 1) x)))))

; part 2
(defn solve-p2-old [data]
  (let [fn (fn [self pos skipped data]
             (if (>= pos (count data))
               1
               (let [prev (data (dec pos))
                     curr (data pos)
                     diff (- curr prev)
                     newSkipped (+ skipped diff)]
                 (cond
                   (< newSkipped 3) (+
                                      (self self (inc pos) 0 data)
                                      (self self (inc pos) newSkipped data)
                                      )
                   (= newSkipped 3) (self self (inc pos) 0 data)
                   (> newSkipped 3) 0))))
        memfn (memoize fn)]
    (memfn memfn 1 0 data)))

(defn solve-p2
  ([data paths index] (let [v (data index)
                            r (range 3)]
                        (assoc paths v (apply + (map #(paths (- v (inc %)) 0) r))))))

(time (as-> (slurp "data/input_d10") x
            (str/split-lines x)
            (map #(Long/parseLong %) x)
            (conj x 0)
            (conj x (+ (apply max x) 3))
            (sort x)
            (vec x)
            ((reduce
               (partial solve-p2 x) {0 1} (drop 1 (range (count x)))) (last x))))



