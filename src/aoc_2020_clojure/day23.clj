(ns aoc-2020-clojure.day23
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-input [value]
  (as-> (str/split value #"") x
        (map #(Integer/parseInt %) x)
        (vec x)))

(defn choose-dest [current next-set N]
  (loop [c current]
    (let [n (dec c)
          nc (case n 0 N n)]
      (if (next-set nc) (recur nc) nc))))

(defn solve [cups moves]
  (let [N (count cups)]
    (loop [current (first cups)
           move 1
           ordering (into {} (map #(vector (cups %) (cups (mod (inc %) N))) (range 0 N)))]
      (if (= 0 (mod move 10000)) (println move))
      (if (> move moves)
        ordering
        (let [n1 (ordering current)
              n2 (ordering n1)
              n3 (ordering n2)
              n4 (ordering n3)
              next-set (into #{} [n1 n2 n3])
              dest (choose-dest current next-set N)
              after-dest (ordering dest)
              new-order (reduce #(assoc %1 (first %2) (second %2)) ordering [[current n4] [dest n1] [n3 after-dest]])]
          (recur (new-order current) (inc move) new-order))))))

(defn fill [arr how-many]
  (let [c (count arr)
        start (inc (reduce max arr))]
    (vec (concat arr (range start (+ start (- how-many c)))))))

(time (as-> (parse-input "974618352") x
            (solve x 100)
            (apply str (drop 1 (take 9 (iterate #(x %) 1))))))

(time (as-> (parse-input "974618352") x
            (fill x 1000000)
            (solve x 10000000)
            (* (x 1) (x (x 1)))))
