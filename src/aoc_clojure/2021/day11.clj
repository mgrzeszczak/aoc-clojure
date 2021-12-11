(ns aoc-clojure.2021.day11
  (:require [clojure.string :as str]
            [clojure.set :as sets]
            [aoc-clojure.lib.lib :as lib]))
(use 'clojure.java.io)

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split-lines x)
        (lib/parse-grid x #(Integer/parseInt %))))

(defn flash-loop [grid]
  (loop [state grid
         c 0
         flashed #{}
         queue (into #{} (map first (filter #(> (second %) 9) grid)))]
    (if (empty? queue)
      [state c]
      (let [[p & rest] queue
            ns (filter #(and (not (flashed %)) (state %)) (lib/grid-neighbors-8 p))
            about-to-flash (into #{} (filter #(and (>= (state %) 9) (not (queue %))) ns))]
        (recur
          (reduce (fn [state x] (assoc state x (inc (state x)))) (assoc state p 0) ns)
          (inc c)
          (conj flashed p)
          (sets/union (into #{} rest) about-to-flash))))))

(defn simulate [grid steps]
  (loop [[s & rest] (range steps)
         state grid
         flash-count 0]
    (if (not s)
      flash-count
      (let [incremented (into {} (map (fn [[p v]] [p (+ v 1)]) state))
            [new-state flash-inc] (flash-loop incremented)]
        (recur rest new-state (+ flash-count flash-inc))))))

(defn simulate-p2 [grid steps]
  (loop [[s & rest] (range steps)
         state grid
         flash-count 0]
    (if (every? #(= 0 (second %)) state)
      s
      (if (not s)
        flash-count
        (let [incremented (into {} (map (fn [[p v]] [p (+ v 1)]) state))
              [new-state flash-inc] (flash-loop incremented)]
          (recur rest new-state (+ flash-count flash-inc)))))))

; p1
(time (as-> (parse-input "data/2021/d11") x
            (simulate x 100)))
; p2
(time (as-> (parse-input "data/2021/d11") x
            (simulate-p2 x 1000)))