(ns aoc-clojure.2022.day05
  (:require [aoc-clojure.lib.lib :as lib]
            [clojure.string :as str]))
(use 'clojure.java.io)

(defn parse-step [line]
  (map #(Integer/parseInt %) (drop 1 (first (re-seq #"move (\d+) from (\d+) to (\d+)" line))))
  )

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split x #"\n\n")
        [(lib/parse-grid-2 (str/split (first x) #"\n"))
         (map parse-step (str/split (second x) #"\n"))
         ]
        x))


(defn move [stacks from to]
  (let [elem (peek (stacks from))]
    (assoc
      (assoc stacks from (pop (stacks from)))
      to
      (conj (stacks to) elem))

    )
  )

(defn move-v2 [stacks n from to]
  (let [elems (take n (stacks from))]
    (assoc
      (assoc stacks from (drop n (stacks from)))
      to
      (concat elems (stacks to)))

    )
  )

(defn apply-steps [steps stacks]
  (loop [[step & rem] steps
         state stacks]
    (if (not step)
      state
      (let [[n from to] step]
        (recur
          rem
          (reduce (fn [s _] (move s from to)) state (range n))
          )
        )
      )

    ))

(defn apply-steps-v2 [steps stacks]
  (loop [[step & rem] steps
         state stacks]
    (if (not step)
      state
      (let [[n from to] step]
        (recur
          rem
          (move-v2 state n from to)
          )
        )
      )

    ))


(defn solve []
  (as-> (parse-input "data/2022/d5") input
        (let [grid (first input)
              steps (second input)
              my (apply max (map second (keys grid)))
              mx (apply max (map first (keys grid)))
              columns (map first (filter #(not= " " (second %)) (for [x (range mx)]
                                                                  [x (grid [x my])]
                                                                  )))
              stacks (into {} (map-indexed #(vector (+ %1 1) %2)
                                           (map (fn [x]
                                                  (into '()
                                                        (reverse
                                                          (filter #(not= " " %) (map #(grid [x %]) (range my)))
                                                          )
                                                        )

                                                  ) columns)
                                           ))
              result-p1 (apply-steps steps stacks)
              result-p2 (apply-steps-v2 steps stacks)
              ]
          (println (apply str (map #(first (result-p1 %)) (sort (keys result-p1)))))
          (println (apply str (map #(first (result-p2 %)) (sort (keys result-p2)))))

          )))

(solve)
