(ns aoc-clojure.2022.day06
  (:require [aoc-clojure.lib.lib :as lib]
            [clojure.string :as str]))
(use 'clojure.java.io)


(defn parse-input [file-name]
  (as-> (slurp file-name) x
        x))


(defn solve[]
  (as-> (parse-input "data/2022/d6") x
        (loop [state []
               [curr & rest] x
               counter 1]
          (if (= 14 (count state))
            (- counter 1)
            (let [idx (.indexOf state curr)]
              (if (= -1 idx)
                (recur (conj state curr) rest (inc counter))
                (recur (conj (subvec state (inc idx)) curr) rest (inc counter))
                )
              )


                                  )

          )

        )

  )

(solve)
