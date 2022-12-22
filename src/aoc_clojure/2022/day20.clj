(ns aoc-clojure.2022.day20
  (:require [aoc-clojure.lib.lib :as lib]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))
(use 'clojure.java.io)


(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split-lines x)
        (map #(Integer/parseInt %) x)
        ))


(defn shift [arr idx]
  (let [n (count arr)
        [v c] (arr idx)
        to-shift (mod v (dec n))
        target-pos (mod (+ idx to-shift) n)]
    (if (= target-pos idx)
      (vec (concat
             (subvec arr 0 idx)
             [[v c]]
             (subvec arr (inc idx))
             ))
      (if (< target-pos idx)
        (vec (concat
               (subvec arr 0 (inc target-pos))
               [[v c]]
               (subvec arr (inc target-pos) idx)
               (subvec arr (inc idx))
               ))
        (vec (concat
               (subvec arr 0 idx)
               (subvec arr (inc idx) (inc target-pos))
               [[v c]]
               (subvec arr (inc target-pos))
               ))
        )
      )

    )
  )

(defn mix [data]
  (let [n (count data)]
    (loop [shifted 0
           array data
           idx 0]
      (if (= shifted n)
        array
        (let [[_ c] (array idx)]
          (if (not= c shifted)
            (recur shifted array (mod (inc idx) n))
            (recur (inc shifted) (shift array idx) idx)
            )
          )

        )
      )
    )
  )


(defn p1 [input]
  (let [data (vec (map-indexed #(vector %2 %1) input))
        mixed (mix data)
        zero-idx (.indexOf mixed (first (filter #(= (first %) 0) data)))]
    (reduce + (map first (map
                           #(mixed (mod (+ zero-idx %) (count mixed)))
                           [1000 2000 3000])))
    )
  )

(defn p2 [input iterations]
  (let [result (loop [data (vec (map-indexed #(vector %2 %1) input))
                      iteration 1]
                 (if (> iteration iterations)
                   data
                   (recur (mix data) (inc iteration))
                   )
                 )
        zero-idx (.indexOf result (first (filter #(= (first %) 0) result)))]
    (reduce + (map first (map
                           #(result (mod (+ zero-idx %) (count result)))
                           [1000 2000 3000])))))

(def decryption-key 811589153)

(defn solve []
  (let [data (parse-input "data/2022/d20")]
    [
     (p1 data)
     (p2 (vec (map #(* % decryption-key) data)) 10)
     ]
    )
  )

(time (solve))