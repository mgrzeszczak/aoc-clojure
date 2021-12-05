(ns aoc-clojure.2020.day14
  (:require [clojure.string :as str]))

(defn parse-data[input]
  (as-> (str/split-lines input) x

        (map #(or (re-matches #"mask = ([01X]+)" %) (re-matches #"mem\[(\d+)\] = (\d+)" %)) x)
        (map #(if (= 2 (count %))
                {:type :mask :value (vec (str/split (second %) #"")) }
                {:type :mem :key (Long/parseLong (nth % 1)) :value (Long/parseLong (nth % 2))}) x)))




; part 1
(defn apply-mask[mask value]
  (let [bs (vec (str/split (Long/toBinaryString value) #""))
        padded (concat (vec (repeat (- (count mask) (count bs)) 0)) bs)
        nv (apply str (map-indexed #(if (not= "X" (mask %1)) (mask %1) %2) padded))]
    (Long/parseLong nv 2)
    )
  )

(defn solve-p1[input]
  (loop [[instr & rest] input
         memory {}
         mask nil]
    (if (nil? instr)
      (reduce + (map second memory))
      (cond
        (= (:type instr) :mask) (recur rest memory (:value instr))
        :else (recur rest (assoc memory (:key instr) (apply-mask mask (:value instr))) mask))


      )
    )
  )

(time (as-> (slurp "data/input_d14") x
            (parse-data x)
            (solve-p1 x)
            ))

; part 2
(defn generate-addresses[[val & rest] current]
  (if (nil? val)
    [(Long/parseLong (apply str current) 2)]
    (case val
      "X" (concat (generate-addresses rest (conj current "1")) (generate-addresses rest (conj current "0")))
      (generate-addresses rest (conj current val)))
    )
  )

(defn apply-mask-v2[mask value]
  (let [bs (vec (str/split (Long/toBinaryString value) #""))
        padded (concat (vec (repeat (- (count mask) (count bs)) 0)) bs)
        nv (map-indexed #(case (mask %1) "X" "X" "1" "1" "0" %2) padded)]
    (generate-addresses nv [])
    )
  )



(defn solve-p2[input]
  (loop [[instr & rest] input
         memory {}
         mask nil]
    (if (nil? instr)
      (reduce + (map second memory))
      (cond
        (= (:type instr) :mask) (recur rest memory (:value instr))
        :else (recur rest
                     (reduce #(assoc %1 %2 (:value instr)) memory (apply-mask-v2 mask (:key instr)))
                     mask))


      )
    )
  )
(time (as-> (slurp "data/input_d14") x
            (parse-data x)
            (solve-p2 x)
            ))

