(ns aoc-clojure.2021.day16
  (:require [clojure.string :as str]
            [clojure.set :as sets]
            [aoc-clojure.lib.lib :as lib]))
(use 'clojure.java.io)
(require '[clojure.data.priority-map :refer [priority-map]])

(def hex-map {
              "0" "0000"
              "1" "0001"
              "2" "0010"
              "3" "0011"
              "4" "0100"
              "5" "0101"
              "6" "0110"
              "7" "0111"
              "8" "1000"
              "9" "1001"
              "A" "1010"
              "B" "1011"
              "C" "1100"
              "D" "1101"
              "E" "1110"
              "F" "1111"
              })

(defn parse-hex [input]
  (apply concat (map hex-map input)))

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split x #"")
        (parse-hex x)))

(defn to-int [bits]
  (Long/parseLong bits 2))

(defn parse-value [bits]
  (loop [rem bits
         result []]
    (let [pack (take 5 rem)]
      (if (= \0 (first pack))
        [(to-int (apply str (apply concat (conj result (drop 1 pack))))) (drop 5 rem)]
        (recur (drop 5 rem) (conj result (drop 1 pack)))))))

(defn parse-sub-packets-0 [bits bit-count parse-packet-fn]
  (loop [packets []
         data bits]
    (if (= bit-count (- (count bits) (count data)))
      [packets data]
      (let [[packet new-bits] (parse-packet-fn data)]
        (recur (conj packets packet) new-bits)))))

(defn parse-sub-packets-1 [bits n parse-packet-fn]
  (loop [packets []
         data bits]
    (if (= n (count packets))
      [packets data]
      (let [[packet new-bits] (parse-packet-fn data)]
        (recur (conj packets packet) new-bits)))))

(defn parse-operator [bits version type-id parse-packet-fn]
  (let [len-type (first bits)
        rem-bits (drop 1 bits)]
    (if (= \0 len-type)
      (let [len (to-int (apply str (take 15 rem-bits)))
            [sub-packets data] (parse-sub-packets-0 (drop 15 rem-bits) len parse-packet-fn)]
        [[version type-id sub-packets] data]
        )
      (let [n (to-int (apply str (take 11 rem-bits)))
            [sub-packets data] (parse-sub-packets-1 (drop 11 rem-bits) n parse-packet-fn)]
        [[version type-id sub-packets] data]))))

(defn parse-packet [bits]
  (let [version (to-int (apply str (take 3 bits)))
        type-id (to-int (apply str (take 3 (drop 3 bits))))]
    (if (= 4 type-id)
      (let [[value rem-bits] (parse-value (drop 6 bits))]
        [[version type-id value] rem-bits])
      (parse-operator (drop 6 bits) version type-id parse-packet))))

(defn calculate-p1 [[version type-id data]]
  (if (= 4 type-id)
    version
    (+ version (reduce + (map calculate-p1 data)))))

(defn eval-packet [[version type-id data]]
  (if (= 4 type-id)
    data
    (let [sub-data (map eval-packet data)
          bool-to-int #(if % 1 0)]
      (case type-id
        0 (reduce + sub-data)
        1 (reduce * sub-data)
        2 (apply min sub-data)
        3 (apply max sub-data)
        5 (bool-to-int (> (first sub-data) (second sub-data)))
        6 (bool-to-int (< (first sub-data) (second sub-data)))
        7 (bool-to-int (= (first sub-data) (second sub-data)))))))

(time (as-> (parse-input "data/2021/d16") x
            (first (parse-packet x))
            [(calculate-p1 x) (eval-packet x)]))