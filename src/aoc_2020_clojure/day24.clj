(ns aoc-2020-clojure.day22
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-line [line]
  line)

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split-lines x)
        (map parse-line x)))

(time (as-> (parse-input "data/input_d22") x))
