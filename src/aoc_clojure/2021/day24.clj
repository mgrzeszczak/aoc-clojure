(ns aoc-clojure.2021.day23
  (:require [clojure.string :as str]
            [clojure.set :as sets]
            [aoc-clojure.lib.lib :as lib]))
(use 'clojure.java.io)
(require '[clojure.data.priority-map :refer [priority-map]])

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        ))

(time (as-> (parse-input "data/2021/d23") x
            ))