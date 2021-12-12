(ns aoc-clojure.2021.day12
  (:require [clojure.string :as str]
            [clojure.set :as sets]
            [aoc-clojure.lib.lib :as lib]))
(use 'clojure.java.io)

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split-lines x)
        (map #(str/split % #"-") x)
        (concat x (map reverse x))
        (group-by first x)
        (into {} (map (fn [[k v]] (vector k (into #{} (map second v)))) x))))

(defn can-revisit [node]
  (re-matches #"[A-Z]+" node))

(defn is-small-cave [node]
  (and (re-matches #"[a-z]+" node)
       (not= node "start") (not= node "end")))

(defn find-paths [start end connections can-revisit-small-caves]
  (loop [queue (list [start #{start} can-revisit-small-caves])
         completed 0]
    (let [[h & tail] queue]
      (if (not h)
        completed
        (let [[node visited can-revisit-small-cave] h]
          (if (= node end)
            (recur tail (inc completed))
            (let [possible-connections (filter #(or (not (visited %)) (can-revisit %) (and (visited %) (is-small-cave %) can-revisit-small-cave)) (connections node))
                  new-states (map #(vector % (conj visited %) (and can-revisit-small-cave (not (and (is-small-cave %) (visited %))))) possible-connections)]
              (recur (concat new-states tail) completed))))))))

(time (as-> (parse-input "data/2021/d12") x
            (doall (map #(find-paths "start" "end" x %) [false true]))))