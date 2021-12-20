(ns aoc-clojure.2021.day18
  (:require [clojure.string :as str]
            [clojure.set :as sets]
            [aoc-clojure.lib.lib :as lib]))
(use 'clojure.java.io)
(require '[clojure.data.priority-map :refer [priority-map]])

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split-lines x)
        (map #(str/replace % "," " ") x)
        (map read-string x)))

(defn leaf? [node]
  (= :leaf (:type node)))
(defn branch? [node]
  (= :branch (:type node)))
(defn l [node]
  (:left node))
(defn r [node]
  (:right node))
(defn val [leaf]
  (:value leaf))
(defn create-leaf [value]
  {:type :leaf :value value})
(defn create-branch [left right]
  {:type :branch :left left :right right})

(defn build-tree [value]
  (if (number? value)
    (create-leaf value)
    (let [[l r] value]
      (create-branch (build-tree l) (build-tree r)))))

(defn explode? [node height]
  (and (branch? node) (leaf? (l node)) (leaf? (r node)) (> height 4)))

(defn split? [leaf]
  (and (>= (val leaf) 10)))

(defn split
  ([node] (split node false))
  ([node performed]
   (if performed
     [node true]
     (if (leaf? node)
       (if (split? node)
         (let [
               ;_ (println "splitting node " node)
               v (val node)]
           [(create-branch (create-leaf (int (Math/floor (/ v 2)))) (create-leaf (int (Math/ceil (/ v 2))))) true])
         [node performed])
       (let [left (l node)
             right (r node)
             [s-left res-l] (split left performed)
             [s-right res-r] (split right res-l)]
         [(create-branch s-left s-right) res-r])))))

(defn carry-value [node value dir]
  (if (leaf? node)
    (create-leaf (+ (val node) value))
    (create-branch
      (if (= dir :left) (carry-value (l node) value dir) (l node))
      (if (= dir :right) (carry-value (r node) value dir) (r node)))))

(defn explode-tree
  ([node] (explode-tree node 1))
  ([node height]
   (if (leaf? node)
     [node nil]
     (if (explode? node height)
       ; explode node and create state
       (let [
             ;_ (println "exploding node" node)
             ]
         [(create-leaf 0) {:l (val (l node)) :r (val (r node)) :used-l false :used-r false}]
         )
       ; else try exploding branch from left
       (let [[l-res l-state] (explode-tree (l node) (inc height))]
         (if (nil? l-state)
           ; no explosion on left side, try right side
           (let [[r-res r-state] (explode-tree (r node) (inc height))]
             (if (nil? r-state)
               ; no explosion on right side, go back
               [(create-branch l-res r-res) nil]
               ; explosion on right side
               (let [{lv :l rv :r ul :used-l ur :used-r} r-state]
                 (if (not ul)
                   ; left value to be carried down
                   [(create-branch (carry-value (l node) lv :right) r-res) (assoc r-state :used-l true)]
                   ; left value already passed down, go back
                   [(create-branch l-res r-res) r-state]
                   ))))
           ; explosion on left side, try carrying right-value
           (let [{lv :l rv :r ul :used-l ur :used-r} l-state]
             (if (not ur)
               ; left value to be carried down
               [(create-branch l-res (carry-value (r node) rv :left)) (assoc l-state :used-r true)]
               ; left value already passed down, go back
               [(create-branch l-res (r node)) l-state]
               ))))))))

(defn fold-tree [node]
  (if (leaf? node)
    (val node)
    [(fold-tree (l node)) (fold-tree (r node))]))

(defn join-trees [left right]
  (create-branch left right))

(defn add-trees [t1 t2]
  (let [t (join-trees t1 t2)]
    (loop [current t]
      (let [[res state] (explode-tree current)]
        (if (nil? state)
          (let [[res state] (split current)]
            (if state
              (recur res)
              current))
          (recur res))))))

(defn magnitude [node]
  (if
    (leaf? node)
    (val node)
    (+ (* 3 (magnitude (l node))) (* 2 (magnitude (r node))))))

; p1
(time (as-> (parse-input "data/2021/d18") x
            (map build-tree x)
            (reduce add-trees x)
            (magnitude x)))

; p2
(time (as-> (parse-input "data/2021/d18") x
            (map build-tree x)
            (apply concat (map (fn [t] (apply concat (map #(vector [t %] [% t]) (filter #(not= t %) x)))) x))
            (reduce max (map (fn [[t1 t2]] (magnitude (add-trees t1 t2))) x))))