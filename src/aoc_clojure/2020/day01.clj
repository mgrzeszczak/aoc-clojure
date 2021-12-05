(ns aoc-clojure.2020.day01)
(use 'clojure.java.io)


(defn get-lines [fname]
  (with-open [r (reader fname)]
    (doall (line-seq r))))

(defn p1 []
  (let [nums (map
               (fn [x] (Integer/parseInt x))
               (get-lines "data/2020/input_d01"))]
    (map second
         (filter
           #(= (first %) 2020)
           (for [n1 nums n2 nums] (list (+ n1 n2) (* n1 n2)) )))))

(defn p2 []
  (let [nums (map
               (fn [x] (Integer/parseInt x))
               (get-lines "data/2020/input_d01"))]
    (map second
         (filter
           #(= (first %) 2020)
           (for [n1 nums n2 nums n3 nums] (list (+ n1 n2 n3) (* n1 n2 n3)) )))))
(p1)
(p2)