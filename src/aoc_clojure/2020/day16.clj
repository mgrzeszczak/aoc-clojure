(ns aoc-clojure.2020.day16
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn create-rule-fn [threshold-strs]
  (let [thresholds (map #(Integer/parseInt %) threshold-strs)
        x1 (nth thresholds 0)
        x2 (nth thresholds 1)
        x3 (nth thresholds 2)
        x4 (nth thresholds 3)]
    #(or (and (>= % x1) (<= % x2)) (and (>= % x3) (<= % x4)))))

(defn parse-rules [rules]
  (as-> (map #(re-matches #"([\s\w]+): (\d+)-(\d+) or (\d+)-(\d+)" %) rules) x
        (map #(list (second %) (drop 2 %)) x)
        (map #(hash-map :name (first %) :fn (create-rule-fn (second %))) x)
        ))

(defn parse-ticket [ticket]
  (map #(Integer/parseInt %) (str/split ticket #",")))

(defn parse-input [file]
  (as-> (slurp file) x
        (str/split x #"\n\n")
        (map #(str/split % #"\n") x)
        (let [rules (parse-rules (first x))
              any-rule (fn [val] (let [fns (map :fn rules)]
                                   (some identity (map #(% val) fns))))]
          {:rules          rules
           :my-ticket      (parse-ticket (second (second x)))
           :nearby-tickets (map parse-ticket (rest (nth x 2)))
           :any-rule       any-rule
           })))

; part 1
(time (as-> (parse-input "data/2020/input_d16") x
            (reduce + (filter (comp not (:any-rule x)) (flatten (:nearby-tickets x))))))

; part 2
(defn part2 [valid-tickets rules]
  (let [rule-count (count rules)]
    (loop [found-matches {}
           found-indices #{}
           remaining-rules rules]
      (println (count found-matches))
      (if (= rule-count (count found-matches))
        found-matches
        (let [ticket-length (count (first valid-tickets))
              rule-matches (for [i (filter #(nil? (found-indices %)) (range ticket-length))]
                             (let [vals (map #(nth % i) valid-tickets)
                                   matching-rules (filter #(every? (:fn %) vals) remaining-rules)]
                               {:index i :matching-rules matching-rules}))
              valid (map #(hash-map :index (:index %) :rule (:name (first (:matching-rules %)))) (filter #(= (count (:matching-rules %)) 1) rule-matches))
              new-found-matches (reduce #(assoc %1 (:rule %2) (:index %2)) found-matches valid)
              new-found-indices (set/union (set (map :index valid)) found-indices)
              matched-rule-names (set (keys new-found-matches))
              new-remaining-rules (filter #(nil? (matched-rule-names (:name %))) remaining-rules)]
          (recur new-found-matches new-found-indices new-remaining-rules))))))

(time (as-> (parse-input "data/2020/input_d16") x
            (assoc x :valid-tickets (filter #(every? (:any-rule x) %) (:nearby-tickets x)))
            (let [mappings (part2 (:valid-tickets x) (:rules x))
                  departure-indices (map second (filter #(str/starts-with? (first %) "departure") mappings))
                  my-ticket (vec (:my-ticket x))]
              (reduce * (map #(my-ticket %) departure-indices)))))
