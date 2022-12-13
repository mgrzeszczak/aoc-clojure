(ns aoc-clojure.2022.day11
  (:require [clojure.string :as str]
            [aoc-clojure.lib.lib :as lib]))
(use 'clojure.java.io)


(defn parse-monkey [idx data]
  (let [[_ items operation test if-true if-false] (str/split-lines data)]
    {
     :id          idx
     :items       (vec (map #(Integer/parseInt %) (str/split (str/replace items #"\s+Starting items: " "") #",\s+")))
     :op          (str "(" (str/replace operation "  Operation: new = old " "") " old)")
     :test        (Integer/parseInt (str/replace test "  Test: divisible by " ""))
     :if-true     (Integer/parseInt (second (first (re-seq #"throw to monkey (\d+)" if-true))))
     :if-false    (Integer/parseInt (second (first (re-seq #"throw to monkey (\d+)" if-false))))
     :inspections 0
     }
    )
  )



(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split x #"\n\n")
        (map-indexed parse-monkey x)
        (into {} (map #(vector (:id %) %) x))
        x))


(defn merge-data [data new-items]
  (into {} (map
             (fn [id]
               (let [current (data id)
                     to-add (new-items id)]
                 [id (assoc current :items (vec (concat (:items current) to-add)))]
                 )
               )
             (keys data)))
  )

(defn turn
  [idx data div-value]
  (let [{items       :items
         op          :op
         test        :test
         if-true     :if-true
         if-false    :if-false
         inspections :inspections} (data idx)]
    (loop [[curr & rem] items
           to-add (into {} (map #(vector % []) (keys data)))
           ]
      (if (not curr)
        (assoc (merge-data data to-add)
          idx
          (assoc (assoc (data idx) :items [])
            :inspections
            (+ (count items) inspections))
          )
        (let [worry-level (if (= 3 div-value) (quot (eval (read-string (str/replace op "old" (str curr)))) div-value) (mod (eval (read-string (str/replace op "old" (str curr)))) div-value))
              divisible (= 0 (mod worry-level test))
              receiver (if divisible
                         if-true
                         if-false
                         )]

          (recur rem (assoc to-add receiver (conj (to-add receiver)
                                                  worry-level
                                                  )))

          )
        )

      )
    )

  )

(defn round
  ([rid data div-value]
   (if (= 0 (mod rid 10))
     (println rid)
     )

   (loop [[curr & rem] (range (inc (apply max (keys data))))
          state data]
     (if (not curr)
       state
       (recur rem (turn curr state div-value))
       )
     )
   ))

(defn solve-p1 [rounds]
  (as-> (parse-input "data/2022/d11") x
        (reduce (fn [data rid] (round rid data 3)) x (range rounds))
        (reduce * (take-last 2 (sort (map :inspections (vals x)))))
        ))


(defn solve-p2 [rounds]
  (as-> (parse-input "data/2022/d11") x
        (let [div-value (reduce * (map :test (vals x)))
              results (reduce (fn [data rid] (round rid data div-value)) x (range rounds))
              ]
          results
          (reduce * (map bigint (take-last 2 (sort (map :inspections (vals results))))))
          )

        ))


(solve-p1 20)
(solve-p2 10000)