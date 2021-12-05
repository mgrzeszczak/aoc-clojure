(ns aoc-clojure.2020.day19
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-rule [rule]
  (let [m (re-matcher #"(\d+|\||\"\w+\")" rule)]
    (loop [parsed '()]
      (let [val (re-find m)]
        (if val
          (recur (conj parsed (str/replace (second val) "\"" "")))
          (let [reversed (reverse parsed)
                rule-no (first reversed)
                rem (rest reversed)]
            (loop [remaining rem
                   groups '()]
              (if (empty? remaining)
                {:no rule-no :groups (reverse (map vec groups))}
                (let [[a b] (split-with #(not= % "|") remaining)]
                  (recur (rest b) (conj groups a)))))))))))

(defn parse-rules [rules]
  (into {} (map #(vector (:no %) (:groups %)) (map parse-rule rules))))

(defn parse-input [name]
  (as-> (slurp name) x
        (str/split x #"\n\n")
        (map #(str/split % #"\n") x)
        {:messages (second x) :rules (parse-rules (first x))}
        ))

; part 1

(defn map-entry[e rules fn]
  (if (re-matches #"\d+" e)
    (fn (rules e) rules) e)
  )

(defn map-entry-p2[e rules fn]
  (if (#{"8" "11"} e)
    (let [s (case e
              "8" (str "(" (fn (rules "42") rules) ")" "+")
              "11"
              (str "(" (str/join "|" (let [r42 (str "(" (fn (rules "42") rules) ")")
                    r31 (str "(" (fn (rules "31") rules) ")")]
                (map #(str r42 "{"  % "}" r31 "{" % "}") (range 1 100))
                )) ")"))]
      s)

    (if (re-matches #"\d+" e)
      (fn (rules e) rules) e)
    )
  )

(defn build-regular-exp
  ([rules] (build-regular-exp (rules "0") rules))
  ([current rules]
   (let [res (str/join "|"
                       (map
                         (fn [group]
                           (apply str (map #(map-entry % rules build-regular-exp) group)))
                         current))]
     (if (re-matches #"\w+" res)
       res
       (str "(" res ")")
       )
     )
   ))

(defn build-regular-exp-p2
  ([rules] (build-regular-exp-p2 (rules "0") rules))
  ([current rules]
   (let [res (str/join "|"
                       (map
                         (fn [group]
                           (apply str (map #(map-entry-p2 % rules build-regular-exp-p2) group)))
                         current))]
     (if (re-matches #"\w+" res)
       res
       (str "(" res ")")
       )
     )
   ))

(time (as-> (parse-input "data/2020/input_d19") x
            {
             :pattern  (re-pattern (build-regular-exp (:rules x)))
             :messages (:messages x)
             }
            (count (filter #(re-matches (:pattern x) %) (:messages x)))))

; part 2
(time (as-> (parse-input "data/2020/input_d19") x
            {
             :pattern  (re-pattern (build-regular-exp-p2 (:rules x)))
             :messages (:messages x)
             }
            (count (filter #(re-matches (:pattern x) %) (:messages x)))
            ))