(ns aoc-clojure.2022.day21
  (:require [aoc-clojure.lib.lib :as lib]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.math :as math]))
(use 'clojure.java.io)

(defn is-number[s]
  (re-matches #"\d+" s))

(defn parse-form[form]
  (if (is-number form)
    [:value (Long/parseLong form)]
    (let [[left operator right] (drop 1 (first (re-seq #"(\w+) ([+-/*]) (\w+)" form)))
          left-parsed (if (is-number left) (Long/parseLong left) left)
          right-parsed (if (is-number right) (Long/parseLong right) right)]
      [:op [left-parsed
            ;(eval (read-string operator))
            operator
            right-parsed]]
      )
    )
  )

(defn parse-line[line]
  (let [[name form] (str/split line #": ")]
    [name (parse-form form)]
    ))

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split-lines x)
        (map parse-line x)
        (into {} x)
        ))


(defn eval-form[form forms]
  (if (forms form)
    (eval-form (forms form) forms)
    (let [[type val] form]
      (case type
        :value val
        :op (let [[l op r] val]
              ((eval (read-string op)) (eval-form l forms) (eval-form r forms))
              ))
      )
    )
    )

(defn expand[form forms]
  (if (= form "humn")
    "humn"
    (if (forms form)
      (expand (forms form) forms)
      (let [[type val] form]
        (case type
          :value val
          :op (let [[l op r] val]
                (list op (expand l forms) (expand r forms))
                ))
        )
      )
    )

  )


(defn binary-search[from to target test-fn]
  (let [mid (Math/floor (/ (+ from to) 2))
        val (test-fn mid)]
    (if (> from to)
      "not found"
      (if (= val target)
        from
        (if (< val target)
          (binary-search from (dec mid) target test-fn)
          (binary-search (inc mid) to target test-fn)
          )
        )

                    )

    )
  )

(defn p2[data]
  (let [root (data "root")
        [_ [l _ r]] root
        test-fn #(eval-form l (assoc data "humn" [:value %]))
        ]
    (binary-search 0 10000000000000 (double (eval-form r data)) test-fn)
    ))

(defn p1[data]
  (eval-form "root" data)
  )

(defn solve []
  (let [data (parse-input "data/2022/d21")]
    [(p1 data) (p2 data)]


    )
  )

(time (solve))