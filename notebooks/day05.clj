;# Day 5
(ns day05
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [nextjournal.clerk :as clerk]))

(def test-input
  "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(defn parse-part1
  [part1]
  (transduce (map (fn [line]
                    (mapv parse-long (string/split line #"\|"))))
             (completing
               (fn [before [before-page after-page]]
                 (update before before-page (fnil conj #{}) after-page)))
             {}
             (string/split-lines part1)))

(defn parse-part2
  [part2]
  (map (fn [line]
         (mapv parse-long (string/split line #",")))
       (string/split-lines part2)))

(defn parse-input
  [input]
  (let [[part1 part2] (string/split input #"\n\n")]
    [(parse-part1 part1)
     (parse-part2 part2)]))

(parse-input test-input)

(defn correct-order? 
  [rules pages]
  (->> (map (fn [i]
              (let [before (subvec pages 0 i)
                    page (nth pages i)
                    r (get rules page)]
                (every? #(not (contains? r %)) before)))
            (range 1 (count pages)))
       (every? true?)))

(defn get-middle
  [pages]
  (nth pages (quot (count pages) 2)))

(defn solve-part1
  [input]
  (let [[rules runs] (parse-input input)]
    (transduce (comp (filter (partial correct-order? rules))
                     (map get-middle))
               +
               runs)))

(solve-part1 test-input)

(def real-input
  (slurp (io/resource "day05.txt")))

(solve-part1 real-input)


(defn solve-part2
  [input]
  (let [[rules runs] (parse-input input)
        bad-runs (remove (partial correct-order? rules) runs)
        rule-comparator (comparator 
                          (fn [x y]
                            (contains? (get rules x) y)))]
    (reduce + (map (comp get-middle #(sort rule-comparator %)) bad-runs))))

(solve-part2 test-input)
(solve-part2 real-input)
