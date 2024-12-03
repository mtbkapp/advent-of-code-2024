;# Day 3
(ns day03
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [nextjournal.clerk :as clerk]))

(def input
  (slurp (io/resource "day03.txt")))

(def part1
  (transduce (map (fn [[_ a b :as match]]
                    (* (parse-long a)
                       (parse-long b))))
             +
             0
             (re-seq #"mul\((\d{1,3}),(\d{1,3})\)" input)))

(->> (re-seq #"(mul|do|don\'t)\(((\d+{1,3}),(\d{1,3}))?\)" input)
     (take 100)
     (clerk/table))

(def part2
  (reduce (fn [state [_ instr _ arg0 arg1]]
            (case instr
              "do" (assoc state :enabled? true)
              "don't" (assoc state :enabled? false)
              "mul" (if (:enabled? state)
                      (update state
                              :sum
                              +
                              (* (parse-long arg0)
                                 (parse-long arg1)))
                      state)))
          {:sum 0 :enabled? true}
          (re-seq #"(mul|do|don\'t)\(((\d+{1,3}),(\d{1,3}))?\)" input)))
