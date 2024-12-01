;# Day 1
(ns day01
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def data
  (let [by-line (->> (slurp (io/resource "day01.txt"))
                     (string/split-lines)
                     (map (fn [line]
                            (map parse-long (string/split line #"\s+" )))))]
    {:left (map first data)
     :right (map second data)}))

(def part1
  (->> (map (comp abs -)
            (sort (:left data))
            (sort (:right data)))
       (reduce +)))

(def part2
  (let [freqs (frequencies (:right data))]
    (transduce (map #(* % (get freqs % 0)))
               +
               0
               (:left data))))
