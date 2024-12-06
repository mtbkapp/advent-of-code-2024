;# Day 4
(ns day04
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [nextjournal.clerk :as clerk]))

(def input
  (slurp (io/resource "day04.txt")))

(def word-search
  (mapv vec (string/split-lines input)))

(def width
  (count (first word-search)))

(def height
  (count word-search))

(defn get-char
  [[x y]]
  (get-in word-search [y x]))

; ## Part 1

(def word
  (seq "XMAS"))

(defn word-for-path
  [path]
  (map get-char path))

(word-for-path [[7 0] [6 0] [5 0] [4 0]])
(= word (word-for-path [[7 0] [6 0] [5 0] [4 0]]))

(defn vec+
  [[ax ay] [bx by]]
  [(+ ax bx) (+ ay by)])

(defn paths-from
  [start]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not (and (zero? dx)
                        (zero? dy)))]
    (->> (iterate #(vec+ % [dx dy]) start)
         (take 4))))

(def part1
  (->> (for [y (range height)
             x (range width)]
         [x y])
       (reduce (fn [word-count position]
                 (->> (paths-from position)
                      (map word-for-path)
                      (filter #(= word %))
                      (count)
                      (+ word-count)))
               0)))

; ## Part 2
(def x-shape-relative-positions 
  [[-1 -1] [1 -1]
   [0 0]
   [-1 1]  [1 1]])

; All possible X's.
; ```
; | M M | M S | S M | S S |
; |  A  |  A  |  A  |  A  |
; | S S | M S | S M | M M |
; ```
;
(def x-shapes
  #{[\M \M \A \S \S]
    [\M \S \A \M \S]
    [\S \M \A \S \M]
    [\S \S \A \M \M]})

(defn x-at?
  [position]
  (contains? x-shapes
             (map (comp get-char #(vec+ position %)) 
                  x-shape-relative-positions)))

(def part2
  (->> (for [y (range height)
             x (range width)]
         [x y])
       (reduce (fn [x-count position]
                 (if (x-at? position)
                   (inc x-count)
                   x-count))
               0)))
