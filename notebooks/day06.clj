;# Day 6
(ns day06
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [nextjournal.clerk :as clerk]
            [advent-of-code-2024.utils :as utils]))

(def test-input 
  "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(def real-input
  (slurp (io/resource "day06.txt")))

(defn parse-map
  [input]
  (let [m (mapv vec (string/split-lines input))
        height (count m)
        width (count (first m))
        p (->> (for [y (range height)
                     x (range width)]
                 {:char (get-in m [y x]) 
                  :pos [x y]})
               (group-by :char))
        [gaurd-dir [{gaurd-pos :pos}]] (first (dissoc p \. \#))]
    {:size [width height]
     :obstacles (into #{} 
                      (map :pos) 
                      (get p \#))
     :gaurd {:dir ({\^ :north 
                    \> :east
                    \v :south
                    \< :west} gaurd-dir)
             :pos gaurd-pos}}))

(parse-map test-input)
(parse-map real-input)

(def right-turns
  {:north :east
   :east :south
   :south :west
   :west :north})

(def dirs
  {:north [0 -1]
   :east [1 0]
   :south [0 1]
   :west [-1 0]})

(defn next-state 
  [{{pos :pos dir :dir} :gaurd 
    obstacles :obstacles
    :as state}]
  (let [potential-next-pos (utils/vec+ pos (get dirs dir))
        obstacle-next? (contains? obstacles potential-next-pos)]
    (if obstacle-next?
      (update-in state [:gaurd :dir] right-turns)
      (assoc-in state [:gaurd :pos] potential-next-pos))))

(defn gone?
  [{{[x y] :pos} :gaurd
    [width height] :size 
    :as state}]
  (or (<= width x)
      (<= height y)
      (< x 0)
      (< y 0)))

(defn solve-part1
  [input]
  (->> (iterate next-state (parse-map input))
       (take-while (complement gone?))
       (map #(get-in % [:gaurd :pos]))
       (distinct)
       (count)))

(solve-part1 test-input)
(solve-part1 real-input)


