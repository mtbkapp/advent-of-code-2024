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
    {:map {:size [width height]
           :obstacles (into #{} 
                            (map :pos) 
                            (get p \#))}
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

(defn sim
  [{[width height] :size obstacles :obstacles} start-guard]
  (loop [{dir :dir [x y :as pos] :pos :as gaurd} start-guard
         path #{}]
    (cond (contains? path gaurd) {:cause :cycle :path path}
          (or (<= width x)
              (<= height y)
              (< x 0 )
              (< y 0)) {:cause :gone :path path}
          :else
          (let [next-pos (utils/vec+ pos (get dirs dir))]
            (recur (if (contains? obstacles next-pos)
                     (update gaurd :dir right-turns)
                     (assoc gaurd :pos next-pos)) 
                   (conj path gaurd))))))

(defn solve-part1
  [{gaurd-init :gaurd the-map :map}]
  (->> (sim the-map gaurd-init)
       :path
       (into #{} (map :pos))))

(defn solve-part2
  [{gaurd-init :gaurd the-map :map} possible-new-obstacles]
  (->> possible-new-obstacles
       (map (fn [new-obstacle]
              (-> (sim (update the-map :obstacles conj new-obstacle) 
                       gaurd-init)
                  (:cause))))
       (frequencies)))

(defn solve-both
  [input]
  (let [parsed-input (parse-map input)
        part1-positions (solve-part1 parsed-input)]
    {:part1 (count part1-positions) 
     :part2 (:cycle (solve-part2 parsed-input part1-positions))}))

(solve-both test-input)
(solve-both real-input)

