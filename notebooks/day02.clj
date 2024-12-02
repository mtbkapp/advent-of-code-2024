;# Day 2
(ns day02
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

; ## Read the data

(def data
  (->> (slurp (io/resource "day02.txt"))
       (string/split-lines)
       (map (comp #(mapv parse-long %)
                  #(string/split % #"\s+")))))

; Are all the input sequences the same length?
(frequencies (map count data))

; ## Part 1

(defn safe?
  [xs]
  (and (or (apply < xs)
           (apply > xs))
       (->> (partition 2 1 xs)
            (map (fn [[x y]]
                   (abs (- x y))))
            (every? (fn [diff]
                      (<= 1 diff 3))))))

(frequencies (map safe? data))

; ## Part 2
; There might be a faster / smarter way of doing this with the slopes between 
; the data points or something but brute force is fast enough for the input 
; data.

; I'm not sure why there isn't a way in Clojure to remove a single item from 
; anywhere in a vector.
(defn remove-nth
  [xs n]
  (into (subvec xs 0 n)
        (subvec xs (inc n) (count xs))))

(defn part2-safe?
  [xs]
  (or (safe? xs)
      (some (fn [n]
              (safe? (remove-nth xs n)))
            (range (count xs)))))

(frequencies (map part2-safe? data))

