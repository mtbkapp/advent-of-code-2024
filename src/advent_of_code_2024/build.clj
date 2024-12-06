(ns advent-of-code-2024.build
  (:require [nextjournal.clerk :as clerk]))

(defn -main
  [& args]
  (clerk/build! {:paths ["notebooks/*"]}))
