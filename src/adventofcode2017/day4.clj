(ns adventofcode2017.day4
  (:require [clojure.math.combinatorics :as combo]))

(def input (as-> "resources/day4.input" x
                 (slurp x)
                 (clojure.string/split x #"\n")
                 (map #(clojure.string/split % #"\s") x)))

(defn part1
  [input]
  (reduce
    (fn [acc passphrase]
      (if (= passphrase (distinct passphrase))
        (inc acc)
        acc))
    0
    input))

(defn part2
  [input]
  (reduce
    (fn [acc passphrase]
      (let [possible-perms (map
                             (partial apply str)
                             (mapcat
                               #(clojure.set/difference (set (combo/permutations %)) #{(seq %)})
                               passphrase))]
        (if (and (= passphrase (distinct passphrase))
                 (empty? (clojure.set/intersection
                           (set possible-perms)
                           (set passphrase))))
          (inc acc)
          acc)))
    0
    input))