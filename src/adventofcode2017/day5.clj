(ns adventofcode2017.day5)

(def input (as-> "resources/day5.input" x
                 (slurp x)
                 (clojure.string/split x #"\n")
                 (mapv read-string x)))

(defn part1
  [input]
  (loop [instructions input
         at           0
         n            0]
    (if (or
          (< at 0)
          (>= at (count instructions)))
      n
      (let [this-instruction (nth instructions at)]
        (recur (update-in instructions [at] inc) (+ at this-instruction) (inc n))))))

(defn part2
  [input]
  (loop [instructions input
         at           0
         n            0]
    (if (or
          (< at 0)
          (>= at (count instructions)))
      n
      (let [this-instruction (nth instructions at)]
        (recur (update-in instructions [at] (if (>= this-instruction 3) dec inc)) (+ at this-instruction) (inc n))))))