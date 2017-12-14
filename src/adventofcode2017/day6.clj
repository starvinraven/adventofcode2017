(ns adventofcode2017.day6)

(def input (vec (map read-string (clojure.string/split "0\t5\t10\t0\t11\t14\t13\t4\t11\t8\t8\t7\t1\t4\t12\t11" #"\t"))))

(defn- redistribute
  [state]
  (let [n-to-distribute (apply max state)
        distribute-at   (.indexOf state n-to-distribute)]
    (loop [state (assoc-in state [distribute-at] 0)
           at    (mod (inc distribute-at) (count state))
           left  n-to-distribute]
      (if (zero? left)
        state
        (recur
          (update-in state [at] inc)
          (mod (inc at) (count state))
          (dec left))))))

(defn part1
  [input]
  (loop [state input
         seen #{}
         i 0]
    (if (contains? seen state)
      i
      (recur
        (redistribute state)
        (conj seen state)
        (inc i)))))


(defn part2
  [input]
  (loop [state input
         seen #{}
         previous-seen nil
         i 0]
    (if (= state (first previous-seen))
      (- i (second previous-seen))
      (recur
        (redistribute state)
        (conj seen state)
        (if (nil? previous-seen)
          (when (contains? seen state) [state i])
          previous-seen)
        (inc i)))))