(ns adventofcode2017.day3)

; right
; right not free: up
; right and up not free: left
; down

(def moves
  {:right [1 0]
   :up    [0 1]
   :left  [-1 0]
   :down  [0 -1]})

(def next-moves
  {:right :up
   :up    :left
   :left  :down
   :down  :right})

(defn- pos-after-move
  [position move]
  (map + position (move moves)))

(defn- determine-next-move1
  [positions last-position last-move]
  (let [primary-move (get next-moves last-move)
        after-primary (pos-after-move last-position primary-move)]
    (if (get positions after-primary)
      [last-move (pos-after-move last-position last-move)]
      [primary-move (pos-after-move last-position primary-move)])))

(defn part1
  [n]
  (loop [positions {[0,0] 0}
         last-move :down
         last-position [0,0]
         moves 1]
    (let [[next-move next-position] (determine-next-move1 positions last-position last-move)]
      (if (= moves n)
        (let [[x y] last-position]
          (+ (Math/abs x) (Math/abs y)))
        (do
          (recur
            (merge positions {next-position (inc moves)})
            next-move
            next-position
            (inc moves)))))))

(defn- calculate-new-value
  [positions [new-x new-y]]
  (+ (get positions [(inc new-x) (inc new-y)] 0)
     (get positions [(inc new-x) new-y] 0)
     (get positions [new-x (inc new-y)] 0)
     (get positions [(dec new-x) (dec new-y)] 0)
     (get positions [(dec new-x) new-y] 0)
     (get positions [new-x (dec new-y)] 0)
     (get positions [(dec new-x) (inc new-y)] 0)
     (get positions [(inc new-x) (dec new-y)] 0)))

(defn- determine-next-move2
  [positions last-position last-move]
  (let [primary-move (get next-moves last-move)
        after-primary (pos-after-move last-position primary-move)]
    (if (get positions after-primary)
      (let [after-secondary (pos-after-move last-position last-move)]
        [last-move after-secondary (calculate-new-value positions after-secondary)])
      [primary-move after-primary (calculate-new-value positions after-primary)])))

(defn part2
  [n]
  (loop [positions {[0,0] 1}
         last-move :down
         last-position [0,0]]
    (let [[next-move next-position next-value] (determine-next-move2 positions last-position last-move)]
      (if (< n next-value)
        next-value
        (recur
          (merge positions {next-position next-value})
          next-move
          next-position)))))