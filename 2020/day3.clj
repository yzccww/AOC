(ns advent-of-code.2020.day3
  (:require [clojure.string :as string]))

(defn file->seq
  [filename]
  (->> (slurp filename)
       (string/split-lines)))

(defn next-location
  [[row col] [right down]]
  (let [next-row (+ row right)
        next-col (+ col down)]
    [next-row next-col]))

(defn next-location-step
  [right down n]
  (next-location [0 0] [(* right n) (* down n)]))

(defn locations-s
  [slope]
  (let [right (first slope)
        down  (second slope)]
    (map (partial next-location-step right down)
         (range))))

(defn with-height?
  [[_ col] height]
  (< col height))

(defn char-at-location
  [grid [row col]]
  (let [x (grid col)
        y (mod row (count x))]
    (nth x y)))

(defn tree?
  [ch]
  (= ch \#))

(defn count-trees-on-slope
  [grid slope]
  (let [height    (count grid)
        locations (locations-s slope)]
    (->> locations
         (take-while (fn [location] (with-height? location height)))
         (map (fn [loc] (char-at-location grid loc)))
         (filter tree?)
         count)))

(defn product-of-trees
  [grid slopes]
  (apply * (map #(count-trees-on-slope grid %) slopes)))

(def slopes [[1 1] [3 1] [5 1] [7 1] [1 2]])

(comment
  (do (def sample-grid (file->seq "resources/2020/day3/input-sample.txt"))
      sample-grid)
  #_=> ["..##......."
        ...
        ".#..#...#.#"]

  (next-location [1 1] [3 1])
  #_=> [4 2]

  (next-location-step 3 1 2)
  #_=> [6 2]

  (->> (locations-s [3 1])
       (take 3))
  #_=> ([0 0] [3 1] [6 2])

  (with-height? [3 1] 10)
  #_=> true

  (with-height? [3 1] 1)
  #_=> false

  (take-while neg? [-2 0 1])
  #_=> (-2)

  (nth ["a" "b" "c"] 1)
  #_=> "b"

  (char-at-location sample-grid [0 0])
  #_=> \.

  (char-at-location sample-grid [2 0])
  #_=> \#

  (tree? \.)
  #_=> false

  (tree? \#)
  #_=> true

  (count-trees-on-slope sample-grid [3 1])
  #_=> 7

  (product-of-trees sample-grid slopes)
  #_=> 336

  (do (def grid (file->seq "resources/2020/day3/input.txt"))
      grid)
  #_=> [".....#....#...#.#..........#..."
        ...
        ".....#....#..............#....#"]

  (count-trees-on-slope grid [3 1])
  #_=> 299

  (product-of-trees grid slopes)
  #_=> 3621285278
  )