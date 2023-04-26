(ns advent-of-code.2020.day3
  (:require [clojure.string :as string]))

(defn file->seq
  [filename]
  (->> (slurp filename)
       (string/split-lines)))

(defn next-location
  [loc [right down]]
  [(+ (first loc) right)
   (+ (second loc) down)])

(defn locations-s
  [height [right down]]
  (take-while #(< (second %) height)
              (map #(next-location [0 0] [(* right %) (* down %)])
                   (range))))

(defn tree?
  [ch]
  (= ch \#))

(defn count-trees-on-slope
  [grid [right down]]
  (let [height          (count grid)
        valid-locations (locations-s height [right down])]
    (->> valid-locations
         (map #(get-in grid [(second %) (mod (first %)
                                             (count (first grid)))]))
         (filter tree?)
         count)))

(defn product-of-trees
  [grid slopes]
  (apply * (map #(count-trees-on-slope grid %) slopes)))

(def slopes [[1 1] [3 1] [5 1] [7 1] [1 2]])

(comment
  (do (def sample-input (file->seq "resources/2020/day3/input-sample.txt"))
      sample-input)
  #_=> ["..##......."
        ...
        ".#..#...#.#"]

  (take-while neg? [-2 0 1])
  #_=> (-2)

  (take 3 (map #(next-location [0 0] [(* 3 %) (* 1 %)])
               (range)))
  #_=> ([0 0] [3 1] [6 2])

  (locations-s 3 [3 1])
  #_=> ([0 0] [3 1] [6 2])

  (mod 10 5)
  #_=> 0

  (get-in sample-input [0 2])
  #_=> \#

  (get-in sample-input [0 0])
  #_=> \.

  (tree? \.)
  #_=> false

  (tree? \#)
  #_=> true

  (count-trees-on-slope sample-input [3 1])
  #_=> 7

  (product-of-trees sample-input slopes)
  #_=> 336

  (do (def input (file->seq "resources/2020/day3/input.txt"))
      input)
  #_=> [".....#....#...#.#..........#..."
        ...
        ".....#....#..............#....#"]

  (count-trees-on-slope input [3 1])
  #_=> 299

  (product-of-trees input slopes)
  #_=> 3621285278
  )