(ns advent-of-code.2020.day3
  (:require [clojure.string :as string]))

(defn file->seq
  [filename]
  (->> (slurp filename)
       (string/split-lines)))

(defn next-location
  [[row col] [right down]]
  [(+ row right)
   (+ col down)])

(defn locations-s
  [slope]
  (map (fn [n] (next-location [0 0] [(* (first slope) n) (* (second slope) n)]))
       (range)))

(defn valid-locations
  [height locations]
  (->> locations
       (take-while (fn [loc] (< (second loc) height)))))

(defn char-at-location
  [grid loc]
  (get-in grid [(second loc) (mod (first loc) (count (first grid)))]))

(defn tree?
  [ch]
  (= ch \#))

(defn count-trees-on-slope
  [grid [right down]]
  (let [locations       (locations-s [right down])
        valid-locations (valid-locations (count grid) locations)]
    (->> valid-locations
         (map (fn [loc] (char-at-location grid loc)))
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

  (take 3 (locations-s [3 1]))
  #_=> ([0 0] [3 1] [6 2])

  (take-while neg? [-2 0 1])
  #_=> (-2)

  (valid-locations (count sample-input) (take 5 (locations-s [3 1])))
  #_=> ([0 0] [3 1] [6 2] [9 3] [12 4])

  (get-in sample-input [0 2])
  #_=> \#

  (get-in sample-input [0 0])
  #_=> \.

  (char-at-location sample-input [0 0])
  #_=> \.

  (char-at-location sample-input [2 0])
  #_=> \#

  (tree? \.)
  #_=> false

  (tree? \#)
  #_=> true

  (def valid-locations-sample (valid-locations (count sample-input) (locations-s [3 1])))
  #_=> #'advent-of-code.2020.day3/valid-locations-sample

  (->> valid-locations-sample
       (map (fn [loc] (char-at-location sample-input loc))))
  #_=> (\. \. \# \. \# \# \. \# \# \# \#)

  (->> valid-locations-sample
       (map (fn [loc] (char-at-location sample-input loc)))
       (filter tree?))
  #_=> (\# \# \# \# \# \# \#)

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