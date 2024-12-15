(ns aoc24.src.day15
  (:require
   [aoc-tools :refer [read-input submit-answer]]
   [utils :refer [mfilter]]
   [clojure.string :as str]))

(def input (read-input "day15.ex2"))
(def real-input (read-input))

(def up [-1 0])
(def down [1 0])
(def left [0 -1])
(def right [0 1])

(defn parse
  [input]
  (let [[grid moves] (->> input
                          (split-with (complement #{""})))
        grid (into {} (for [x (range (count grid))
                            y (range (count (first grid)))
                            :let [char (nth (nth grid x) y)]
                            :when (not= char \.)]
                        [[x y] char]))
        start-pos (->> grid
                       (mfilter :vals #{\@})
                       (keys)
                       (first))
        grid (-> grid
                 (dissoc start-pos)
                 (assoc :position start-pos))
        moves (map {\< left \> right \^ up \v down} (apply str moves))]

    [grid moves]))

(defn box?
  [char]
  (= char \O))

(defn wall?
  [char]
  (= char \#))

(defn push-into
  [grid pos direction]
  (let [next (->> (map + pos direction)
                  (iterate (partial map + direction))
                  (partition-by #(wall? (grid %)))
                  (first)
                  (filter #(nil? (grid %)))
                  (first))]
    (if (nil? next)
      grid
      (-> grid
          (dissoc pos)
          (assoc :position pos)
          (assoc next \O)))))

(defn draw
  [grid]
  (let [border (->> grid
                    (keys)
                    (remove keyword?)
                    (map first)
                    (apply max))
        printout (->> (for [x (range (inc border))
                            y (range (inc border))]
                        (if (= [x y] (grid :position))
                          \@
                          (get grid [x y] \.)))
                      (partition (inc border))
                      (map (partial apply str))
                      (str/join "\n"))]
    (println printout)))

(defn move
  [grid direction]
  (let [next (map + (grid :position) direction)]
    (cond
      (wall? (grid next)) grid
      (box? (grid next)) (-> grid
                             (push-into next direction))
      :else (assoc grid :position next))))

(defn gps
  [[x y]]
  (+ (* 100 x) y))


;; Part 1
(time
 (let [[grid moves] (parse real-input)]
   (->> moves
        (reduce move grid)
        (mfilter :vals #{\O})
        (keys)
        (map gps)
        (apply +)
        #_(submit-answer 1))))
