(ns aoc24.src.day14
  (:require
   [aoc-tools :refer [read-input submit-answer]]
   [utils :refer [extract-numbers]]
   [clojure.string :refer [join]]))

(def input (read-input :test))
(def real-input (read-input))

(def borders [101 103])
(def hrz (/ (dec (first borders)) 2))
(def vrt (/ (dec (second borders)) 2))

(defn get-robots
  [input]
  (->> input
       (map extract-numbers)))

(defn move
  [[x y vx vy]]
  [(mod (+ x vx) (first borders)) (mod (+ y vy) (second borders)) vx vy])

(defn move-n
  [robot n]
  (->> robot
       (iterate move)
       (take (inc n))
       (last)))

(defn move-all
  [robots]
  (map move robots))

(defn quadrant
  [[x y vx vy]]
  (cond
    (and (< x hrz) (< y vrt)) 1
    (< x hrz) 2
    (< y vrt) 3
    :else 4))

;; Part 1
(->> real-input
     (get-robots)
     (map #(move-n % 100))
     (remove #(= (first %) hrz))
     (remove #(= (second %) vrt))
     (map quadrant)
     (frequencies)
     (vals)
     (apply *)
     #_(submit-answer 1))
