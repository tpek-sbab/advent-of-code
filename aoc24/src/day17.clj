(ns aoc24.src.day17
  (:require
   [aoc-tools :refer [read-input submit-answer]]
   [utils :refer [mfilter extract-numbers]]
   [clojure.math.numeric-tower :refer [expt]]
   [clojure.string :refer [join]]))

(def input (read-input :day17.ex2))
(def real-input (read-input))


(defn get-computer
  [input]
  (->> input
       (map extract-numbers)
       (remove empty?)
       (map #(if (= (count %) 1) (first %) %))
       (interleave [:A :B :C :program])
       (partition 2)
       (map vec)
       (into {:pointer 0 :output [] :continue true})))

(defn combo
  [computer operand]
  (cond
    (< operand 4) operand
    (= operand 4) (computer :A)
    (= operand 5) (computer :B)
    (= operand 6) (computer :C)))


(defn adv
  [computer operand]
  (let [operand (combo computer operand)
        denom (expt 2 operand)]
    (-> computer
        (update :A / denom)
        (update :A int)
        (update :pointer + 2))))

(defn bxl
  [computer operand]
  (-> computer
      (update :B bit-xor operand)
      (update :pointer + 2)))

(defn bst
  [computer operand]
  (let [operand (combo computer operand)]
    (-> computer
        (assoc :B (mod operand 8))
        (update :pointer + 2))))

(defn jnz
  [computer operand]
  (cond
    (zero? (computer :A)) (update computer :pointer + 2)
    (= operand (computer :pointer)) (update computer :pointer + 2)
    :else (assoc computer :pointer operand)))

(defn bxc
  [computer operand]
  (-> computer
      (update :B bit-xor (computer :C))
      (update :pointer + 2)))

(defn out
  [computer operand]
  (let [operand (combo computer operand)]
    (-> computer
        (update :output conj (mod operand 8))
        (update :pointer + 2))))

(defn bdv
  [computer operand]
  (let [operand (combo computer operand)
        denom (expt 2 operand)]
    (-> computer
        (assoc :B (computer :A))
        (update :B / denom)
        (update :B int)
        (update :pointer + 2))))

(defn cdv
  [computer operand]
  (let [operand (combo computer operand)
        denom (expt 2 operand)]
    (-> computer
        (assoc :C (computer :A))
        (update :C / denom)
        (update :C int)
        (update :pointer + 2))))

(def program-map {0 adv 1 bxl 2 bst 3 jnz 4 bxc 5 out 6 bdv 7 cdv})

(defn process
  [computer]
  (if (>= (computer :pointer) (count (computer :program)))
    (dissoc computer :continue)
    (let [opcode (nth (computer :program) (computer :pointer))
          program (program-map opcode)
          operand (nth (computer :program) (inc (computer :pointer)))
          next (program computer operand)]
      (if (= next computer)
        (dissoc next :continue)
        next))))

(defn output
  [computer]
  (let [final (->> computer
                   (iterate process)
                   (drop-while :continue)
                   (first))
        outp (final :output)]
    outp))

;; Part 1
(->> real-input
     (get-computer)
     (output)
     (map str)
     (join ",")
     #_(submit-answer 1))


(defn nth-output
  [computer n]
  (let [outp (->> computer
                  (iterate process)
                  (drop-while #(and
                                (:continue %)
                                (< (count (:output %)) n)))
                  (first)
                  (:output))]
    (if (< (count outp) n)
      nil
      (last outp))))

(defn check-val
  [computer val]
  (map #(nth-output (assoc computer :A %) val) (range 10000)))

(defn from-oct
  [n]
  (->> n
      ;;  (reverse)
       (map * (iterate #(* % 8) 1))
       (apply +)))

(def computer (get-computer input))

(defn minmax
  [coll]
  [(apply min coll) (apply max coll)])

(->> (range)
     (map #(assoc (get-computer real-input) :A %))
     (map output)
     (map from-oct)
     (map #(quot % 64))
     (partition 64)
     (map set)
     (map first)
     (remove #(< % 1000))
    ;;  (map minmax)
     )

(defn calc
  [x]
  (from-oct (output (assoc (get-computer real-input) :A x))))


(for [a (range 2990 3010)]
  (let [ca (calc a)]
    [a ca (float (/ ca a))]))

;; Part 2
;; (time
;;  (let [computer (get-computer real-input)]
;;    (->> (range)
;;         (map (partial check-value computer))
;;         (drop-while nil?)
;;         (first))))
