(ns aoc-tools
  (:require
   [utils :refer [extract-numbers parse-int]]
   [clj-http.client :as client]
   [clojure.string :as str]
   [clojure.java.io :as io]))

(defn extract-example-text
  [body]
  (->> body
       (re-seq #"(?is)<pre><code>(.*?)\n? *?</code></pre>")
       (first)
       (last)))

(defn fetch-input
  [year day]
  (let [base-url       (str "https://adventofcode.com/20" year "/day/" day)
        input-url      (str base-url "/input")
        cookie         (slurp "token.txt")
        payload        {:headers {:Cookie     cookie
                                  :User-Agent "Tomas' AoC Clojure Library https://github.com/tpek-sbab/advent-of-code"}}
        base-response  (client/get base-url payload)
        example-text   (extract-example-text (:body base-response))
        input-response (client/get input-url payload)
        input-text     (:body input-response)]
    [example-text input-text]))


(defn create-files
  ([day]
   (let [year (->> (io/file ".")
                   (.list)
                   (filter #(re-seq #"aoc\d\d" %))
                   (sort)
                   (last)
                   (re-seq #"\d+")
                   (first))]
     (create-files (parse-int year) day)))

  ([year day]
   (let [year       (mod year 2000)
         input-path (str "aoc" year "/inputs/")
         src-path   (str "aoc" year "/src/")
         [example-text input-text] (fetch-input year day)]
     (do
       (spit (str input-path "day" day ".ex") example-text)
       (spit (str input-path "day" day ".txt") input-text)
       (spit (str src-path "day" day ".clj") (str "(ns aoc" year ".src.day" day "\n"
                                                  "  (:require \n"
                                                  "    [aoc-tools :refer [read-input submit-answer]]))\n\n"
                                                  "(def input (read-input :test))\n"
                                                  "(def real-input (read-input))\n\n"))))))

(defn get-year-and-day
  []
  (let [[project _ file] (str/split (str *ns*) #"\.")
        year (first (extract-numbers project))
        day  (first (extract-numbers file))]
    [year day]))

(defn read-input
  [& [arg]]
  (let [[year day] (get-year-and-day)
        file (cond
               (nil? arg) (str "day" day ".txt")
               (= arg :test) (str "day" day ".ex")
               :else (str (name arg)))
        path (str "aoc" year "/inputs/" file)]
    (->> (with-open [rdr (io/reader path)]
           (mapv str (line-seq rdr))))))

(defn submit-answer
  [part answer]
  (let [_ (println "Submitting answer" answer)
        [year day] (get-year-and-day)
        url      (str "https://adventofcode.com/20" year "/day/" day "/answer")
        cookie   (slurp "token.txt")
        payload  {:method      "POST"
                  :headers     {:Cookie     cookie
                                :User-Agent "Tomas' AoC Clojure Library https://github.com/tpek-sbab/advent-of-code"}
                  :form-params {"level" (str part) "answer" (str answer)}}
        response (client/post url payload)]
    (re-seq #"(?is).{20}answer.{50}" (:body response))))

