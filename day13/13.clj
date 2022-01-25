(require '[clojure.string :as strn])

(defn make-points
  "makes a vector of two ints
  which represent a point in 2-D
  space. input string in form x,y"
  [point-string]
  (let [[f s] (strn/split point-string #",")]
    [(read-string f) (read-string s)]))

(defn make-instruction
  "makes vector [dir num] that represents
  folding instructions from a statement
  `fold along x = 665` for instance"
  [instruction-string]
  ((fn [[[dir] numlist]][dir (read-string (strn/join (drop 1 numlist)))])
   (split-at 1 (drop 11 instruction-string))))

;; makes points and instructions vector
(let [inp (strn/split-lines
           (slurp "13.txt"))
      [coords _ commands]
      (partition-by #(= % "") inp)]
  (def coord-pairs (map make-points coords))
  (def command-pairs (map make-instruction commands)))

(defn fold-points
  "[points]-> instruction -> [points]"
  [points [dir value]]
  (if (= \y dir)
    ((comp (partial map first) (partial partition-by identity) sort)
         (map (fn [[x y :as point]] (if (> y value)
                           [x (- value (- y value))]
                           point)) points))
    ((comp (partial map first) (partial partition-by identity) sort)
         (map (fn [[x y :as point]] (if (> x value)
                           [(- value (- x value)) y]
                           point)) points))))

(defn render-row
  "returns string that visaully represents a row given
  points."
  [points]
  (let [xs (into #{} (map first points))
        limit (inc
               (reduce max xs))]
   (strn/join (map (fn [i] (if (get xs i) \# \space)) (range limit)))))

(defn visualise
  [points]
  (strn/join "\n"
   (map render-row
        (partition-by second
                      (sort-by second points)))))

;; part 1
(count (fold-points coord-pairs (first command-pairs)))

;; part 2
(spit "rendered.txt"
      (visualise (reduce fold-points coord-pairs command-pairs)))
