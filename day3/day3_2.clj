(require '[clojure.string :as strn])

(def input (-> "3_1.txt"
               slurp
               (strn/split-lines)))

(defn most-common-n [n coll]
  (let [chars (map #(get-in % [1 n]) coll)
        freq0 (reduce (fn [acc item] (if (= \0 item) (inc acc) acc)) 0 chars)
        freq1 (- (count chars) freq0)]
    (if (> freq0 freq1) \0 \1)
    ))

(defn reduce-to-largest-and-smallest [coll]
  (let [i-list (range (count coll))
        partitions (group-by (fn [[i y]] (= (most-common-n i coll) (get y i)))
                             (map (fn [a b] [a b]) i-list coll))]
    [(first (get partitions true)), (first (get partitions false))]))

(let [[largest smallest] input]
  (println smallest)
  (println largest))
