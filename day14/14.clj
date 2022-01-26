(require '[clojure.string :as strn])

(defn minmax
  "for reduction (when partial'ed with a suitable keyfn), will return vector
  of largest and smallest"
  [keyfn [prevmax prevmin :as prev] item]
  (let [item' (keyfn item)]
    (cond
      (< prevmax item') [item' prevmin]
      (> prevmin item') [prevmax item']
      :else prev)))

(defn charlist-scan-to-map
  "adds every adjacent pair of chars to a map with
  frequencies"
  [chlist]
  (frequencies (map str chlist (rest chlist))))

(defn parse-rule
  "from form XY -> Z to a pair [XY Z]"
  [rule-string]
  (let [[unit [ch]] (strn/split rule-string #"[ ]+->[ ]+")]
    [unit ch]))

(let [[[a] _ b] (partition-by #(= % "")
                            (strn/split-lines (slurp "14.txt")))]
  (def polymer-string a)
  (def rule-strings b))

(def polymer-map (charlist-scan-to-map polymer-string))
(def rule-map (into {} (map parse-rule rule-strings)))

;; for everything in polymer-map, build a new map by applying rules and merging with +.
;; discard old map.
(defn build-map-from-rules
  [prev-map [[a b :as units] amount]]
  (let [letter (get rule-map units)
        a' (str a letter)
        b' (str letter b)
        to-insert (if (= a' b')
                    {a' (* 2 amount)}
                    {a' amount b' amount})]
    (merge-with + prev-map to-insert)))

(defn count-elements
  [prev-map [[a b] amount]]
  (if (= a b)
    (merge-with + prev-map {a (* 2 amount)})
    (merge-with + prev-map {a amount b amount})))

(def part-2 ;; apply build-map-from-rules 10 times
  (loop [to-map polymer-map
       iteration 0]
  (if (= 40 iteration) to-map
      (recur (reduce build-map-from-rules {} to-map) (inc iteration)))))

((fn [[a b]] (Math/ceil (-  (/ a 2) (/ b 2))))
 (reduce (partial minmax second) [0 Integer/MAX_VALUE] (reduce count-elements {} part-2)))
