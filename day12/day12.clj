(require '[clojure.string :as strn])

(def input
  (map #(strn/split % #"-")
       (strn/split-lines (slurp "12.txt"))))

(defn cave-make [xs]
  (loop [s {}
         [[a b :as x] & rest] xs]
    (if (nil? x) s
        (recur
         (update (update s a conj b) b conj a)
         rest))))

(def caves (cave-make input))

(defn small? "x is a string" [x]
  (Character/isLowerCase (first x)))

(defn step [[visit twice] cave]
  (cond
    (small? cave)
    (if (not (get visit cave))
      (if (and twice (not= cave "start"))
        [visit false]
        nil)
      [(disj visit cave) twice])
    :else [visit twice]))

(defn walk [caves visit cave]
  (if (= "end" cave) 1
      (let [item (step visit cave)]
        (if (not item) 0
            (reduce +
                    (map (partial walk caves item) (get caves cave)))))))

(def inter (into #{} (filter small? (map first caves))))

(def final (map #(walk caves [inter %] "start") [false true]))

(doseq [i final] (println i))
