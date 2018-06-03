(ns app.game)

(defn- adder
  "Ignores empty seqs because adding them results in spacer 0s
  [(8) ()] -> [(8) (0)] is bad."
  [nums]
  (->> nums
       (filter seq)
       (map #(apply + %))))

(defn- even-groups [coll]
  (split-at 2 coll))

(def pull-left
  (comp
   (filter pos?)
   (partition-by identity)
   (map even-groups)
   (map adder)
   flatten))

(defn compact-collection [coll]
  (take 4 (concat (pull-left coll)
                  (repeat 0))))

(defn update-tiles [colls]
  (doseq [coll colls]
    (let [values (compact-collection coll)
          zipped (zipmap coll values)]
      (doseq [[a val] zipped]
        (reset! a val)))))

(defn left [rows]
  (update-tiles rows))

(defn right [rows]
  (->> rows
       (map reverse)
       update-tiles
       (map reverse)))

(defn up [columns]
  (left columns))

(defn down [columns]
  (right columns))


;; Top left (1, 1)
;; Bottom right (4, 4)
(def tiles {:x1y1 (atom 0) :x2y1 (atom 0) :x3y1 (atom 0) :x4y1 (atom 0)
            :x1y2 (atom 0) :x2y2 (atom 0) :x3y2 (atom 0) :x4y2 (atom 0)
            :x1y3 (atom 0) :x2y3 (atom 0) :x3y3 (atom 0) :x4y3 (atom 0)
            :x1y4 (atom 0) :x2y4 (atom 0) :x3y4 (atom 0) :x4y4 (atom 0)})

(def rows (partition 4
                     (map tiles [:x1y1 :x2y1 :x3y1 :x4y1
                                 :x1y2 :x2y2 :x3y2 :x4y2
                                 :x1y3 :x2y3 :x3y3 :x4y3
                                 :x1y4 :x2y4 :x3y4 :x4y4])))

(def columns (partition 4
                        (map tiles [:x1y1 :x1y2 :x1y3 :x1y4
                                    :x2y1 :x2y2 :x2y3 :x2y4
                                    :x3y1 :x3y2 :x3y3 :x3y4
                                    :x4y1 :x4y2 :x4y3 :x4y4])))

(def moves {:left  #(left rows)
            :right #(right rows)
            :up    #(up columns)
            :down  #(down columns)})

(defn add-random-tile []
  (reset!
   (rand-nth (filter #(zero? @%) (vals tiles)))
   (if (> 0.9 (rand)) 2 4)))

(defn add-random-tiles [n]
  (repeatedly n add-random-tile))

(defn print-game-board []
  (doseq [row rows]
    (println (map deref row))))

(defn make-move [instruction]
  (apply (instruction moves))
  (add-random-tile)
  (print-game-board))

