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

(defn pull-left [coll]
  (->> coll
       (filter pos?)
       (partition-by identity)
       (map even-groups)
       (map adder)
       flatten))

(defn compact-collection [coll]
  (take 4 (concat (pull-left coll)
                  (repeat 0))))

(defn update-tile [tile new-val]
  (reset! tile new-val))

(defn update-tiles [old new-vals]
  (doall (map update-tile old new-vals)))

(defn nested-deref [coll]
  (mapv #(mapv deref %) coll))

(defmacro reversing-game-state [body]
  (let [[func coll] body]
    `(map reverse (~func (map reverse ~coll)))))

(defn only-move [tiles]
  (update-tiles
   tiles
   (compact-collection (nested-deref tiles))))

(defn left [rows]
  (only-move rows))

(defn right [rows]
  (reversing-game-state
   (only-move rows)))

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

(defn reset-game []
  (update-tiles (vals tiles) (take 16 (repeat 0))))

(defn new-game []
  (doall (add-random-tiles 2))
  (print-game-board))
