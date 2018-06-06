(ns app.game)

(def ^:const orders {:rows    [:x1y1 :x2y1 :x3y1 :x4y1
                               :x1y2 :x2y2 :x3y2 :x4y2
                               :x1y3 :x2y3 :x3y3 :x4y3
                               :x1y4 :x2y4 :x3y4 :x4y4]
                     :columns [:x1y1 :x1y2 :x1y3 :x1y4
                               :x2y1 :x2y2 :x2y3 :x2y4
                               :x3y1 :x3y2 :x3y3 :x3y4
                               :x4y1 :x4y2 :x4y3 :x4y4]})

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

(defn update-foursome [old new-vals]
  (mapv update-tile old new-vals))

(defn update-game [old new-vals]
  (mapv update-foursome old new-vals))

(defn nested-deref [coll]
  (mapv #(mapv deref %) coll))

(defn only-move [tiles]
  (update-game
   tiles
   (map compact-collection
        (nested-deref tiles))

(defn reverse-only-move [tiles]
  (->> tiles
       (map reverse)
       (only-move)
       (map reverse)))

;; Top left (1, 1)
;; Bottom right (4, 4)
(def tiles {:x1y1 (atom 0) :x2y1 (atom 0) :x3y1 (atom 0) :x4y1 (atom 0)
            :x1y2 (atom 0) :x2y2 (atom 0) :x3y2 (atom 0) :x4y2 (atom 0)
            :x1y3 (atom 0) :x2y3 (atom 0) :x3y3 (atom 0) :x4y3 (atom 0)
            :x1y4 (atom 0) :x2y4 (atom 0) :x3y4 (atom 0) :x4y4 (atom 0)})


(def rows (->> orders
               :rows
               (mapv tiles)
               (partition 4)))

(def columns (->> orders
                  :columns
                  (mapv tiles)
                  (partition 4)))

(defn add-random-tile []
  (reset!
   (rand-nth (filter #(zero? @%) (vals tiles)))
   (if (> 0.9 (rand)) 2 4)))

(defn add-random-tiles [n]
  (repeatedly n add-random-tile))

(defn print-game-board []
  (println "========")
  (doseq [row rows]
    (println (map deref row))))

(defmulti move identity)

(defmethod move :left [_]
  (only-move rows))

(defmethod move :right [_]
  (reverse-only-move rows))

(defmethod move :up [_]
  (only-move columns))

(defmethod move :down [_]
  (reverse-only-move columns))


(defn make-move [instruction]
  ;; TODO: Reference to columns and rows no correct in 'moves
  (move instruction)
  (add-random-tile)
  (print-game-board))

(defn make-n-moves [n]
  (run! make-move (repeatedly n #(rand-nth [:left :right :up :down]))))

(defn reset-game []
  (update-game (vals tiles) (take 16 (repeat 0))))

(defn game-over? []
  "TODO: work correctly."
  (empty? (filter pos? (map deref (vals tiles)))))

(defn new-game []
  (doall (add-random-tiles 2))
  (print-game-board))
