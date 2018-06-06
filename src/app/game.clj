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
(defn add-random-tile [{:keys [board] :as game-state}]
  (let [empty (rand-nth (keys (filter empty-tile? board)))
        new-value (if (> 0.9 (rand)) 2 4)]
    (assoc-in game-state
               [:board empty] new-value)))

(defn add-random-tiles [n game-state]
  (cond
    (zero? n) game-state
    :else (add-random-tiles (dec n)
                            (add-random-tile game-state))))

(defn print-game-board []
  (println "========")
  (doseq [row rows]
    (println (map deref row))))


(defn reset-game []
  (update-game (vals tiles) (take 16 (repeat 0))))
(defmulti move (fn [_ move] move))

(defmethod move :left [game-state _]
  (assoc game-state :board
         (zipmap (orders :rows)
                 (-> game-state
                     (get-board-as :rows)
                     (actuate)))))

(defmethod move :right [game-state _]
  (assoc game-state :board
         (zipmap (orders :rows)
                 (-> game-state
                     (get-board-as :rows)
                     (actuate-reverse)))))

(defmethod move :up [game-state _]
  (assoc game-state :board
         (zipmap (orders :columns)
                 (-> game-state
                     (get-board-as :columns)
                     (actuate)))))

(defmethod move :down [game-state _]
  (assoc game-state :board
         (zipmap (orders :columns)
                 (-> game-state
                     (get-board-as :columns)
                     (actuate-reverse)))))

(defn make-move [game-state instruction]
  (-> game-state
      (assoc :prev-board (:board game-state))
      (move instruction)
      (add-random-tile)
      (simple-score)
      (print-game-board)))

(defn make-n-moves [n game-state]
  (cond
    (zero? n) game-state
    (game-over? game-state) (do
                              (println "Game Over with " n "moves left")
                              (print-game-board game-state))
    :else (let [random-move (rand-nth [:left :right :up :down])
                new-game-state (make-move game-state random-move)]
            (make-n-moves (dec n) new-game-state))))

(defn game-over? []
  "TODO: work correctly."
  (empty? (filter pos? (map deref (vals tiles)))))

(defn new-game []
  (doall (add-random-tiles 2))
  (print-game-board))
(defn new-game
  ([] (new-game 2))
  ([n]
   (add-random-tiles
    n
    {:board (zipmap (orders :rows) (repeat 0))
     :score 0})))
