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

(defn +compact-collection+ [coll]
  (take 4 (concat (pull-left coll)
                  (repeat 0))))

(def compact-collection (memoize +compact-collection+))

(defn actuate [input]
  (flatten (map compact-collection input)))

(defn actuate-reverse [input]
  (flatten
   (->> input
        (map reverse)
        (map compact-collection)
        (map reverse))))
(defn simple-score [{:keys [board] :as game-state}]
  (assoc game-state :score (reduce + (vals board))))

(defn empty-tile? [[_ value]]
  (zero? value))

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

(defn print-game-board [game-state]
  (println "========")
  (println (str "Score: " (:score game-state)))
  (doseq [row (get-rows game-state)]
    (println row))
  game-state)


(defn get-board-as [{:keys [board]} direction]
  (->> (get orders direction)
       (map board)
       (partition 4)))

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

(defn cells-available? [{:keys [board]}]
  (seq (filter empty-tile? board)))

(defn matches-available? [{:keys [board] :as game-state}]
  (or
   (= board
      (actuate (get-board-as game-state :rows)))
   (= board
      (actuate (get-board-as game-state :columns)))))

(defn game-over? [game-state]
  (not (or (cells-available? game-state)
           (matches-available? game-state))))


(defn new-game
  ([] (new-game 2))
  ([n]
   (add-random-tiles
    n
    {:board (zipmap (orders :rows) (repeat 0))
     :score 0})))
