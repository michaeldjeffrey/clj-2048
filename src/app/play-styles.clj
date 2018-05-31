(comment
  (ns app.play-styles
    (:require [app.driver :refer [up down left right elements-with-class] :as driver]
              [app.game :refer [current-score game-over? tiles->input]]
              [etaoin.api :as api]))

  (def ^:dynamic *rt* 3)
  (def ^:dynamic *ut* 3)

  (def ^:dynamic *move-set* (cycle [left down]))

  (def ^:const full-move-set [left down up right])

  (def ^:dynamic data (atom []))

  (driver/with-driver (api/chrome)
    (driver/go "http"))

  (defn random-game []
    (while (not (game-over?))
      (let [instruction (rand-nth full-move-set)]
        (instruction)
        (swap! data conj 
               [(tiles->input (elements-with-class "tile-position"))
                (str (:name (meta instruction)))
                (current-score)]))))


  (defn one []
    (let [prev-score (atom (current-score))
          right-threshold (atom *rt*)
          up-threshold (atom *ut*)]
      (doseq [instruction *move-set*
              :while (not (game-over?))]
        (instruction)
        (let [curr-score (delay (current-score))]
          (if (= @prev-score @curr-score)
            (swap! right-threshold dec)
            (do
              (reset! right-threshold *rt*)
              (reset! prev-score curr-score)))
          (when (zero? @right-threshold)
            (right)
            (reset! right-threshold 0)
            (when (= @curr-score (current-score))
              (swap! up-threshold dec))
            (when (zero? @up-threshold)
              (up)
              (reset! up-threshold *ut*)))))
      (current-score)))

  (declare -two)
  (defn two []
    (let [[f & r] *move-set*]
      (-two f r 0 *rt* *rt* *ut*)))

  (defn -two [instruction move-set prev-score r-thresh r-attempts u-thresh]
    (instruction)
    (let [[h & t] move-set
          curr-score (current-score)
          same-score? (= prev-score curr-score)]
      (cond
        (game-over?) curr-score
        (zero? u-thresh) (-two up t curr-score r-thresh r-attempts *ut*)
        (zero? r-attempts) (-two h t curr-score r-thresh *rt* (dec u-thresh))
        (zero? r-thresh) (-two right t curr-score *rt* (dec r-attempts) u-thresh)
        same-score? (-two h t curr-score (dec r-thresh) r-attempts u-thresh)
        :else (recur h t curr-score r-thresh r-attempts u-thresh)))))



  
