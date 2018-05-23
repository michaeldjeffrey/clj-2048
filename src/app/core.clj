(ns app.core
  (:require [etaoin.api :as api]
            [etaoin.keys :as keys]))


;;; TODO: Look into caching the score elements.
;;; TODO: Maybe introduce some threading?
;;; TODO: The actual ML bits

(def ^:dynamic *driver* nil)

(defn open-page []
  "I have a problem"
  (api/go *driver* "https://gabrielecirulli.github.io/2048/"))

(defmacro define-direction [name key]
  `(defn ~name []
     (api/fill *driver* {:tag :body} ~key)))

(define-direction up keys/arrow-up)
(define-direction down keys/arrow-down)
(define-direction left keys/arrow-left)
(define-direction right keys/arrow-right)

(defn get-tiles
  "querier likes to return elements matching regex of :has-class.
  'tile' will return all classes containing the word 'tile'."
  []
  (api/query-all *driver* {:tag :div :fn/has-class "tile-position"}))

(defn tile-classes
  "Get a vector of all classes for each tile block"
  []
  (for [tile (get-tiles)]
    (api/get-element-attr-el *driver* tile :class)))

(defn tile-classes-to-input
  [tiles] 
  (for [tile tiles]
    ;; split on spaces an hyphen
    ;; tile tile-{value} tile-position-{x}-{y}
    ;; coordinates from top left (1, 1) to bottom left (1, 4)
    (let [[_ _ value _ _ x y] (clojure.string/split tile #"( |-)")]
      (map read-string [value x y]))))

(defn get-current-score []
  (read-string
   (api/get-element-text *driver* {:tag :div :fn/has-class "score-container"})))

(defn game-over? []
  (try
    (api/query *driver* {:tag :div :fn/has-class "game-over"})
    (catch Exception e false)))

(defn restart-game []
  (api/click *driver* {:tag :a :fn/has-class "restart-button"}))

;; (get-current-score)
(def next-move (cycle [left down]))

(def DEFAULT-RIGHT-THRESHOLD 3)

(defn play-dumb [restart?]
  (when restart? (restart-game))
  (let [previous-score (atom (get-current-score))
        right-threshold (atom DEFAULT-RIGHT-THRESHOLD)]
    (doseq [move next-move
            :while (not (game-over?))]
      (move)
      (let [current-score (get-current-score)]
        (if (= @previous-score current-score)
          (swap! right-threshold dec)
          (do
            (reset! right-threshold DEFAULT-RIGHT-THRESHOLD)
            (reset! previous-score current-score))))
      (when (= @right-threshold 0)
        (right)
        (reset! right-threshold 3)))
    (get-current-score)))

(def window-size (do
                   (let [test (api/chrome)]
                     (api/maximize test)
                     (let [size (api/get-window-size test)]
                       (api/quit test)
                       size))))
(def width (/ (:width window-size) 2))
(def height (/ (:height window-size) 2))

(def window-configs [[0 0]
                     [width 0]
                     [0 height]
                     [width height]])

(defn one-game [x y]
  (binding [*driver* (api/chrome)]
    (api/set-window-size *driver* width height)
    (api/set-window-position *driver* x y)
    (open-page)
    (api/wait-visible *driver* :active)
    (play-dumb (game-over?))
    (let [score (get-current-score)]
      (api/quit *driver*)
      score)))


(defn do-scores []
  (for [[x y] (take 4 (cycle window-configs))]
    (future
      (one-game x y))))

(def scores (doall (do-scores)))


