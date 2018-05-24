(ns app.core
  (:require [etaoin.api :as api]
            [etaoin.keys :as keys]
            [lambda-ml.core :refer :all]
            [lambda-ml.neural-network :refer :all]
            [lambda-ml.util :refer :all]))


;;; TODO: Look into caching the score elements.
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

(def DEFAULT-RIGHT-THRESHOLD 4)
(def DEFAULT-UP-THRESHOLD 3)

(defn play-dumb [restart?]
  (when restart? (restart-game))
  (let [previous-score (atom (get-current-score))
        right-threshold (atom DEFAULT-RIGHT-THRESHOLD)
        up-threshold (atom DEFAULT-UP-THRESHOLD)]
    (doseq [move next-move
            :while (not (game-over?))]
      (move)
      (let [current-score (get-current-score)]
        (if (= @previous-score current-score)
          (swap! right-threshold dec)
          (do
            (reset! right-threshold DEFAULT-RIGHT-THRESHOLD)
            (reset! previous-score current-score)))
        (when (= @right-threshold 0)
          (right)
          (reset! right-threshold DEFAULT-RIGHT-THRESHOLD)
          (when (= current-score (get-current-score))
            (swap! up-threshold dec))
          (when (= @up-threshold 0)
            (up)
            (reset! up-threshold DEFAULT-UP-THRESHOLD)))))
    (get-current-score)))

(def window-size (do
                   (let [test (api/chrome)]
                     (api/maximize test)
                     (let [size (api/get-window-size test)]
                       (api/quit test)
                       size))))
(def width (/ (:width window-size) 3))
(def height (/ (:height window-size) 2))

(def window-configs [[0 0]
                     [width 0]
                     [0 height]
                     [width height]
                     [(* width 2) 0]
                     [(* width 2) height]])

(defmacro mj-with-driver
  "(api/with-driver) macro does a let for local binding.
  I want my binding to be a bit farther spread than that. CL style."
  {:style/indent 1}
  [driver & body]
  `(binding [*driver* ~driver]
     (try
       ~@body
       (finally
         (api/quit *driver*)))))

(defn setup-game [{:keys [width height x y]}]
  (api/set-window-size *driver* width height)
  (api/set-window-position *driver* x y)
  (open-page)
  (api/wait-visible *driver* :active))

(def default-game-options {:width width :height height
                           :x 0 :y 0})

(defn n-games [{:keys [port rounds] :as options}]
  (mj-with-driver (api/chrome {:port port})
    (setup-game (merge default-game-options options))
    (doall
     (for [_ (range rounds)]
       (play-dumb (game-over?))))))

(defn one-game [x y]
  (n-games 1 {:width width :height height
              :x x :y y
              :rounds 1}))

(defn do-scores [rounds]
  (for [[[x y] port] (zipmap (take 6 (cycle window-configs)) (range 9000 10000))]
    (future
      (n-games {:rounds rounds :port port
                :width width :height height
                :x x :y y}))))

(comment 
  (def scores (doall (do-scores 3)))
  (let [all-scores (mapcat deref scores)]
    (println all-scores)
    (println (str "Average Score: " (float (/ (apply + all-scores) (count all-scores)))))))


