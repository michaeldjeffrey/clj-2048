(ns app.core
  (:require [clojure.string :refer [split]]
            [etaoin.api :as api]
            [etaoin.keys :as keys]))

(def ^:dynamic *driver*)

(declare left down up right)
(def ^:const full-move-set [#'left #'down #'up #'right])

;; TODO: Read from training folder and continue where left off
;; 880
(def run-count (atom 1000))
;; (reset! run-count 0)

(defmacro with-driver
  "(api/with-driver) macro does a let for local binding.
  I want my binding to be a bit farther spread than that. CL style."
  {:style/indent 1}
  [driver & body]
  `(binding [*driver* ~driver]
     (try
       ~@body
       (catch Exception e#
         (clojure.pprint/pprint e#))
       (finally
         (api/quit *driver*)))))

(defmacro define-direction [name key value]
  `(defn ~name []
     (api/fill *driver* {:tag :body} ~key)
     ~value))

(define-direction up keys/arrow-up 0)
(define-direction down keys/arrow-down 1)
(define-direction left keys/arrow-left 2)
(define-direction right keys/arrow-right 3)

(defn go [page]
  (api/go *driver* page))

(defn elements-with-class
  "Assuming all elements we want to query are divs"
  [class]
  (api/query-all *driver* {:tag :div :fn/has-class class}))

(defn click-with-class
  "Assuming all elements we want to click are links"
  [class]
  (api/click *driver* {:tag :a :fn/has-class class}))

(defn classes-of [element]
  (api/get-element-attr-el *driver* element :class))

(defn text-of [class-name]
  (api/get-element-text *driver* {:tag :div :fn/has-class class-name}))

(defn deconstruct-classes [class-string]
  (split class-string #"( |-)"))

(defn pull-values
  ;; tile tile-{value} tile-position-{x}-{y}
  ;; coordinates from top left (1, 1) to bottom left (1, 4)
  [[_ _ tile-value _ _ tile-x tile-y]]
  (map read-string [tile-value tile-x tile-y]))

(defn tiles->input [tiles]
  (->> tiles
       (map classes-of)
       (map deconstruct-classes)
       (map pull-values)))

(defn current-score []
  (read-string
   (text-of "score-container")))

(defn game-over? []
  (not-empty (elements-with-class "game-over")))

(defn restart-game []
  (click-with-class "restart-button"))

(defn make-move-get-state [move]
  (let [move-value (move)]
    (Thread/sleep 50)
    [(into [] (tiles->input (elements-with-class "tile-position")))
     move-value
     (current-score)]))

(defn random-game []
  (for [instruction (repeatedly #(rand-nth full-move-set))
        :while (not (game-over?))]
    (make-move-get-state instruction)))

(defn pad [n val coll]
  (take n (concat coll (repeat val))))

(defn get-file-number []
  (apply str (-> run-count
                 (swap! inc)
                 (str)
                 (split #"")
                 (reverse)
                 (->>
                  (pad 4 "0"))
                 (reverse))))

(defn get-file-name []
  (str "src/training-data/game-" (get-file-number) ".edn"))

(def windows [{:x 0 :y 0 :port 9000}
              {:x 0 :y 525 :port 9001}
              {:x 400 :y 0 :port 9002}
              {:x 400 :y 525 :port 9003}
              {:x 800 :y 0 :port 9004}
              {:x 800 :y 525 :port 9005}
              {:x 1200 :y 0 :port 9006}
              {:x 1200 :y 525 :port 9007}
              {:x 1600 :y 0 :port 9008}
              {:x 1600 :y 525 :port 9009}])

(defn position-window [{:keys [x y]}]
  (api/set-window-size *driver* 300 500)
  (api/set-window-position *driver* x y)
  (api/scroll *driver* 0 150))

(defn game->data [game]
  {:moves (count game)
   :data game
   :score (current-score)})

(defn write-game-data [data]
  (binding [*print-length* nil]
    (let [file-name (get-file-name)]
      (println "writing file " file-name)
      (spit file-name data))))

(defn run-instance [config]
  (with-driver (api/chrome {:port (:port config)})
    (go "https://gabrielecirulli.github.io/2048/")
    (Thread/sleep 250)
    (position-window config)
    (doseq [i (range 5)]
      (restart-game)
      (-> (random-game)
          (game->data)
          (write-game-data)))))



(comment
  (doall (map #(future (run-instance %)) windows)))

(comment
  (run-instance {:x 0 :y 0 :port 9000}))
