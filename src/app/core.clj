(ns app.core
  (:require [etaoin.api :as api]
            [etaoin.keys :as keys]))

(def my-driver (api/chrome))

;; I have a problem
(api/go my-driver "https://gabrielecirulli.github.io/2048/")


(defn direction [driver dir]
  (partial api/fill driver {:tag :body} dir))

(def up (direction my-driver keys/arrow-up))
(def down (direction my-driver keys/arrow-down))
(def left (direction my-driver keys/arrow-left))
(def right (direction my-driver keys/arrow-right))


(doseq [i (range 300)]
  ((rand-nth [down left right])))

(defn get-tiles
  "querier likes to return elements matching regex of :has-class.
  'tile' will return all classes containing the word 'tile'."
  []
  (api/query-all my-driver {:tag :div :fn/has-class "tile-position"}))

(defn tile-classes
  "Get a vector of all classes for each tile block"
  []
  (for [tile (get-tiles)]
    (api/get-element-attr-el my-driver tile :class)))

(defn tile-classes-to-input
  [tiles] 
  (for [tile tiles]
    ;; split on spaces an hyphen
    ;; tile tile-{value} tile-position-{x}-{y}
    ;; coordinates from top left (1, 1) to bottom left (1, 4)
    (let [[_ _ value _ _ x y] (clojure.string/split tile #"( |-)")]
      (map read-string [value x y]))))

(tile-classes-to-input (tile-classes))
  

