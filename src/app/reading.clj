(ns app.cleaning
  (:require [clojure.java.io :as io]
            [me.raynes.fs :as fs]))

(def ^:const position-map [[1 1] [2 1] [3 1] [4 1]
                           [1 2] [2 2] [3 2] [4 2]
                           [1 3] [2 3] [3 3] [4 3]
                           [1 4] [2 4] [3 4] [4 4]])

(defn position-swap
  "structure stored as [value x y] because that's how the classes
  were on the website. Swapping them around lets me use the
  coordinates as a key in a map."
  [[value x y]]
  [[x y] value])

(defn board-state->input
  "Clean board state into a map.
  Create a 16 vector of values."
  [state]
  (let [data (into {} (map position-swap state))]
    (mapv #(get data %) position-map)))

(defn clean-frame [[board move score]]
  {:board (board-state->input board)
   :move move
   :score score})

(defn clean-game [{:keys [score data id]}]
  {:id id
   :score score
   :data (mapv clean-frame data)
   :count (count data)})

(defn write-cleaned-game [data]
  (binding [*print-length* nil]
    (let [file-name (str "src/cleaned-data/" (:id data) ".edn")]
      (spit file-name data))))

(defn get-file-id [file]
  (re-find #"\d+{4}" (.getName file)))

(defn process-file [file]
  (-> file
      slurp
      read-string
      (assoc :id (get-file-id file))
      clean-game
      write-cleaned-game))

(defn clean-all-data []
  (fs/with-cwd "src/training-data"
    (doall (map process-file (fs/glob "*.edn")))))

(comment 
  (clean-all-data))



