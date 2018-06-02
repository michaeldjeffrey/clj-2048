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

(defn compact-collection [coll]
  (take 4 (concat
           (->> coll
                (filter pos?)
                (partition-by identity)
                (map even-groups)
                (map adder)
                (flatten))
           (repeat 0))))

(defn left [rows]
  (map compact-collection rows))

(defn right [rows]
  (->> rows
       (map reverse)
       (map compact-collection)
       (map reverse)))

(defn up [columns]
  (left columns))

(defn down [columns]
  (right columns))

