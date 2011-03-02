(ns sudoku.util)

(defn in? [seq elem]
  "Surprisingly, clojure doesn't provide a standard function
  for such a frequently used case?
  contains? tests whether an element can be `get` from a coll"
  (not (nil? (some #(= % elem) seq))))

(defn copy-map [a-map]
  "Make a copy of the hash-map"
  (apply hash-map (interleave (keys a-map) (vals a-map))))

(defn first-not-falsy-elem
  ([elem coll]
  (cond (empty? coll) elem
        ((comp not false?) elem) elem
        :else (recur (first coll) (rest coll))))
  ([coll]
  (first-not-falsy-elem (first coll) (rest coll))))
