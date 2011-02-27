(ns sudoku)

(defn in? [seq elem]
  "Surprisingly, clojure doesn't provide a standard function
  for such a frequently used case?
  contains? tests whether an element can be `get` from a coll"
  (not (nil? (some #(= % elem) seq))))
