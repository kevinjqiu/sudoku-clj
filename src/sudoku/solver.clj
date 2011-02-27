(ns sudoku.solver
  :use [sudoku.const :only [in? *squares* *peers* *digits*]])

(into {} [[:a 1] [:b 2]])

(def *test-grid* "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......")

(defn- create-init-grid []
  (into {} (map #(vector % *digits*) *squares*)))

(defn- grid-values [grid]
  "convert grid string into a dict of {square:char} with '.' representing empty cell"
  (zipmap *squares* grid))

(defn- eliminated [digits digit]
  "returns digits with digit eliminated"
  (clojure.string/replace-first digits digit ""))

(defn- try-eliminate-digit [grid square digit]
  "eliminate digit from grid[square]; propagate when values or places <= 2.
  Return grid, or false if a contradiction is detected"
  (let [digits (get grid square)]
    (if (not (in? digits digit))
      grid ; already eliminated
      (let [current-grid (update-in grid [square] #(eliminated digits digit))
            current-digits-in-square (get current-grid square)]
        (cond 
          (empty current-digits-in-square) false ; Contradiction: eliminated the last value
          (= 1 (count current-digits-in-square))
            (let [current-digit-in-square current-digits-in-square] ; alias
              
              ;(try-eliminate-digits current-grid )))) 

(defn- try-eliminate-digits [grid square current-digit digits]
  (cond (empty? digits) 
          grid
        :else
          (let [current-grid (try-eliminate-digit grid square current-digit)]
            (if (false? current-grid) 
              false
              (recur current-grid square (first digits) (rest digits))))))

(defn- assign [grid square digit]
  "Eliminate all the other values (except d) from grid[square]
  and propagate. Return false if a contradiction is detected"
  (let [digits-to-eliminate (eliminated (get grid square) digit)]
    (try-eliminate-digits grid square (first digits-to-eliminate) (rest digits-to-eliminate))))

(defn- try-assign-values-to-grid [current-grid-value rest-grid-values current-grid]
  "current-grid-value: a tuple of (square, char)
   rest-grid-values:   a map of {square:char}
   current-grid:       a map of {square:chars} -- this is the current state of the grid"
  (cond 
    (nil? current-grid-value) 
      current-grid
    :else
      (let [s (first current-grid-value)
            d (last current-grid-value)]
        (if (not (in? *digits* d)) 
          false
          (let [current-state (assign current-grid s d)]
            (if (false? current-state)
              false
              (recur (first rest-grid-values) (rest rest-grid-values) current-state)))))))

(defn parse-grid [grid]
  "convert grid to a map of possible values,
  in the form of {square: digits},
  or False if a contradiction is detected."
  (let [init-grid (create-init-grid)
        values (grid-values grid)]
    (try-assign-values-to-grid (first values) (rest values) init-grid)))
  
