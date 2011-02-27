(ns sudoku.solver
  (:use [clojure.contrib.logging :only [spy info]]
        [sudoku :only [in?]]
        [sudoku.const :only [*squares* *peers* *digits* *units*]]))

(def *test-grid* "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......")

(declare assign)
(declare eliminate)

(defn- create-init-grid []
  (into {} (map #(vector % *digits*) *squares*)))

(defn- grid-values [grid]
  "convert grid string into a dict of {square:char} with '.' representing empty cell"
  (zipmap *squares* grid))

(defn- eliminated [digits digit]
  "returns digits with digit eliminated"
  (clojure.string/replace-first digits digit ""))

(defn- try-assign-digit-in-unit [grid-ref current-unit digit]
  "assign digit to the one place available to it in unit,
  otherwise, return false"
  (println (str "try-assign-digit-in-unit: " current-unit ", " digit))
  (let [places-for-digit (filter #(in? (get @grid-ref %) digit) current-unit)]
    (cond (empty? places-for-digit) (do (println "no place for digit") false) ;; Contradiction: no place for digit
          (= 1 (count places-for-digit))
            ;; assign it!
            (assign grid-ref (first places-for-digit) digit)
          :else true)))

(defn- try-assign-digit-in-units [grid-ref units digit]
  (some true? (map #(try-assign-digit-in-unit grid-ref % digit) units)))

(defn- no-contradictions? [grid-ref square remaining-digits]
  (println remaining-digits)
  (cond 
    (empty? remaining-digits) (do (println "last digit removed" false)) ;; Contradiction: last digit removed
    (= 1 (count remaining-digits))
      ;; if a square is reduced only to a single value, 
      ;; eliminate that value from its peers.
      (every? true? (map #(eliminate grid-ref % remaining-digits) (get *peers* square)))
    :else
      true))

(defn- eliminate [grid-ref square digit]
  "Eliminate d from @grid-ref[square]; propagate when values or places <= 2.
  Return false if contradiction is detected. False otherwise"
  (println (str "eliminate[" square "," digit "]"))
  (let [digits (get @grid-ref square)]
    (if (not (in? digits digit)) true
      (let [new-values (eliminated digits digit)]
        (dosync (alter grid-ref #(assoc % square new-values)))
        ;(println (str "new-gridref" (get @grid-ref square)))
        (if (not (no-contradictions? grid-ref square new-values)) false
          ;; if the unit is reduced to only one place for value d, then put it there
          (try-assign-digit-in-units grid-ref (get *units* square) digit))))))

(defn- assign [grid-ref square digit]
  "Eliminate all the other values (except d) from grid[square]
  and propagate. Return false if a contradiction is detected"
  (println (str "assign[" square "," digit "]"))
  (let [digits-to-eliminate (eliminated (get @grid-ref square) digit)]
    (every? true? (map #(eliminate grid-ref square %) digits-to-eliminate))))

(defn parse-grid [grid]
  "convert grid to a map of possible values,
  in the form of {square: digits},
  or False if a contradiction is detected."
  (let [grid-ref (ref (create-init-grid))
        values (grid-values grid)]
    (if (every? true? (map #(assign grid-ref (first %) (last %)) values))
      (do (println "YAY!") @grid-ref)
      (do (println "OH CRAP") false))))

(print (parse-grid *test-grid*))

