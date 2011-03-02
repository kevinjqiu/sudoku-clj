(ns sudoku.solver
  (:use [sudoku.util :only [in? copy-map first-not-falsy-elem]]
        [sudoku.grid :only [peers units *squares* *digits* *rows* *cols* render-grid create-init-grid grid-values]]))


(declare assign eliminate)


(defn- eliminated [digits digit]
  "returns digits with digit eliminated"
  (clojure.string/replace-first digits digit ""))

(defn- try-eliminate-digit-from-peers
  [grid-ref square digits]
  "If a square has only one possible value,
  then eliminate value from the square's peers.
  return false if there's a contradiction"
  (cond
    (empty? digits) false ;; the last digit was removed
    (= 1 (count digits))
      ; if not all(eliminate(values, s2, d2) for s2 in peers[s]
      (every? true? (map #(eliminate grid-ref % (first digits)) (peers square)))
    :else true))

(defn- try-put-digit-in-units
  [grid-ref units digit]
  "If a unit has only one possible place for a digit,
  then put the value there"
  (every? true?
    (for [unit units]
      (let [digit-places (filter #(in? (get @grid-ref %) digit) unit)]
        (cond (empty? digit-places) false ;; contradiction: no place for this value
              (= 1 (count digit-places))  ;; if there's only one place for the digit, assign it!
                (assign grid-ref (first digit-places) digit)
              :else true)))))

(defn- eliminate [grid-ref square digit]
  "Eliminate digit from @grid-ref[square]; propagate when values or places <= 2.
  Return false if contradiction is detected." 
  (let [digits (get @grid-ref square)]
    (if (not (in? digits digit)) true
      (let [new-digits (eliminated digits digit)]
        (dosync (alter grid-ref #(assoc % square new-digits)))
        (if (not (try-eliminate-digit-from-peers grid-ref square new-digits)) false
          (try-put-digit-in-units grid-ref (units square) digit))))))

(defn- assign [grid-ref square digit]
  "Eliminate all the other values (except d) from grid[square]
  and propagate. Return false if a contradiction is detected"
  (let [digits-to-eliminate (eliminated (get @grid-ref square) digit)]
    (every? true? (map #(eliminate grid-ref square %) digits-to-eliminate))))

(defn- parse-grid [grid-str]
  "convert grid-str to a map of possible values,
  in the form of {square: digits},
  or False if a contradiction is detected."
  (let [grid-ref (ref (create-init-grid))
        grid-values (grid-values grid-str)]
    ;; if digit is not in *digits*, it's a blank cell
    (if (every? true? (map #(or (not (in? *digits* (last %))) (assign grid-ref (first %) (last %))) grid-values))
      grid-ref
      false)))


(defn- find-next-square [grid-ref]
  (let [candidates (filter #(< 1 (first %)) (map (fn [square] (let [candidates (get @grid-ref square) cnt (count candidates)] [cnt square])) *squares*))]
    (last (apply min-key #(first %) candidates))))


(defn- search [grid-ref]
  (cond (false? @grid-ref) false
        (every? true? (map #(= 1 (count (get @grid-ref %))) *squares*)) grid-ref ;; solved!
        :else
          ;; choose the unfilled square s with the fewest possibilities
          (let [s (find-next-square grid-ref)]
            (let [success (first-not-falsy-elem
                            (map 
                              #(let [new-grid-ref (ref (copy-map @grid-ref))] 
                                 (if (assign new-grid-ref s %) 
                                   (search new-grid-ref)
                                   false)) 
                              (get @grid-ref s)))]
              (if ((comp not false?) success) success false)))))

(defn solve [grid-str]
  (let [grid-ref (parse-grid grid-str)]
    (search grid-ref)))

(def *test-grid* "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......")
(let [grid (solve *test-grid*)]
  (println (render-grid @grid)))

