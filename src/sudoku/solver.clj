(ns sudoku.solver
  (:use [clojure.contrib.logging :only [spy info]]
        [clojure.contrib.string :only [join]]
        [sudoku :only [in?]]
        [sudoku.const :only [peers units *squares* *digits* *rows* *cols*]]))

(def *test-grid* "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......")

(def *solving* (ref false))

(declare assign)
(declare eliminate)
(declare render-grid)

(defn- trace [msg]
  (if (true? @*solving*) (println msg)))

(defn- create-init-grid []
  (zipmap *squares* (repeat (count *squares*) *digits*)))

(defn- grid-values [grid]
  "convert grid string into a dict of {square:char} with '.' representing empty cell"
  (zipmap *squares* grid))

(defn- eliminated [digits digit]
  "returns digits with digit eliminated"
  (clojure.string/replace-first digits digit ""))

(defn- try-eliminate-value-from-peers
  [grid-ref square digits]
  "If a square has only one possible value,
  then eliminate value from the square's peers.
  return false if there's a contradiction"
  (trace (str "try-eliminate-value-from-peers:" square " " digits))
  (cond
    (empty? digits) false ;; the last digit was removed
    (= 1 (count digits))
      ; if not all(eliminate(values, s2, d2) for s2 in peers[s]
      (every? true? (map #(eliminate grid-ref % (first digits)) (peers square)))
    :else true))

(defn- try-put-value-in-units
  [grid-ref units digit]
  "If a unit has only one possible place for a digit,
  then put the value there"
  (trace (str "try-put-value-in-units" units " " digit))
  (every? true?
    (for [unit units]
      (let [digit-places (filter #(in? (get @grid-ref %) digit) unit)]
        (trace (str "places for " digit " in unit: " (apply str unit) "=" (apply str digit-places)))
        (cond (empty? digit-places) false ;; contradiction: no place for this value
              (= 1 (count digit-places))
                (assign grid-ref (first digit-places) digit)
              :else true)))))

(defn- eliminate [grid-ref square digit]
  "Eliminate digit from @grid-ref[square]; propagate when values or places <= 2.
  Return false if contradiction is detected." 
  (let [digits (get @grid-ref square)]
    ;(println "digits: " digits ", digit: " digit ", " (in? digits digit))
    (if (not (in? digits digit)) (do (trace (str ">>> " square "-=" digit "; skip")) true)
      (let [new-digits (eliminated digits digit)]
        (trace (str ">>> " square "-=" digit))
        (dosync (alter grid-ref #(assoc % square new-digits)))
        ;(print (render-grid @grid-ref))
        (if (not (try-eliminate-value-from-peers grid-ref square new-digits)) false
          (try-put-value-in-units grid-ref (units square) digit))))))

(defn- assign [grid-ref square digit]
  "Eliminate all the other values (except d) from grid[square]
  and propagate. Return false if a contradiction is detected"
  (trace (str ">>> ASSIGN: " square "<-" digit ))
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

(defn- centre [content width]
  (if (>= (count content) width)
    content
    (let [num-spaces (- width (count content))
          spaces-before (int (/ num-spaces 2))
          spaces-after (- num-spaces spaces-before)]
      (str 
        (apply str (repeat spaces-before " ")) 
        content 
        (apply str (repeat spaces-after " "))))))

(defn- render-grid [grid]
  (let [width (+ 1 (apply max (map #(count (get grid %)) *squares*)))
        line (clojure.string/join "+" (repeat 3 (apply str (repeat (* width 3) \-))))]
    (apply str (map 
      (fn [row] 
        (str
          (apply 
            str 
            (apply str (map 
              (fn [col] 
                (str 
                  (centre (get grid (str row col)) width)
                  (if (contains? #{\3 \6} col) "|" "")))
              *cols*)))
          "\n"
          (if (contains? #{\C \F} row) (str line "\n") "")))
      *rows*))))

(defn- find-next-square [grid-ref]
  (let [candidates (filter #(< 1 (first %)) (map (fn [square] (let [candidates (get @grid-ref square) cnt (count candidates)] [cnt square])) *squares*))]
    (last (apply min-key #(first %) candidates))))

(defn- copy-map [a-map]
  (apply hash-map (interleave (keys a-map) (vals a-map))))

(defn- search [grid-ref]
  (print (render-grid @grid-ref))
  (cond (false? @grid-ref) false
        (every? true? (map #(= 1 (count (get @grid-ref %))) *squares*)) grid-ref ;; solved!
        :else
          ;; choose the unfilled square s with the fewest possibilities
          (let [s (find-next-square grid-ref)]
            (println ">>> next: " s ":" (get @grid-ref s))
            (let [success (some true? (map #(let [new-grid-ref (ref (copy-map @grid-ref))] (if (assign new-grid-ref s %) (search new-grid-ref) false)) (get @grid-ref s)))]
              (if (true? success) @grid-ref false)))))

(defn solve [grid-str]
  (let [grid-ref (parse-grid grid-str)]
    (dosync (ref-set *solving* true))
    (search grid-ref)))

;(println (render-grid @(parse-grid *test-grid*)))

(try
  (let [grid (solve *test-grid*)]
    (print grid)
    (println (render-grid grid)))
  (catch Exception ex (println "foobar:" (-> ex .printStackTrace))))

