(ns infinitexo.core
  (:require
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rd]))

(enable-console-print!)

;; ---------------------------------------------------------------------------
;; Rules

(def win-length 5)

;; The grid always shows at least this area, and keeps `margin` empty cells
;; around the outermost marks, so playing near an edge grows the board.
(def base-area {:min-x 0 :min-y 0 :max-x 9 :max-y 9})
(def margin 2)

(def players
  {:x {:name "X" :next :o}
   :o {:name "O" :next :x}})

(def directions [[1 0] [0 1] [1 1] [1 -1]])

(defn- run-from
  "Cells owned by `player` going from `pos` (exclusive) in direction [dx dy]."
  [cells [x y] player [dx dy]]
  (->> (iterate (fn [[cx cy]] [(+ cx dx) (+ cy dy)]) [(+ x dx) (+ y dy)])
       (take-while #(= player (get cells %)))))

(defn winning-line
  "Returns the cells of a line of at least `win-length` through `pos`, or nil."
  [cells pos]
  (let [player (get cells pos)]
    (some (fn [[dx dy]]
            (let [line (concat (reverse (run-from cells pos player [(- dx) (- dy)]))
                               [pos]
                               (run-from cells pos player [dx dy]))]
              (when (>= (count line) win-length)
                (vec line))))
          directions)))

(defn bounds
  "The visible area: the base area plus `margin` cells around every mark."
  [cells]
  (reduce (fn [b [x y]]
            {:min-x (min (:min-x b) (- x margin))
             :min-y (min (:min-y b) (- y margin))
             :max-x (max (:max-x b) (+ x margin))
             :max-y (max (:max-y b) (+ y margin))})
          base-area
          (keys cells)))

;; ---------------------------------------------------------------------------
;; State

(defn new-round [starting-player]
  {:cells {}
   :turn starting-player
   :starter starting-player
   :winner nil
   :last-move nil})

(defonce app-state
  (atom (merge (new-round :x)
               {:scores {:x 0 :o 0}})))

(defn play [state pos]
  (if (or (:winner state) (contains? (:cells state) pos))
    state
    (let [player (:turn state)
          cells (assoc (:cells state) pos player)
          line (winning-line cells pos)]
      (cond-> (assoc state
                     :cells cells
                     :last-move pos
                     :turn (get-in players [player :next]))
        line (-> (assoc :winner {:player player :line line})
                 (update-in [:scores player] inc))))))

(defn next-round [state]
  ;; Players take turns starting, so neither always has the first move.
  (merge state (new-round (get-in players [(:starter state) :next]))))

(defn reset-scores [state]
  (merge state (new-round :x) {:scores {:x 0 :o 0}}))

;; ---------------------------------------------------------------------------
;; View

(defn cross [x y]
  [:g {:class "mark mark--x"
       :transform (str "translate(" (+ x 0.22) "," (+ y 0.22) ") scale(0.56)")}
   [:line {:x1 0 :y1 0 :x2 1 :y2 1}]
   [:line {:x1 1 :y1 0 :x2 0 :y2 1}]])

(defn nought [x y]
  [:circle {:class "mark mark--o" :r 0.3 :cx (+ x 0.5) :cy (+ y 0.5)}])

(defn cell [x y owner {:keys [last? winning? playable?]}]
  [:g
   [:rect {:class (str "cell"
                       (when playable? " cell--playable")
                       (when last? " cell--last")
                       (when winning? " cell--win"))
           :x (+ x 0.04) :y (+ y 0.04) :width 0.92 :height 0.92 :rx 0.12
           :on-click (when playable? #(swap! app-state play [x y]))}]
   (case owner
     :x [cross x y]
     :o [nought x y]
     nil)])

(defn win-stroke [line]
  (let [[x1 y1] (first line)
        [x2 y2] (last line)]
    [:line {:class "win-line"
            :x1 (+ x1 0.5) :y1 (+ y1 0.5)
            :x2 (+ x2 0.5) :y2 (+ y2 0.5)}]))

(defn board []
  (let [{:keys [cells winner last-move]} @app-state
        {:keys [min-x min-y max-x max-y]} (bounds cells)
        width (inc (- max-x min-x))
        height (inc (- max-y min-y))
        winning (set (:line winner))]
    [:svg.board {:view-box (str min-x " " min-y " " width " " height)
                 :role "grid"
                 :aria-label "Game board"}
     (for [y (range min-y (inc max-y))
           x (range min-x (inc max-x))
           :let [pos [x y]
                 owner (get cells pos)]]
       ^{:key (str x "," y)}
       [cell x y owner {:last? (= pos last-move)
                        :winning? (contains? winning pos)
                        :playable? (and (nil? owner) (nil? winner))}])
     (when winner
       [win-stroke (:line winner)])]))

(defn symbol-badge [player]
  [:span {:class (str "badge badge--" (name player))} (get-in players [player :name])])

(defn status []
  (let [{:keys [turn winner]} @app-state]
    [:p.status
     (if winner
       [:<> [symbol-badge (:player winner)] " wins with five in a row!"]
       [:<> [symbol-badge turn] "'s turn"])]))

(defn scoreboard []
  (let [{:keys [scores]} @app-state]
    [:div.scores
     [:span [symbol-badge :x] " " (:x scores)]
     [:span.scores__sep "–"]
     [:span (:o scores) " " [symbol-badge :o]]]))

(defn infinitexo []
  (let [{:keys [winner]} @app-state]
    [:main.game
     [:h1 "Infinite XO"]
     [:p.rules "Get " win-length " in a row. The board grows as you play near its edges."]
     [scoreboard]
     [status]
     [board]
     [:div.actions
      [:button.btn.btn--primary {:on-click #(swap! app-state next-round)}
       (if winner "Play again" "New game")]
      [:button.btn {:on-click #(swap! app-state reset-scores)} "Reset scores"]]]))

(defn mount []
  (rd/render [infinitexo] (.getElementById js/document "app")))

(mount)

(defn on-js-reload []
  (mount))
