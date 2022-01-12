(ns infinitexo.core
  (:require
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rd]))

(enable-console-print!)

;;(println "This text is printed from src/infinitexo/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(defn new-board [size]
  (vec (repeat size (vec (repeat size "B")))))


(def app-state (atom {:text "Infinite XO"
                      :board (new-board 10)
                      :current-player -1
                      :current-state "B"}))

(defn computer-move []
  (swap! app-state assoc-in [:board 0 0] "C"))

(defn change-state [app-atom]
  (swap! app-atom update-in [:current-state] inc))

(def o [:svg
        {:view-box "0 0 30 30"
         :width 500
         :height 500}
        [:circle {:r 30 :cx 30 :cy 30}]])


(def active-users
  [{:name "Player X"
    :symbol "X"
    :total-wins 0}
   {:name "Player O"
    :symbol "O"
    :total-wins 0}
   #_{:name "Player 3"
      :symbol "I"
      :total-wins 0}])

(defn new-game-click [e]
  (swap! app-state assoc :board (new-board 10)))

(defn display-winner [symbol]
  (let [winning-player (->> active-users
                            (filter #(= (:symbol %) symbol))
                            (first)
                            (:name))]
    (when (not (nil? winning-player)) (when (js/confirm (str winning-player " has won. Do you want to restart?"))
                                        (new-game-click nil)))))


  (defn check-solution [i j]
    (let [board (get-in @app-state [:board])
          valids-nr 5
          valid? (fn [arr] (let
                            [groups (partition-by identity arr)
                             filtered-groups (filter #(and (>= (count %) valids-nr)
                                                           (not= (first %) "B")) groups)]
                             (if (> (count filtered-groups) 0) (first (first filtered-groups)) nil)))
          horrizontal (vec (for [x (map #(+ (- i (dec valids-nr)) %) (range (dec (* 2 valids-nr))))
                                 :when (<= 0 x (dec (count board)))]
                             (get-in board [j x])))
          vertical (vec (for [x (map #(+ (- j (dec valids-nr)) %) (range (dec (* 2 valids-nr))))
                              :when (<= 0 x (dec (count board)))]
                          (get-in board [x i])))
          left-diagonal (vec (for [x (map #(+ (- (dec valids-nr)) %) (range (dec (* 2 valids-nr))))
                                   :when (and (<= 0 (+ x j) (dec (count board)))
                                              (<= 0 (+ x i) (dec (count board))))]
                               (get-in board [(+ j x) (+ i x)])))
          right-diagonal (vec (for [x (map #(+ (- (dec valids-nr)) %) (range (dec (* 2 valids-nr))))
                                    :when (and (<= 0 (- j x) (dec (count board)))
                                               (<= 0 (+ i x) (dec (count board))))]
                                (get-in board [(- j x) (+ i x)])))]
      (->> [horrizontal vertical left-diagonal right-diagonal]
           (map valid?)
           (remove nil?)
           (first)
           (display-winner))))


(defn blank [i j]
  [:rect {:width 0.95
          :height 0.95
          :fill "gray"
          :x i
          :y j
          :on-click (fn rect-click []
                      (swap! app-state update-in [:current-player] #(mod (inc %) (count active-users)))
                      (swap! app-state assoc-in [:current-state] (get-in active-users [(get-in @app-state [:current-player]) :symbol]))
                      (swap! app-state assoc-in [:board j i] (get-in @app-state [:current-state]))
                      (check-solution i j))}])


(defn cross [i j]
  [:g  {:stroke "black" :stroke-width 0.1
        :stroke-linecap "round"
        :transform
        (str "translate(" (+ i 0.2) "," (+ 0.2 j) ") 
                    scale(0.6)")}
   [:line {:x1 0 :y1 0 :x2 1 :y2 1}]
   [:line {:x1 1 :y1 0 :x2 0 :y2 1}]])

(defn line [i j]
  [:g  {:stroke "black" :stroke-width 0.1
        :stroke-linecap "round"
        :transform
        (str "translate(" (+ i 0.2) "," (+ 0.2 j) ") 
                    scale(0.6)")}
   [:line {:x1 0 :y1 0 :x2 1 :y2 1}]
   #_[:line {:x1 1 :y1 0 :x2 0 :y2 1}]])


(defn circle [i j]
  [:circle {:r 0.45
            :fill "red"
            :cx (+ i 0.5)
            :cy (+ j 0.5)}])




(defn infinitexo []
  [:center
   [:h1 (:text @app-state)]
   (let [n 10]
     [:svg
      {:view-box [0 0 n n]
       :width 400
       :height 400}
      (for [i (range n)
            j (range n)] ;;[0 1 2 3 4] [0 1 2 3 4]
        ^{:key (+ (* i n) j)}
        (case (get-in @app-state [:board j i])
          "B" [blank i j]
          "X" [cross i j]
          "I" [line i j]
          "O" [circle i j]))])
   [:p [:button {:on-click new-game-click} "New Game"]]])

(rd/render [infinitexo]
           (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
