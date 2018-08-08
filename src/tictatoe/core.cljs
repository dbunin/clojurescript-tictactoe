(ns tictatoe.core
    (:require [reagent.core :as reagent :refer [atom]]))

(defn new-board [n]
  (vec (repeat n (vec (repeat n "B")))))

(defonce app-state
  (atom {:text "Welcome to tic tac toe"
         :board (new-board 3)
         :status :in-progress}))

(defn computer-move [board]
  (let [remaining-spots (for [i (range 3)
                              j (range 3)
                              :when (= (get-in board [j i]) "B")]
                          [j i])
        move (rand-nth remaining-spots)]
    (if move
      (assoc-in board move "C")
      board)))

(defn straight [owner board [x y] [dx dy]]
  (every? true?
          (for [i (range 3)]
            (= (get-in board [(+ (* dx i) x)
                              (+ (* dy i) y)])
               owner))))

(defn win? [owner board]
  (some true?
          (for [i (range 3)
                j (range 3)
                dir [[1 0] [0 1] [1 1] [1 -1]]]
           (straight owner board [i j] dir))))

(defn full? [board]
  (every? #{"P" "C"} (apply concat board)))

(defn game-status [board]
  (cond
    (win? "P" board) :player-victory
    (win? "C" board) :computer-victory
    (full? board) :draw
    :else :in-progress))

(defn update-status [state]
  (assoc state :status (game-status (:board state))))

(defn check-game-status [state]
  (-> state
      (update-in [:board] computer-move)
      (update-status)))

(defn blank [i j]
  [:rect
   {:width 0.9
    :height 0.9
    :fill "grey"
    :x i
    :y j
    :on-click
    (fn rect-clicked [e]
      (when (= (:status @app-state) :in-progress)
        (swap! app-state assoc-in [:board j i] "P")
        (if (win? "P" (:board @app-state))
          (swap! app-state assoc :status :player-victory)
          (swap! app-state check-game-status))))}])

(defn circle [i j]
  [:circle
   {:r 0.45
    :fill "green"
    :cx (+ 0.45 i)
    :cy (+ 0.45 j)}])

(defn cross [i j]
  [:g {:stroke "darkred"
       :stroke-width 0.35
       :stroke-linecap "round"
       :transform
       (str "translate(" (+ 0.5 i) "," (+ 0.5 j) ") "
            "scale(0.35)")}
   [:line {:x1 -1 :y1 -1 :x2 1 :y2 1}]
   [:line {:x1 1 :y1 -1 :x2 -1 :y2 1}]])

(defn tictactoe []
  [:center
   [:h1 (:text @app-state)]
   (case (:status @app-state)
     :player-victory [:h2 "You won!"]
     :computer-victory [:h2 "Computer won!"]
     :draw [:h2 "DRAW!"]
     nil)
   (into
    [:svg
      {:view-box "0 0 3 3"
      :width 500
      :height 500}]
      (for [i (range (count (:board @app-state)))
            j (range (count (:board @app-state)))]
        (case (get-in @app-state [:board j i])
          "B" [blank i j]
          "P" [circle i j]
          "C" [cross i j])))
   [:p
    [:button
     {:on-click
      (fn new-game-click [e]
        (swap! app-state assoc :board (new-board 3))
        (swap! app-state assoc :status :in-progress))}
     "New Game!"]]])

(reagent/render-component [tictactoe]
                          (. js/document (getElementById "app")))

(defn on-js-reload [])
 