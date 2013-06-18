(ns bots.gameSetup
  (:require [clojure.set :as cset]))

(defn index-of [x coll]
  (.indexOf x coll))

(def suits ["h" "c" "s" "d"])
(def ranks ["_" "2" "3" "4" "5" "6" "7" "8" "9" "T" "J" "Q" "K" "A"])

(def deck (set (for [x ranks
                     y suits
                     :when (not= x "_")]
                 [x y])))

(def players [:bot :human])

(def blinds {:small-blind 1 :big-blind 2})

(def streets {:pre-flop 2 :flop 2 :turn 4 :river 4 :showdown 0})
(def street-order [:pre-flop :flop :turn :river :showdown])

(def max-bets 8)

(def strengths [
                :high-card 
                :pair 
                :two-pair 
                :trips 
                :straight 
                :is-flush 
                :full-house 
                :quads 
                :straight-flush])

(def max-checks 2)
(def max-bets 8)

(def game-state (atom {:human 100000
                       :bot 100000
                       :pot 0
                       :bets 0
                       :checks 0
                       :player-turn :human
                       :street :flop
                       :facing-bet? false
                       :call? false}))

(defn human-stack []
  (:human @game-state))

(defn bot-stack []
  (:bot @game-state))

(defn pot-size []
  (:pot @game-state))

(defn bets []
  (:bets @game-state))

(defn checks []
  (:checks @game-state))

(defn player-turn []
  (:player-turn @game-state))

(defn street []
  (:street @game-state))

(defn facing-bet? []
  (:facing-bet? @game-state))

(defn player-call? []
  (:call? @game-state))

(defn switch-player []
  (if (= (@game-state :player-turn) :human)
    (swap! game-state assoc :player-turn :bot)
    (swap! game-state assoc :player-turn :human)))

(defn update-facing-bet [bool]
  (swap! game-state assoc :facing-bet? bool))

(defn update-call [bool]
  (swap! game-state assoc :call? bool))

(defn update-pot [amt]
  (swap! game-state assoc :pot (+ (pot-size) 
                                  (* amt 
                                     ((street) streets)))))

(defn pot-to-zero []
  (swap! game-state assoc :pot 0))

(defn checks-to-zero []
  (swap! game-state assoc :checks 0))

(defn update-player-stack [amt]
  (swap! game-state assoc (player-turn) (- ((player-turn) @game-state) 
                                           (* amt 
                                              ((street) streets)))))

(defn update-street []
  (swap! game-state assoc :street (nth street-order (+ (index-of street-order (street)) 
                                                       1))))

(defn reset-street []
  (swap! game-state assoc :street :pre-flop))

(defn update-checks []
  (swap! game-state assoc :checks (inc (checks))))

(defn update-bets []
  (swap! game-state assoc :bets (inc (bets))))

(defn get-stack [player]
  (if (= player :human)
    (human-stack)
    (bot-stack)))

(defn pay-winner [player]
  (swap! game-state assoc player (+ (get-stack player)
                                    (pot-size))))

(defn chop-pot []
  ((swap! game-state assoc :human (+ (human-stack) 
                                     (/ (pot-size) 2))) 
   (swap! game-state assoc :bot (+ (bot-stack) 
                                   (/ (pot-size) 2)))))

(def stub (atom deck))

(defn update-stub [card]
  (swap! stub cset/difference #{card}))

(defn reset-deck []
  (reset! stub deck))

(defn deal-card []
  (let [card (rand-nth (seq @stub))]
    (update-stub card)
    card))

(defn deal-flop []
  [(deal-card) (deal-card) (deal-card)])

(defn deal-turn []
  [(deal-card)])

(defn deal-river []
  [(deal-card)])

;; board values
(def flop (atom (deal-flop)))

(def turn (atom (deal-turn)))

(def river (atom (deal-river)))

(defn create-hands []
  [(deal-card) (deal-card)])

(def player-hand (atom (create-hands)))

(def bot-hand (atom (create-hands)))

(defn player-final-hand []
  (concat @player-hand @flop @turn @river))

(defn bot-final-hand []
  (concat @bot-hand @flop @turn @river))

(def position (atom {:button :human
                     :oop :bot}))

(defn switch-pos []
  (let [new-button (:oop @position)
        new-oop (:button @position)]
    (swap! position assoc :oop new-oop)
    (swap! position assoc :button new-button)))

(defn sb-player []
  (:button @position))

(defn bb-player []
  (:oop @position))
