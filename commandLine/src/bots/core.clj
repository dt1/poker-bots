(ns bots.core
  (:require [clojure.set :as cset])
  (:use [bots.gameSetup]
        [bots.botStrat]
        [bots.handStrength]))

(declare play-game)
(declare player-action)

(defn bt-helper [player-kickers bot-kickers]
  (cond (true? (first (map #(apply = %&) (sort player-kickers) (sort bot-kickers))))
        (chop-pot)

        (true? (first (map #(apply > %&) (first (sort player-kickers)) (first (sort bot-kickers)))))
        (pay-winner :human)

        (true? (first (map #(apply < %&) (first (sort player-kickers)) (first (sort bot-kickers)))))
        (pay-winner :bot)))

(defn break-tie [player-strength bot-strength]
  (let [p-vals (first player-strength)
        b-vals (first bot-strength)]
    (println p-vals)
    (println b-vals)
    (cond (true? (first (map #(apply = %&) (sort (first p-vals)) (sort (first b-vals)))))
          (bt-helper (rest p-vals) (rest b-vals))
          
          (true? (first (map #(apply > %&) (first (sort p-vals)) (first (sort b-vals)))))
          (pay-winner :human)
          
          (true? (first (map #(apply < %&) (first p-vals) (first b-vals))))
          (pay-winner :bot))))

(defn compare-hand-strength []
  (let [player-strength (get-hand-strength (player-final-hand))
        bot-strength (get-hand-strength (bot-final-hand))]
    (cond (> (index-of strengths (last player-strength)) 
             (index-of strengths (last bot-strength)))
          (pay-winner :human)

          (< (index-of strengths (last player-strength)) 
             (index-of strengths (last bot-strength)))
          (pay-winner :bot)
          
          (= (index-of strengths (last player-strength)) 
             (index-of strengths(last bot-strength)))
          (break-tie player-strength bot-strength))))

;; player actions
(defn check []
  (update-checks)
  (switch-player)
  (player-action))

(defn call []
  (update-pot 1)
  (update-player-stack 1)
  (update-street)
  (checks-to-zero)
  (update-facing-bet false)
  (update-call true)
  (switch-player)
  (player-action))

(defn bet []
  (update-pot 1)
  (checks-to-zero)
  (update-player-stack 1)
  (update-bets)
  (update-facing-bet true)
  (switch-player)
  (player-action))

(defn raise []
  (update-pot 2)
  (update-player-stack 2)
  (update-bets)
  (switch-player)
  (player-action))

(defn fold []
  (if (= (player-turn) :bot)
    (swap! game-state assoc :human (+ (human-stack) 
                                      (pot-size)))
    (swap! game-state assoc :bot (+ (bot-stack) 
                                    (pot-size))))
  (play-game))

(defn bot-check-bet []
  (if (= (bot-moves) 1)
    (do (println "Lizzie Checks") 
        (check))
    (do (println "Lizzie Bets") 
        (bet))))

(defn bot-raise-call []
  (if (= (bot-moves) 0) 
    (do (println "Lizzie Calls") 
        (call))
    (do (println "Lizzie Raises") 
        (raise))))

(defn raise-call-fold []
  (if (= (player-turn) :bot)
    (bot-raise-call))
  (println "Call, Raise, or Fold")
  (let [action (read-line)]
    (cond (= action "c")
          (call)
          (or (= action "b")
              (= action "r"))
          (raise)
          (= action "f")
          (fold)
          :else
          (raise-call-fold))))

(defn check-bet []
  (if (= (player-turn) :bot)
    (bot-check-bet))
  (println "Check or Bet")
  (let [action (read-line)]
    (cond (= action "c")
          (check)
          (= action "b")
          (bet)
          :else
          (check-bet))))

(defn call-fold []
  (if (= (player-turn) :bot)
    (do (println "Lizzie Calls")
        (call)))
  (println "Call or Fold")
  (let [action (read-line)]
    (cond (= action "c")
          (call)
          (= action "f")
          (fold)
          :else 
          (call-fold))))

;; preflop setup
(defn post-sb []
  (swap! game-state assoc (sb-player) (- ((sb-player) @game-state) 
                                         (:small-blind blinds))))

(defn post-bb []
  (swap! game-state assoc (bb-player) (- ((bb-player) @game-state) 
                                         (:big-blind blinds))))

(defn post-blinds []
  (println (bb-player) "posts Big Blind")
  (println (sb-player) "posts Small Blind")
  (post-sb) 
  (post-bb)
  (swap! game-state assoc :pot (+ (:big-blind blinds) (:small-blind blinds))))

;; small-blind moves
(defn pf-call []
  (update-pot 0.5)
  (update-player-stack 0.5)
  (update-checks)
  (switch-player)
  (player-action))

(defn pf-raise []
  (update-pot 1.5)
  (update-player-stack 1.5)
  (switch-player)
  (update-facing-bet true)
  (player-action))

(defn sb-action []
  (if (= (sb-player) :bot)
    (if (= (bot-moves) 0) 
      (do (println "Lizzie Calls") 
          (pf-call))
      (do (println "Lizzie Raises") 
          (pf-raise))))
  (println "Call, Raise, or Fold")
  (let [action (read-line)]
    (cond (= action "c")
          (pf-call)
          (or (= action "b")
              (= action "r"))
          (pf-raise)
          (= action "f")
          (fold)
          :else 
          (sb-action))))

(defn format-cards [cards]
  (map
    (fn [c] (apply format "%s%s" c))
    cards))


(defn get-board []
  (cond (= (street) :flop)
        (println "flop" (format-cards@flop))

        (= (street) :turn)
        (println "turn" (format-cards (concat @flop @turn)))
        
        (or (= (street) :river)
            (= (street) :showdown))
        (println "river" (format-cards (concat @flop @turn @river)))
        
        :else (println "preflop")))

(defn print-hands []
  (println "Lizzie: " (bot-stack))
  (println "You: " (human-stack))
  (println)
  (println "Pot: " (pot-size))
  (println)
  (println (street))
  (println (get-board))
  (println)
  (println @player-hand)
  (println))


(defn display-info []
  (println "pot" (str "$" (pot-size)))
  (println "bot-stack" (str "$" (bot-stack)))
  (println "player-stack" (str "$" (human-stack)))
  (println "")
  (println "Street" (str (street)))
  (get-board)
  (println (format-cards @player-hand))
  (println ""))

(defn showdown []
  (println "Lizzie Shows: " (format-cards @bot-hand))
  (println "Lizzie Shows" (bot-final-hand))
  (println "for" (get-hand-strength (bot-final-hand)))
  (println "Hero Shows" (player-final-hand))
  (println "for" (get-hand-strength (player-final-hand)))
  (println (compare-hand-strength))
  (play-game))

(defn player-action []
  (display-info)

  (cond (= (street) :showdown)
        (showdown)
        
        (and (= (street) :pre-flop)
             (false? (facing-bet?))
             (= (checks) 0))
        (sb-action)

        (and (= (street) :pre-flop)
             (false? (facing-bet?))
             (= (checks) 1))
        (check-bet)
        
        (false? (facing-bet?))
        (if (= (checks) max-checks)
          (do (update-street)
              (display-info)
              (check-bet))
          (check-bet))

        (true? (facing-bet?))
        (if (< (bets) max-bets)
          (raise-call-fold)
          (call-fold))
        
        (= (checks) max-checks)
        (do (update-street)
            (update-facing-bet false)
            (display-info)
            (check-bet)))

  (get-board))

(defn play-game []
  (reset-deck)
  (switch-pos)
  (reset! player-hand (create-hands))
  (reset! flop (deal-flop))
  (reset! turn (deal-turn))
  (reset! river (deal-river))
  (reset! bot-hand (create-hands))
  (println "Lizzie: " (:bot @game-state))
  (println "You: " (:human @game-state))
  (println @player-hand "\n")
  (reset-street)
  (pot-to-zero)
  (post-blinds)
  (player-action))

(defn -main []
  (println "Welcome to Limit Heads Up Poker -vs- Lizzie")
  (play-game))
