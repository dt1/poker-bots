(ns bots.core
  (:require [bots.dealer :as dealer])
  (:use [bots.gameSetup]
        [bots.botStrat]
        [bots.handStrength]))

(declare play-game)
(declare player-action)

(defn break-tie [p-vals b-vals]
  (if (or (empty? p-vals)
          (empty? b-vals))
    (do (dealer/chop)
        (chop-pot))
    
    (cond (true? (= (first p-vals) (first b-vals)))
              (break-tie (pop p-vals)
                         (pop b-vals))

          (true? (> (first p-vals) (first b-vals)))
          (do (dealer/player-wins) 
              (pay-winner :human))

          (true? (< (first p-vals) (first b-vals)))
          (do (dealer/lizzy-wins) 
              (pay-winner :bot)))))

(defn compare-hand-strength []
  (let [player-strength (get-hand-strength (player-final-hand))
        bot-strength (get-hand-strength (bot-final-hand))]
    (let [p-index (index-of strengths (last player-strength))
          b-index (index-of strengths (last bot-strength))]
      (cond (> p-index b-index)
            (do (dealer/player-wins)
                (pay-winner :human))
            
            (< p-index b-index)
            (do (dealer/lizzy-wins)
                (pay-winner :bot))
          
            (= p-index b-index)
            (break-tie (reverse (flatten (first player-strength))) 
                       (reverse (flatten (first bot-strength))))))))
  
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
    (pay-winner :bot)
    (pay-winner :human))
    (play-game))

(defn bot-check-bet []
  (if (= (bot-moves) 1)
    (do (dealer/lizzie-checks) 
        (check))
    (do (dealer/lizzie-bets) 
        (bet))))

(defn bot-raise-call []
  (if (= (bot-moves) 0) 
    (do (dealer/lizzie-calls) 
        (call))
   (do (dealer/lizzie-raises)
        (raise))))

(defmulti crf :action)

(defmethod crf "c" [_]
  (call))

(defmethod crf "r" [_]
  (raise))

(defmethod crf "f" [_]
  (fold))

(defn raise-call-fold []
  (if (= (player-turn) :bot)
    (bot-raise-call))
  (dealer/call-raise-fold)
  (let [action (read-line)]
    (if (some #{action} ["c" "r" "f"])
      (crf {:action action})
      (raise-call-fold))))

(defmulti cb :action)

(defmethod cb "c" [_]
  (check))

(defmethod cb "b" [_]
  (bet))

(defn check-bet []
  (if (= (player-turn) :bot)
    (bot-check-bet))
  (dealer/check-bet)
  (let [action (read-line)]
    (if (some #{action} ["c" "b"])
      (cb {:action action})
      (check-bet))))

(defmulti cf :action)

(defmethod cf "c" [_]
  (call))

(defmethod cf "f" [_]
  (fold))

(defn call-fold []
  (if (= (player-turn) :bot)
    (do (dealer/lizzie-calls)
        (call)))
  (dealer/call-fold)
  (let [action (read-line)]
    (if (some #{action} ["c" "f"])
      (cf {:action action})
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

(defmulti sb-crf :action)

(defmethod sb-crf "c" [_]
  (update-pot 0.5)
  (update-player-stack 0.5)
  (update-checks)
  (switch-player)
  (player-action))

(defmethod sb-crf "r" [_]
  (update-pot 1.5)
  (update-player-stack 1.5)
  (switch-player)
  (update-facing-bet true)
  (player-action))

(defmethod sb-crf "f" [_]
  (fold))
  
(defn sb-action []
  (if (= (sb-player) :bot)
    (if (= (bot-moves) 0)
      (do (dealer/lizzie-calls)
          (pf-call))
      (do (dealer/lizzie-raises)
          (pf-raise))))
  (dealer/call-raise-fold)
  (let [action (read-line)]
    (if (some #{action} ["c" "r" "f"])
      (sb-crf {:action action})
          (sb-action))))

(defn format-cards [cards]
  (map
    (fn [c] (apply format "%s%s" c))
    cards))

;; display board
(defmulti display-street :street)

(defmethod display-street :flop [_]
  (println "flop" (format-cards @flop)))

(defmethod display-street :turn [_]
  (println "turn" (format-cards (concat @flop @turn))))

(defmethod display-street :river [_]
  (println "river" (format-cards (concat @flop @turn @river))))

(defmethod display-street :showdown [_]
  (println "showdown" (format-cards (concat @flop @turn @river))))

(defmethod display-street :default [_]
  (println "preflop"))

(defn get-board []
  (display-street {:street (street)}))

;; / display board

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
  (compare-hand-strength)
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
