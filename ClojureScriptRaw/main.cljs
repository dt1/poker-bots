(ns cex.main
  (:require [clojure.set :as cset]
            [goog.events :as event]
            [goog.dom :as gdom]))


(declare play-game)
(declare player-action)

;; ClojureScript setup
 
(defn get-element [id]
  (gdom/getElement (name id)))

(defn inner-html [htm val]
  (let [a (get-element htm)]
    (set! (.-innerHTML a)
          val)))

(defn remove-children [id]
  (let [parent (gdom/getElement (name id))]
    (do (gdom/removeChildren parent))))

(defn index-of [coll x]
  (first (for [[idx y] (map-indexed vector coll)
               :when (= y x)] idx)))

(defn create-button [val]
  (let [new-button (.createElement js/document "input")]
    (set! (.-type new-button)
          "button")
    (set! (.-value new-button)
          val)
    (set! (.-id new-button)
          val)
    new-button))

(defn create-card [val]
  (let [[rank suit] val]
    (let [card-span (.createElement js/document "span")]
      (set! (.-style.border card-span)
            "1px solid black")
      (set! (.-style.margin card-span)
            "0px 2px 0px 2px")
      (cond (= suit "S")
            (do (set! (.-innerHTML card-span)
                      (str rank "&#9824;"))
                (set! (.-style.color card-span)
                      "black"))
            (= suit "H")
            (do (set! (.-innerHTML card-span)
                      (str rank "&#9829;"))
                (set! (.-style.color card-span)
                      "red"))
            (= suit "D")
            (do (set! (.-innerHTML card-span)
                      (str rank "&#9830;"))
                (set! (.-style.color card-span)
                      "blue"))
            (= suit "C")
            (do (set! (.-innerHTML card-span)
                      (str rank "&#9827;"))
                (set! (.-style.color card-span)
                      "green")))
      card-span)))

(defn show-cards [elem cards]
  (remove-children elem)
  (doseq [x cards]
    (.appendChild (get-element elem)
                  (create-card x))))

(defn action-buttons [coll]
  (doseq [x coll]
    (.appendChild (get-element "button-field") 
                  (create-button x))))

(def cb ["check" "bet"])
(def crf ["call" "raise" "fold"])
(def cf ["call" "fold"])

(defn cb-buttons []
  (remove-children "button-field")
  (action-buttons cb))

(defn crf-buttons []
  (remove-children "button-field")
  (action-buttons crf))

(defn cf-buttons []
  (remove-children "button-field")
  (action-buttons cf))

(defn bot-moves []
  (rand-int 2))

(def suits ["H" "C" "S" "D"])
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
    (do (swap! game-state assoc :player-turn :human)
        (set! (.-border "hero-info")
              "3px solid blue"))))

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

(defn rank-count [hand-ranks qty]
  (for [[kk vv] hand-ranks
        :when (= vv qty)]
    kk))

(defn rank-frequency [hand]
  (let [r (for [[x y] hand] x)]
    (frequencies r)))

(defn suit-frequency [hand]
  (let [s (for [[x y] hand] y)]
    (frequencies s)))

(defn get-kickers [k-cards to-take]
  (take-last to-take
             (sort
              (map #(index-of ranks %) k-cards))))

(defn high-card [hand]
  (let [h-hand (rank-count (rank-frequency hand) 1)]
    (let [h2 (sort (map #(index-of ranks %) h-hand))]
      (conj
       (take-last 1 h2)
       (take 4
             (get-kickers h-hand 5))))))

(defn pair [hand]
  (let [p-card (rank-count (rank-frequency hand) 2)
        k-cards (rank-count (rank-frequency hand) 1)]
    (if (not (empty? p-card))
      (conj (map #(index-of ranks %) p-card)
            (get-kickers k-cards 3)))))

(defn two-pair [hand]
  (let [p-cards (rank-count (rank-frequency hand) 2)
        k-cards (rank-count (rank-frequency hand) 1)]
    (if (not (empty? p-cards))
      (cond (= (count p-cards) 2)
            (conj (map #(index-of ranks %) p-cards)
                  (get-kickers k-cards 1))
            (= (count p-cards) 3)
            (conj (take-last 2 (sort (map #(index-of ranks %) p-cards)))
                  (let [kicker (get-kickers k-cards 1)
                        other-kicker (take 1 (sort (map #(index-of ranks %) p-cards)))]
                    (if (>= (first kicker) (first other-kicker))
                      kicker
                      other-kicker)))))))

(defn trips [hand]
  (let [t-cards (rank-count (rank-frequency hand) 3)
        k-cards (concat (rank-count (rank-frequency hand) 1)
                        (rank-count (rank-frequency hand) 2))]
    (if (not (empty? t-cards))
      (conj (map #(index-of ranks %) t-cards)
            (get-kickers k-cards 2)))))

(defn quads [hand]
  (let [q-cards (rank-count (rank-frequency hand) 4)
        k-cards (rank-count (rank-frequency hand) 1)]
    (if (not (empty? q-cards))
      (conj (map #(index-of ranks %) q-cards)
            (get-kickers k-cards 1)))))

(defn full-house [hand]
  (let [t-cards (rank-count (rank-frequency hand) 3)
        p-cards (rank-count (rank-frequency hand) 2)]
    (if (not (empty? t-cards))
      (cond (= (count t-cards) 2)
            (conj (take-last 1 (sort (map #(index-of ranks %) t-cards)))
                  (take 1 (sort (map #(index-of ranks %) t-cards))))
            :else
            (if (not (empty? p-cards))
              (conj (map #(index-of ranks %) t-cards)
                    (take-last 1 (sort (map #(index-of ranks %) p-cards)))))))))

(defn is-flush [hand]
  (let [a (suit-frequency hand)]
    (if (some #(<= 5 %) (vals a))
      (let [tt (first
                (for [[k v] a]
                  (if (>= v 5)
                    k)))]
        (let [ss (for [[x y] hand]
                   (if (= y tt)
                     x))]
          (sort (map #(index-of ranks %) ss)))))))

(defn straight [hand]
  (let [hf (keys (rank-frequency hand))]
    (let [t (sort (set (map #(index-of ranks %) hf)))]
      (if (and (= (take 4 t) [1 2 3 4])
               (= (last t) 13)
               (not (some #{6} t)))
        (conj (take 4 t) (last t))
        (loop [tt t]
          (when (> (count tt) 4)
            (if (= (- (last (take 5 tt)) (first (take 5 tt))) 4)
              (loop [res tt]
                (if (= (- (last (take-last 5 res)) (first (take-last 5 res))) 4)
                  (take-last 5 res)
                  (recur (take (- (count res) 1) res))))
              (recur (rest tt)))))))))


(defn sf-helper [hand suit]
  (remove nil? (for [[x y] hand]
                 (if (= y suit)
                   [x y]))))


(defn straight-flush [hand]
  (let [h-hand (sf-helper hand "H")
        d-hand (sf-helper hand "D")
        c-hand (sf-helper hand "C")
        s-hand (sf-helper hand "S")]
    (cond (>= (count h-hand) 5)
          (straight h-hand)
          (>= (count d-hand) 5)
          (straight d-hand)
          (>= (count c-hand) 5)
          (straight c-hand)
          (>= (count s-hand) 5)
          (straight s-hand))))

(defn get-hand-strength [hand]
  (let [sf-hand (straight-flush hand)
        q-hand (quads hand)
        fh-hand (full-house hand)
        f-hand (is-flush hand)
        s-hand (straight hand)
        t-hand (trips hand)
        tp-hand (two-pair hand)
        p-hand (pair hand)
        h-hand (high-card hand)]
    (cond sf-hand [sf-hand :straight-flush]
          q-hand [q-hand :quads]
          fh-hand [fh-hand :full-house]
          f-hand [f-hand :is-flush]
          s-hand [s-hand :straight]
          t-hand [t-hand :trips]
          tp-hand [tp-hand :two-pair]
          p-hand [p-hand :pair] 
          :else [h-hand :high-card])))

(defn break-tie [p-vals b-vals]
  (if (or (empty? p-vals)
          (empty? b-vals))
    (do (inner-html "dealer-talk" "Players Chop")
        (chop-pot))
    
    (cond (true? (= (first p-vals) (first b-vals)))
          (break-tie (pop p-vals)
                         (pop b-vals))

              (true? (> (first p-vals) (first b-vals)))
          (do (inner-html "dealer-talk" "You Win!")
              (pay-winner :human))
          
          (true? (< (first p-vals) (first b-vals)))
          (do (inner-html "dealer-talk" "Lizzie Wins!")
              (pay-winner :bot)))))

(defn compare-hand-strength []
  (let [player-strength (get-hand-strength (player-final-hand))
        bot-strength (get-hand-strength (bot-final-hand))]
    (let [p-index (index-of strengths (last player-strength))
          b-index (index-of strengths (last bot-strength))]
      (cond (> p-index b-index)
            (do (inner-html "dealer-talk" "You Win!") 
                (pay-winner :human))
            
            (< p-index b-index)
            (do  (inner-html "dealer-talk" "Lizzie Wins!")
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
    (pay-winner :human)
    (pay-winner :bot))
    (play-game))

(defn bot-check-bet []
  (if (= (bot-moves) 1)
    (do (inner-html "bot-action" "Lizzie Checks") 
        (check))
    (do (inner-html "bot-action" "Lizzie Bets") 
        (bet))))

(defn bot-raise-call []
  (if (= (bot-moves) 0) 
    (do (inner-html "bot-action" "Lizzie Calls") 
        (call))
    (do (inner-html "bot-action" "Lizzie Raises") 
        (raise))))

(defn crf-click-event [actions]
  (doseq [x actions]
    (event/listen (get-element x)
                  "click"
                  #(cond (= x "call")
                         (call)
                         (= x "raise")
                         (raise)
                         (= x "fold")
                         (fold)))))

(defn raise-call-fold []
  (cond (= (player-turn) :bot)
        (bot-raise-call)
        
        (= (player-turn) :human)
        (do (crf-buttons)
            (crf-click-event crf))))

(defn cb-click-event [actions]
  (doseq [x actions]
    (event/listen (get-element x)
                  "click"
                  #(cond (= x "check")
                         (check)
                         (= x "bet")
                         (bet)))))

(defn check-bet []
  (cond (= (player-turn) :bot)
        (bot-check-bet)
        
        (= (player-turn) :human)
        (do (cb-buttons)
            (cb-click-event cb))))

(defn cf-click-event [actions]
  (doseq [x actions]
    (event/listen (get-element x)
                  "click"
                  #(cond (= x "call")
                         (call)
                         (= x "fold")
                         (fold)))))

(defn call-fold []
  (if (= (player-turn) :bot)
    (do (inner-html "bot-action" "Lizzie Calls")
        (call)))
  (cf-buttons)
  (cf-click-event cf))

;; preflop setup
(defn post-sb []
  (swap! game-state assoc (sb-player) (- ((sb-player) @game-state) 
                                         (:small-blind blinds))))

(defn post-bb []
  (swap! game-state assoc (bb-player) (- ((bb-player) @game-state) 
                                         (:big-blind blinds))))

(defn post-blinds []
  (inner-html "dealer-talk" (str (bb-player) " posts Big Blind"))
  (inner-html "dealer-talk" (str (sb-player) " posts Small Blind"))
  (post-sb) 
  (post-bb)
  (swap! game-state assoc :pot (+ (:big-blind blinds) 
                                  (:small-blind blinds))))

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
  (update-bets)
  (player-action))

(defn sb-click-event [actions]
  (doseq [x actions]
    (event/listen (get-element x)
                  "click"
                  #(cond (= x "raise")
                         (pf-raise)
                         (= x "call")
                         (pf-call)
                         (= x "fold")
                         (fold)))))

(defn sb-action []
  (cond (= (sb-player) :bot)
        (if (= (bot-moves) 0) 
          (do (inner-html "bot-action" "Lizzie Calls") 
              (pf-call))
          (do (inner-html "bot-action" "Lizzie Raises") 
              (pf-raise)))

        (= (sb-player) :human)
        (do (crf-buttons)
            (sb-click-event crf))))

(defn get-board []
  (cond (= (street) :flop)
        (show-cards "cards" @flop)

        (= (street) :turn)
        (show-cards "cards" (concat @flop @turn))
        
        (or (= (street) :river)
            (= (street) :showdown))
        (show-cards "cards" (concat @flop @turn @river))
        
        :else (inner-html "cards" "")))

(defn display-info []
  (inner-html "pot" (str "$" (pot-size)))
  (inner-html "bot-stack" (str "$" (bot-stack)))
  (inner-html "player-stack" (str "$" (human-stack)))
  (inner-html "street" (str (street)))
  (inner-html "bot-stack" (str "$" (bot-stack)))
  (inner-html "player-stack" (str "$" (human-stack)))
  )

(defn showdown []
  (inner-html "bot-action" "")
  (remove-children "button-field")
  (inner-html "dealer-talk" (str "Lizzie Shows: " (bot-final-hand)
                                 "<br>"
                                 "For: " (get-hand-strength (bot-final-hand)) 
                                 "<br>"
                                 "Hero Shows: " (player-final-hand)
                                 "<br>"
                                 "For: " (get-hand-strength (player-final-hand))))
  (show-cards "bot-cards" @bot-hand)
  (compare-hand-strength)
  (js/setTimeout #(play-game) 500))

(defn act []
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

(defn player-action []
  (js/setTimeout #(act) 200))

(defn play-game []
  (inner-html "bot-cards" "")
  (inner-html "cards" "")
  (reset-deck)
  (switch-pos)
  (reset! player-hand (create-hands))
  (reset! flop (deal-flop))
  (reset! turn (deal-turn))
  (reset! river (deal-river))
  (reset! bot-hand (create-hands))
  (show-cards "player-cards" @player-hand)
  (reset-street)
  (pot-to-zero)
  (post-blinds)
  (player-action))

(set! (.-onload js/window) play-game)
