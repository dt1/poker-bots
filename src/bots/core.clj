(ns bots.core
    (use clojure.set))

;; Forward Declarations
(declare deal-card)
(declare call)
(declare bet)
(declare raise)
(declare fold)
(declare raise-call-fold)
(declare check-bet)
(declare player-action)
(declare post-sb)
(declare post-bb)
(declare post-blinds)
(declare pf-call)
(declare pf-raise)
(declare sb-action)
(declare play-game)
(declare bot-play)
(declare check)

;; game setup
(def suits ["H" "C" "S" "D"])
(def ranks ["_" "2" "3" "4" "5" "6" "7" "8" "9" "T" "J" "Q" "K" "A"])
(def players [:bot :human])

(def blinds {:small-blind 1 :big-blind 2})

(def streets {:pre-flop 2 :flop 2 :turn 4 :river 4 :showdown 0})
(def street-order [:pre-flop :flop :turn :river :showdown])

(def max-bets 8)

(def strengths [:high-card :pair :two-pair :trips :straight :is-flush :full-house :quads :straight-flush])

(def checks 0)

;; bot strategy
(defn bot-moves []
  (rand-int 2))

;; create deck and hands
(def deck (set (for [x ranks
                     y suits
                     :when (not= x "_")]
                 [x y]))) 

;; handle state and mutation
(def game-state (atom {:human 100000
                       :bot 100000
                       :pot 0
                       :bets 0}))

(def stub (atom deck))

(defn update-stub [card]
    (swap! stub clojure.set/difference #{card}))

(defn reset-deck []
  (reset! stub deck)) 

(defn player-turn [player]
  (if (= player :human)
    :bot
    :human))

(defn deal-card []
  (let [card (rand-nth (seq @stub))]
    (update-stub card)
    card))

(defn create-hands []
  [(deal-card) (deal-card)])

(def player-hand (atom (create-hands)))
(def bot-hand (atom (create-hands)))

(def position (atom {:button :human
                     :oop :bot}))

(defn switch-pos []
  (let [new-button (:oop @position)
        new-oop (:button @position)]
    (swap! position assoc :oop new-oop)
    (swap! position assoc :button new-button)))

;; hand strength helpers
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

;; dealer functions
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

(defn get-kickers [k-cards to-take]
  (take-last to-take
             (sort
              (map #(.indexOf ranks %) k-cards))))

(defn high-card [hand]
  (let [h-hand (rank-count (rank-frequency hand)1)]
    (let [h2 (sort (map #(.indexOf ranks %) h-hand))]
      (conj
       (take-last 1 h2)
       (take 4
             (get-kickers h-hand 5))))))

(defn pair [hand]
  (let [p-card (rank-count (rank-frequency hand) 2)
        k-cards (rank-count (rank-frequency hand) 1)]
    (if (not (empty? p-card))
      (conj (map #(.indexOf ranks %) p-card)
            (get-kickers k-cards 3)))))

(defn two-pair [hand]
  (let [p-cards (rank-count (rank-frequency hand) 2)
        k-cards (rank-count (rank-frequency hand) 1)]
    (if (not (empty? p-cards))
      (cond (= (count p-cards) 2)
            (conj (map #(.indexOf ranks %) p-cards)
                  (get-kickers k-cards 1))
            (= (count p-cards) 3)
            (conj (take-last 2 (sort (map #(.indexOf ranks %) p-cards)))
                  (let [kicker (get-kickers k-cards 1)
                        other-kicker (take 1 (sort (map #(.indexOf ranks %) p-cards)))]
                  (if (>= (first kicker) (first other-kicker))
                    kicker
                    other-kicker)))))))

(defn trips [hand]
  (let [t-cards (rank-count (rank-frequency hand) 3)
        k-cards (concat (rank-count (rank-frequency hand) 1)
                        (rank-count (rank-frequency hand) 2))]
    (if (not (empty? t-cards))
      (conj (map #(.indexOf ranks %) t-cards)
            (get-kickers k-cards 2)))))

(defn quads [hand]
  (let [q-cards (rank-count (rank-frequency hand) 4)
        k-cards (rank-count (rank-frequency hand) 1)]
    (if (not (empty? q-cards))
      (conj (map #(.indexOf ranks %) q-cards)
            (get-kickers k-cards 1)))))

(defn full-house [hand]
  (let [t-cards (rank-count (rank-frequency hand) 3)
        p-cards (rank-count (rank-frequency hand) 2)]
    (if (not (empty? t-cards))
      (cond (= (count t-cards) 2)
            (conj (take-last 1 (sort (map #(.indexOf ranks %) t-cards)))
                  (take 1 (sort (map #(.indexOf ranks %) t-cards))))
            :else
            (conj (map #(.indexOf ranks %) t-cards)
                  (take-last 1 (sort (map #(.indexOf ranks %) p-cards))))))))

(defn is-flush [hand]
  (let [a (suit-frequency hand)]
    (if 
        (some #(<= 5 %) (vals a))
      (let [tt (first
                (for [[k v] a]
                  (if (>= v 5)
                    k)))]
        (let [ss (for [[x y] hand]
                   (if (= y tt)
                     x))]
          (sort (map #(.indexOf ranks %) ss)))))))

(defn straight [hand]
  (let [hf (keys (rank-frequency hand))]
    (let [t (sort (set (map #(.indexOf ranks %) hf)))]
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

(defn straight-flush [hand]
  (let [h-hand (for [[x y] hand]
                 (if (= y "H")
                   [x y]))
        d-hand (for [[x y] hand]
                 (if (= y "D")
                   [x y]))
        c-hand (for [[x y] hand]
                 (if (= y "C")
                   [x y]))
        s-hand (for [[x y] hand]
                 (if (= y "S")
                   [x y]))]
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
        p-hand (pair hand)]
    (cond  (not (nil? sf-hand)) [sf-hand :straight-flush]
           (not (nil? q-hand)) [q-hand :quads]
           (not (nil? fh-hand)) [fh-hand :full-house]
           (not (nil? f-hand)) [f-hand :is-flush]
           (not (nil? s-hand)) [s-hand :straight]
           (not (nil? t-hand)) [t-hand :trips]
           (not (nil? tp-hand)) [tp-hand :two-pair]
           (not (nil? p-hand)) [p-hand :pair])))

(defn bt-helper [player-strength bot-strength]
  (if (= player-strength bot-strength)
    ((swap! game-state assoc :human (+ (:human @game-state) (/ (:pot @game-state) 2))) 
     (swap! game-state assoc :bot (+ (:bot @game-state) (/ (:pot @game-state) 2))))
    (let [p-value (reverse player-strength)
          b-value (reverse bot-strength)]
      (loop [p-value p-value
             b-value b-value]
        (cond (> (first p-value) (first b-value))
              (swap! game-state assoc :human (+ (:human @game-state) (:pot @game-state)))
              (< (first p-value) (first b-value))
              (swap! game-state assoc :bot (+ (:bot @game-state) (:pot @game-state)))
              :else
              (recur (rest p-value) (rest b-value)))))))

(defn break-tie [player-strength bot-strength]
  (let [p-vals (first player-strength)
        b-vals (first bot-strength)]
    (cond (> (last p-vals) (last b-vals))
          (swap! game-state assoc :human (+ (:human @game-state) (:pot @game-state)))
          (< (last p-vals) (last b-vals))
          (swap! game-state assoc :bot (+ (:bot @game-state) (:pot @game-state)))
          (= (last p-vals) (last b-vals))
          (bt-helper (first p-vals) (first b-vals)))))

(defn compare-hand-strength [player-hand bot-hand]
  (let [player-strength (get-hand-strength player-hand)
        bot-strength (get-hand-strength bot-hand)]
    (cond (> (.indexOf strengths (last player-strength)) (.indexOf strengths (last bot-strength)))
          ((println "You win with: " (last player-strength))
           (swap! game-state assoc :human (+ (:human @game-state) (:pot @game-state))))
          (< (.indexOf strengths (last player-strength)) (.indexOf strengths (last bot-strength)))
          ((println "Lizzie wins with: " (last bot-strength))
           (swap! game-state assoc :bot (+ (:bot @game-state) (:pot @game-state))))     
          (= (.indexOf strengths (last player-strength)) (.indexOf strengths(last bot-strength)))
          (break-tie player-strength bot-strength))))

;; player actions
(defn check [player street checks]
  (player-action (player-turn player) street 0 false (inc checks)))

(defn call [player street]
  (swap! game-state assoc :pot (+ (:pot @game-state) (street streets)))
  (swap! game-state assoc player (- (player @game-state) (street streets)))
  (player-action (player-turn player) (nth street-order (+ (.indexOf street-order street) 1)) 0 false 0))

(defn bet [player street total-bets]
  (swap! game-state assoc :pot (+ (:pot @game-state) (street streets)))
  (swap! game-state assoc player (- (player @game-state) (street streets)))
  (player-action (player-turn player) street (inc total-bets) true 0))

(defn raise [player street total-bets]
  (swap! game-state assoc :pot (+ (:pot @game-state) (* 2 (street streets))))
  (swap! game-state assoc player (- (player @game-state) (* 2 (street streets))))
  (player-action (player-turn player) street (inc total-bets) true 0))

(defn fold [player]
  (if (= player :bot)
    (swap! game-state assoc :human (+ (:human @game-state) (:pot @game-state)))
    (swap! game-state assoc :bot (+ (:bot @game-state) (:pot @game-state))))
  (swap! game-state assoc :pot 0)
  (play-game))

(defn bot-check-bet [street total-bets checks]
  (cond (= (bot-moves) 1) ((println "Lizzie Bets") (bet :bot street total-bets))
        (= (bot-moves) 0) ((println "Lizzie Checks") (check :bot street checks))
        :else  ((println "Lizzie Bets") (bet :bot street total-bets))))

(defn bot-raise-call [street total-bets]
  (cond (= (bot-moves) 1) ((println "Lizzie Raises") (raise :bot street total-bets))
        (= (bot-moves) 0) ((println "Lizzie Calls") (call :bot street))
        :else  ((println "Lizzie Raises") (raise :bot street total-bets))))

(defn raise-call-fold [player street total-bets]
  (if (= player :bot)
    (bot-raise-call street total-bets))
  (println "Call, Raise, or Fold")
  (let [action (read-line)]
    (cond (= action "c")
          (call player street)
          (or (= action "b")
              (= action "r"))
          (raise player street total-bets)
          (= action "f")
          (fold player)
           :else
           (recur player street total-bets))))

(defn check-bet [player street total-bets checks]
  (if (= player :bot)
    (bot-check-bet street total-bets checks))
  (println "Check or Bet")
  (let [action (read-line)]
    (cond (= action "c")
          (check player street 1)
          (= action "b")
          (bet player street total-bets)
          :else
          (recur player street total-bets checks))))

(defn call-fold [player street]
  (if (= player :bot)
    ((println "Lizzie Calls")
     (call :bot street))
    ((println "Call or Fold")
     (let [action (read-line)]
       (cond (= action "c")
             (call player street)
             (= action "f")
             (fold player)
             :else (call-fold player street))))))

;; preflop setup
(defn post-sb [sb-player]
  (swap! game-state assoc sb-player (- (sb-player @game-state) (:small-blind blinds))))

(defn post-bb [bb-player]
  (swap! game-state assoc bb-player (- (bb-player @game-state) (:big-blind blinds))))

(defn post-blinds [button-player]
  (if (= button-player :bot)    
    ((post-sb :bot) (post-bb :human))
    ((post-sb :human) (post-bb :bot)))
  (swap! game-state assoc :pot (+ (:big-blind blinds) (:small-blind blinds))))

;; small-blind moves
(defn pf-call [player]
  (swap! game-state assoc player (- (player @game-state) 1))
  (swap! game-state assoc :pot (+ (:pot @game-state) 1))
  (player-action (player-turn player) :pre-flop 1 false 1))

(defn pf-raise [player]
  (swap! game-state assoc player (- (player @game-state) 3))
  (swap! game-state assoc :pot (+ (:pot @game-state) 3))
  (player-action (player-turn player) :pre-flop 2 true 0))

(defn sb-action [player]
  (if (= player :bot)
    (cond (= (bot-moves) 1) ((println "Lizzie Raises") (pf-raise :bot))
          (= (bot-moves) 0) ((println "Lizzie Calls") (pf-call :bot))
          :else  ((println "Lizzie Raises") (pf-raise :bot)))    
  ((println "Call, Raise, or Fold")
   (let [action (read-line)]
     (cond (= action "c")
           (pf-call :human)
           (or (= action "b")
               (= action "r"))
           (pf-raise :human)
           (= action "f")
           (fold :human)
           :else (sb-action player))))))

(defn get-board [street]
  (cond (= street :flop)
        @flop
        (= street :turn)
        (concat @flop @turn)
        (= street :river)
        (concat @flop @turn @river)
        (= street :showdown)
        ((println "Player has: " (concat @player-hand @flop @turn @river))
         (println)
         (println "Lizzie's shows: " @bot-hand)
         (println "Lizzie has: " (concat @bot-hand @flop @turn @river))
         (compare-hand-strength  (concat @player-hand @flop @turn @river) (concat @bot-hand @flop @turn @river))
         (play-game))))

(defn player-action [player street total-bets facing-bet? checks]
  (if (= checks 2)
    (player-action (:oop @position) (nth street-order (+ (.indexOf street-order street) 1)) 0 false 0))
  (println "Lizzie: " (:bot @game-state))
  (println "You: " (:human @game-state))
  (println)
  (println "Pot: " (:pot @game-state))
  (println)
  (println street)
  (println (get-board street))
  (println)
  (println @player-hand)
  (println)
  (cond (and (= checks 0)
             (false? facing-bet?))
        (check-bet (:oop @position) street total-bets checks)
        (and (= checks 1)
             (false? facing-bet?))
        (check-bet (:button @position) street total-bets checks)
        (and (true? facing-bet?)
             (< total-bets max-bets))
        (raise-call-fold player street total-bets)
        (and (true? facing-bet?)
             (= total-bets max-bets))
        (call-fold player street)))

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
  (post-blinds (:button position))
  (sb-action (:oop position)))

(defn -main []
  (println "Welcome to Limit Heads Up Poker -vs- Lizzie")
  (play-game))
