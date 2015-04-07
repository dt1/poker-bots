(ns bots.dealer)

(defmulti chat :chat)

;; take action

(defmethod chat :crf [_]
  (println "Call, Raise, or Fold"))

(defmethod chat :cb [_]
  (println "Check or Bet"))

(defmethod chat :cf [_]
  (println "Call or Fold"))

;; lizzie actions
(defmethod chat :lizzie-checks [_]
  (println "Lizzie Checks"))

(defmethod chat :lizzie-bets [_]
  (println "Lizzie Bets"))

(defmethod chat :lizzie-calls [_]
  (println "Lizzie Calls"))

(defmethod chat :lizzie-raises [_]
  (println "Lizzie Raises"))

;; showdown
(defmethod chat :lizzy-wins [_]
  (println "Lizzy Wins!"))

(defmethod chat :player-wins [_]
  (println "You Win!"))

(defmethod chat :chop [_]
  (println "Chop!"))

(defmethod chat :default [_]
  (println ""))

(defn chat-map [x]
  (chat {:chat x}))


(defn call-raise-fold []
  (chat-map :crf))

(defn check-bet []
	 (chat-map :cb))

(defn call-fold []
	 (chat-map :cf))

(defn lizzy-wins []
	 (chat-map :lizzy-wins))

(defn player-wins []
	 (chat-map :player-wins))

(defn chop []
	 (chat-map :chop))

(defn lizzie-raises []
	 (chat-map :lizzie-raises))

(defn lizzie-calls []
	 (chat-map :lizzie-calls))

(defn lizzie-bets []
	 (chat-map :lizzie-bets))

(defn lizzie-checks []
	 (chat-map :lizzie-checks))
