(ns bots.tests
  (:require [bots.handStrength :as hs]
            [clojure.test :refer :all]))



(defn test-two-pair [h r]
  (is (= (hs/get-hand-strength h) r)))


;; high card
(def h1 [["Q" "s"] ["9" "d"] ["2" "c"] ["8" "h"] ["6" "d"] ["5" "s"] ["T" "d"]])


(def h2 [["J" "c"] ["4" "h"] ["A" "c"] ["2" "c"] ["Q" "h"] ["K" "h"] ["9" "c"]])

(def h3 [["9" "h"] ["T" "d"] ["A" "c"] ["2" "c"] ["Q" "h"] ["K" "h"] ["9" "c"]])


;; pair
(def p1 [["T" "s"] ["Q" "s"] ["A" "d"] ["Q" "c"] ["6" "s"] ["J" "s"] ["2" "d"]])

(def p2 [["T" "h"] ["J" "c"] ["A" "d"] ["Q" "c"] ["6" "s"] ["J" "s"] ["2" "d"]])

(def p3 [["5" "h"] ["K" "c"] ["2" "c"] ["8" "h"] ["6" "d"] ["5" "s"] ["T" "d"]])


;; two pair
(def tp1 [["T" "s"] ["7" "c"] ["Q" "h"] ["Q" "c"] ["K" "h"] ["K" "c"] ["6" "s"]])

(def tp2 [["T" "s"] ["7" "c"] ["Q" "h"] ["Q" "c"] ["K" "h"] ["K" "c"] ["6" "s"]])

(def tp3 [["T" "s"] ["T" "c"] ["Q" "h"] ["Q" "c"] ["K" "h"] ["K" "c"] ["6" "s"]])

(def tp4 [["6" "s"] ["8" "c"] ["Q" "h"] ["Q" "c"] ["K" "h"] ["K" "c"] ["6" "s"]])


;; trips
(def t31 [["T" "s"] ["T" "c"] ["T" "h"] ["Q" "c"] ["J" "h"] ["K" "c"] ["6" "s"]])

;; (def tp2 [["T" "s"] ["7" "c"] ["Q" "h"] ["Q" "c"] ["K" "h"] ["K" "c"] ["6" "s"]])

;; (def tp3 [["T" "s"] ["T" "c"] ["Q" "h"] ["Q" "c"] ["K" "h"] ["K" "c"] ["6" "s"]])

;; (def tp4 [["6" "s"] ["8" "c"] ["Q" "h"] ["Q" "c"] ["K" "h"] ["K" "c"] ["6" "s"]])




(def tp1-result '[((9) 11 12) :two-pair])




;; full houses

(def fh1 [["Q" "s"] ["Q" "c"] ["Q" "d"] ["K" "s"] ["K" "d"] ["2" "c"] ["3" "c"]])

(def fh2 [["K" "s"] ["K" "c"] ["K" "d"] ["3" "s"] ["3" "d"] ["2" "c"] ["3" "c"]])




