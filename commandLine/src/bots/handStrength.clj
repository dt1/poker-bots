(ns bots.handStrength
  (use [bots.gameSetup]))

(defn rank-count [hand-ranks qty]
  (for [[kk vv] hand-ranks
        :when (= vv qty)]
    kk))

(defn desc [m]
  (reverse (sort (vals m))))

(defn rank-frequency [hand]
  (frequencies (map first hand)))

(defn suit-frequency [hand]
  (frequencies (map second hand)))

(defn get-ranks-strength [hand-map]
  (map #(index-of ranks %) hand-map))

(defn get-kickers [k-cards to-take]
  (take-last to-take
             (sort
              (get-ranks-strength k-cards))))

(defn compare-kickers [k1 k2]
  (let [kk1 (first k1)
        kk2 (last k2)]
    (if (>= kk1 kk2)
      (list kk1)
      (list kk2))))

(defn ranks-strength-count [hand-map cnt]
  (get-ranks-strength (rank-count hand-map cnt)))

(defn sorted-ranks-strength-count [hand-map cnt]
  (sort (ranks-strength-count hand-map cnt)))
  

(defn high-card [hand-map]
  (let [h-hand (sorted-ranks-strength-count hand-map 1)]
    (conj
     (take-last 1 h-hand)
     (take-last 4 (drop-last h-hand)))))

(defn pair [hand-map]
  (let [p-card (ranks-strength-count hand-map 2)
        k-cards (sorted-ranks-strength-count hand-map 1)]
    (conj p-card (take-last 3 k-cards))))

(defn two-pair [hand-map]
  (let [p-cards (sorted-ranks-strength-count hand-map 2)
        k-cards (sorted-ranks-strength-count hand-map 1)]
    (if (= (count p-cards) 2)
      (conj p-cards (take-last 1 k-cards))
      (conj (take-last 2 p-cards)
            (compare-kickers p-cards k-cards)))))

(defn trips [hand-map]
  (let [t-cards (ranks-strength-count hand-map 3)
        k-cards (sorted-ranks-strength-count hand-map 1)]
    (conj t-cards
          (take-last 2 k-cards))))

(defn quads [hand-map]
  (let [q-cards (ranks-strength-count hand-map 4)
        k-cards (sort (concat (ranks-strength-count hand-map 1)
                              (ranks-strength-count hand-map 2)
                              (ranks-strength-count hand-map 3)))]
    (conj q-cards
          (take-last 1 k-cards))))

(defn full-house [hand-map]
  (let [t-cards (sorted-ranks-strength-count hand-map 3)
        p-cards (sorted-ranks-strength-count hand-map 2)]
    (if (= (count t-cards) 2)
          (conj (take-last 1 t-cards)
                (take 1 t-cards))
          (conj t-cards
                (take-last 1 p-cards)))))

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
          (sort (get-ranks-strength ss)))))))

(defn straight [hand-map]
  (let [t (sort (set (get-ranks-strength (keys hand-map))))]
    (println t)
    (if (and (= (take 4 t) [1 2 3 4])
             (= (last t) 13)
             (not (some #{6} t)))
      (conj (take 4 t) 
            (last t))
      (loop [tt t]
        (when (> (count tt) 4)
          (let [s5 (take 5 tt)]
            (if (= (- (last s5) (first s5)) 4)
              (loop [res tt]
                (let [r5 (take-last 5 res)]
                  (if (= (- (last r5) (first r5)) 4)
                    r5
                    (recur (drop-last res)))))
                (recur (rest tt)))))))))

(defn sf-helper [hand suit]
  (remove nil? (for [[x y] hand]
                 (if (= y suit)
                   [x y]))))

(defn straight-flush [hand]
  (let [h-hand (sf-helper hand "h")
        d-hand (sf-helper hand "d")
        c-hand (sf-helper hand "c")
        s-hand (sf-helper hand "s")]
    (cond (>= (count h-hand) 5)
          (straight h-hand)
          (>= (count d-hand) 5)
          (straight d-hand)
          (>= (count c-hand) 5)
          (straight c-hand)
          (>= (count s-hand) 5)
          (straight s-hand))))

(defmulti disperse-hand :s)

;; quads
(defmethod disperse-hand '(4 1 1 1) [m]
  (quads (:h m)))

(defmethod disperse-hand '(4 2 1) [m]
  (quads (:h m)))

(defmethod disperse-hand '(4 3) [m]
  (quads (:h m)))

;; full house
(defmethod disperse-hand '(3 2 1 1) [m]
  (full-house (:h m)))

(defmethod disperse-hand '(3 2 2) [m]
  (full-house (:h m)))

(defmethod disperse-hand '(3 3 1) [m]
  (full-house (:h m)))

;; trips ; straigtht ; flush
(defmethod disperse-hand '(3 1 1 1 1) [_]
  (println "3k ; st ; fl"))

;; two pair ; straight ; flush
(defmethod disperse-hand '(2 2 1 1 1) [_]
  (println "2p ; st ; fl"))

;; two pair
(defmethod disperse-hand '(2 2 2 1) [m]
  (two-pair (:h m)))

;; pair ; straight ; flush
(defmethod disperse-hand '(2 1 1 1 1 1) [m]
  (pair (:h m)))

;; high card ; straight ; flush
(defmethod disperse-hand '(1 1 1 1 1 1 1) [_]
  (println "high card ; st ; fl"))

(defmethod disperse-hand '(5 1 1) [_]
  (println "flush"))

(defn make-frequency-maps [hand]
  (let [rf-hand (rank-frequency hand)
        desc-seq (desc rf-hand)]
    (disperse-hand {:s desc-seq :h rf-hand})))

(defn get-hand-strength [hand]
  (prn hand)
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
