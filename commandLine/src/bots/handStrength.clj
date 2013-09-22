(ns bots.handStrength
  (use [bots.gameSetup]))

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
    (if 
        (some #(<= 5 %) (vals a))
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
        (conj (take 4 t) 
              (last t))
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
