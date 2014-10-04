(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [last-element a-seq]
                 (if (empty? a-seq)
                   last-element
                   (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [seq1 seq2]
                 (cond
                  (and (empty? seq1) (empty? seq2)) true
                  (or (empty? seq1) (empty? seq2)) false
                  (not= (first seq1) (first seq2)) false
                  :else (recur (rest seq1) (rest seq2))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [a-seq2 a-seq
         idx 0]
    (cond
     (empty? a-seq2) nil
     (pred (first a-seq2)) idx
     :else (recur (rest a-seq2) (inc idx)))))

(defn avg [a-seq]
  (loop [sum 0
         elemCount 0
         a-seq a-seq]
    (if (empty? a-seq)
      (/ sum elemCount)
      (recur (+ sum (first a-seq)) (inc elemCount) (rest a-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [parity-set #{}
         a-seq a-seq]
    (if (empty? a-seq)
      parity-set
      (recur (toggle parity-set (first a-seq)) (rest a-seq)))))

(defn fast-fibo [n]
  (loop [Fn-1 0
         Fn 1
         accu-n 1]
    (cond
     (== n 0) 0
     (== accu-n n) Fn
     :else (recur Fn (+ Fn-1 Fn) (inc accu-n)))))

(defn cut-at-repetition [a-seq]
  (loop [cut-seq '()
         a-seq a-seq]
    (if (empty? a-seq)
      (reverse cut-seq)
      (recur (if (.contains cut-seq (first a-seq))
               cut-seq
               (cons (first a-seq) cut-seq))
             (rest a-seq)))))

