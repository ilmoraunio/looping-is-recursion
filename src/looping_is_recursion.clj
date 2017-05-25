(ns looping-is-recursion)

(defn power [base exp]
  (if (zero? exp)
    1
    (loop [n (Math/abs exp)
           acc base]
      (if (= 1 n)
        (if (pos? exp)
          acc
          (/ 1 acc))
        (recur (dec n) (* acc base))))))

(defn last-element [a-seq]
  (loop [[a & rest] a-seq]
    (if (seq rest)
      (recur rest)
      a)))

(defn seq= [seq1 seq2]
  (loop [[a & a-rest] seq1
         [b & b-rest] seq2]
    (cond (and (= a b)
               (empty? a-rest)
               (empty? b-rest)) true
          (not= a b) false
          (not= (seq a-rest) (seq b-rest)) false
          :else (recur a-rest b-rest))))

(defn find-first-index [pred a-seq]
  (loop [n 0
         [a & rest] a-seq]
    (cond (nil? a) nil
          (pred a) n
          (empty? rest) nil
          :else (recur (inc n) rest))))

(defn avg [a-seq]
  (if (empty? a-seq)
    nil
    (loop [[x & rst] a-seq
           n 1
           acc 0]
      (if (seq rst)
        (recur rst (inc n) (+ acc x))
        (/ (+ acc x) n)))))


(defn parity [a-seq]
  (loop [freqs {}
         [a & rst :as coll] a-seq]
    (if (nil? a)
      (map first (filter #(-> % second odd?) freqs))
      (recur
        (update-in freqs [a] #(if (nil? %) 1 (inc %)))
        rst))))

(defn fast-fibo [n]
  (loop [i 1
         f_n-1 0
         f_n 1]
    (cond (= n 0) 0
          (= n 1) 1
          (= i n) f_n
          :else (recur (inc i) f_n (+ f_n-1 f_n)))))

(defn cut-at-repetition [a-seq]
  (loop [s (hash-set)
         coll a-seq]
    (let [[a & rest] coll]
      (cond
        (nil? a) (take (count s) a-seq)
        (contains? s a) (take (count s) a-seq)
        :else (recur (conj s a) rest)))))

