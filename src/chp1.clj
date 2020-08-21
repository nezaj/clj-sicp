(ns chp1)

;; Exercise 1.2
(comment
  (/ (+ 5.0 4 (- 2
                 (- 3
                    (+ 6
                       (/ 4 5)))))
     (* 3 (- 6 2) (- 2 7))))

;; Exercise 1.3
(defn sum-square-two-largest [xs]
  (->> xs
       sort
       (take-last 2)
       (map #(* % %))
       (apply +)))
(comment

  "Should return 181 -> 10^2 + 9^2"
  (sum-square-two-largest [9 8 10 2]))

(defn p [] (p) )
(defn test [x y]
  (if (= x 0) 0 y))
(comment
  "This should cause stack oveflow due to expanding arguments indefinitely"
  (test 0 (p)))

;; Exercise 1.11
(defn piecewise [n]
  (if (< n 3)
    3
    (+
      (piecewise (- n 1))
      (* 2 (piecewise (- n 2)))
      (* 3 (piecewise (- n 3))))))

(defn iter-piecewise [n]
  (loop [cnt 3
         m3 3
         m2 3
         m1 3]
    (if (< n cnt)
      m1
      (recur
        (+ 1 cnt)
        m2
        m1
        (+ m1
           (* 2 m2)
           (* 3 m3))))))
(comment
  (piecewise 6)
  (iter-piecewise 6))

;; Exercise 1.12
(defn next-row [last-row]
  (let [sum-neighbor (map-indexed
                       (fn [idx x]
                         (+ x
                            (get last-row
                                 (+ 1 idx)
                                 0)))
                       last-row)]
    (into [1] sum-neighbor)))

(defn pascal [n]
  (condp = n
    0 []
    1 [[1]]
    (let [prev-p (pascal (- n 1))]
      (conj prev-p (next-row (last prev-p))))))

;; Exercise 1.16
(defn fast-exp [b n]
  (loop [a 1
         b b
         n n]
    (cond
      (= 0 n) a
      (odd? n) (recur (* a b) b (- n 1))
      :else (recur a (* b b) (/ n 2)))))

;; Exercise 1.41
(defn double [f]
  (fn [a] ( f (f a))))

;; Exercise 1.42
(defn compose [& fs]
  (fn [a] (->> fs
               reverse
               (reduce (fn [res f] (f res))
                       a))))

(defn square [x] (* x x))

;; Exercise 1.43
(defn repeated-loop [f n]
  (loop [n n
         f f]
    (if (<= n 1)
      f
      (recur (- n 1) (compose f f)))))

(defn repeated-range [f n]
  (->> (range n)
       (map (fn [_] f))
       (apply compose)))

;; Exercise 1.46
(defn iterative-improve [good-enough? improver]
  (fn [guess]
    (loop [guess guess]
      (if (good-enough? guess)
        guess
        (recur (improver guess))))))