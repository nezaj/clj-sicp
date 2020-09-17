(ns chp4
  (:require [amb-interpreter :as amb-interpreter]))

(defn my-map [f xs]
  (if (nil? (seq xs))
    '()
    (cons (f (first xs)) (my-map f (rest xs)))))

(defn my-map-iter [f xs]
  (loop [res '[]
         my-xs xs]
    (if (nil? my-xs)
      res
      (recur (conj res (f (first my-xs)))
             (next my-xs)))))

(defn my-subsets [xs]
  (if (nil? xs)
    '(())
    (let [head (first xs)
          tail (my-subsets (next xs))]
      (concat
        (my-map-iter #(cons head %) tail)
        tail))))

; 4.21
;; fib with y combinator
((fn [n]
   ((fn [fib] (fib fib n))
    (fn [fib k]
      (condp = k
        0 0
        1 1
        (+ (fib fib (- k 1))
           (fib fib (- k 2)))))))
 10)

; 4.35
(comment
  (amb-interpreter/bootstrap-repl
    ['(define (require p) (if (false? p) (amb) true))
     '(define (an-integer-between a b)
              (if (> a b)
                (amb)
                (amb
                  a
                  (an-integer-between (+ a 1) b))))
     '(define (a-pythagorean-triple-between low high)
              (let ((i (an-integer-between low high)))
                (let ((j (an-integer-between i high)))
                  (let ((k (sqrt (+ (* i i) (* j j)))))
                    (require (= (floor k) k))
                    (cons i (cons j (cons (int k) ())))))))]))

; 4.64
; 4.65
; 4.66
; 4.67
; 4.70
; 4.71
; 4.72
; 4.73
; 4.74
; 4.75
; 4.76