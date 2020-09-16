(ns chp4
  (:require [lisp-interpreter :as li]))

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
; 4.36
; 4.37
; 4.46
; 4.49
