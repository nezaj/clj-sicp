(ns chp4)

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

; 4.1

; 4.2
; 4.3
; 4.4
; 4.6
; 4.7
; 4.8
; 4.9 (let's do one of these?)
; 4.10 (seems like a fun thinking exercise)
; 4.11
; 4.12
; 4.13
; 4.14
; 4.15
; 4.16
; 4.17
; 4.18
; 4.19
; 4.20
; 4.21
; 4.22
; 4.23
; 4.24?
; 4.25
; 4.26
; 4.27
; 4.28
; 4.29
; 4.30
; 4.31
; 4.35
; 4.36
; 4.37
; 4.46
; 4.49
