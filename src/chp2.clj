(ns chp2)

; Exercise 2.1
(defn make-rat [num denom]
  (cond
    (or
      (and (neg? num) (neg? denom))
      (and (pos? num) (pos? denom)))
    [(Math/abs num) (Math/abs denom)]

    (or (neg? num) (neg? denom))
    [(* -1 (Math/abs num)) (Math/abs denom)]

    :else
    [(Math/abs num) (Math/abs denom)]))

(defn make-rat [num denom]
  (let [sign (if (pos? (* num denom)) 1 -1)
        num' (Math/abs num)
        denom' (Math/abs denom)]
    [(* sign num') denom']))

; Exercise 2.7
(defn make-interval [a b] (if (<= a b) [a b] nil))
(def lower-bound (partial apply min))
(def upper-bound (partial apply max))
(defn add-interval [a b]
  (make-interval
    (min (lower-bound a) (lower-bound b))
    (max (upper-bound a) (upper-bound b))))

; Exercise 2.8
(defn sub-interval [a b]
  (let [[left right] (sort-by lower-bound [a  b])
        lower (min (upper-bound left) (lower-bound right))
        upper (min (upper-bound left) (upper-bound right))]
    (make-interval lower upper)))

; Exercise 2.17
(defn find-last [a]
  (let [n (next a)]
    (if n (find-last n) (first a))))

; Exercise 2.18
(defn my-reverse [a]
  (if (nil? a)
    a
    (concat (my-reverse (next a)) (list (first a)))))

(defn my-reverse-iter [a]
  (loop [ret '()
         a a]
    (if (nil? a)
      ret
      (recur (cons (first a) ret)
             (next a)))))

; Exercise 2.21
(defn square [x] (* x x))
(defn square-list-recur [items]
  (if (nil? items)
    items
    (cons (square (first items)) (square-list-recur (next items)))))

(def square-list (partial map square))

; Exercise 2.27
(defn deep-reverse [xs]
  (->> xs
       my-reverse
       (map (fn [x] (if (coll? x)
                      (deep-reverse x)
                      x)))))

; Exercise 2.28
(defn fringe [tree]
  (cond
    (nil? tree)
    tree

    (not (seq? tree))
    (list tree)

    :else (concat (fringe (first tree)) (fringe (next tree)))))

; 2.30 + 2.31
(defn deep-map [f xs]
  (map (fn [x] (if (coll? x) (deep-map f x) (f x))) xs))

(def square-tree (partial deep-map square))

; 2.33
(defn length [seq]
  (reduce
    (fn [acc x] (inc acc))
    0 seq))

; 2.41
(defn generate-perms [xxs]
  (letfn [(generate-perms-helper [xs ys]
            (if (nil? xs)
              (map (fn [y] [y]) ys)
              (mapcat (fn [x]
                        (map (fn [y] (flatten [x y])) ys))
                      xs)))]
    (reduce generate-perms-helper xxs)))

(defn triples [n k]
  (->> (range 1 (+ n 1))
       (repeat 3)
       generate-perms
       (filter (fn [t] (= (apply + t) k)))))