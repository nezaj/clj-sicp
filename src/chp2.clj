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

; 2.42

(defn combination [xs ys]
  (for [x xs y ys] [x y]))

(defn all-positions [board-size]
  (combination (range board-size) (range board-size)))

(defn can-kill? [[x1 y1] [x2 y2]]
  (or
    (= x1 x2)                                               ; horizontal
    (= y1 y2)                                               ; vertical
    (= (Math/abs (- x1 x2)) (Math/abs (- y1 y2)))))         ; diag

(defn safe? [positions candidate]
  (not (some #(can-kill? % candidate) positions)))

(defn queens [board-size num-queens]
  (condp = num-queens
    1 (map (partial conj #{}) (all-positions board-size))
    (let [prev-solutions (queens board-size (dec num-queens))
          candidates (all-positions board-size)]
      (->> (combination prev-solutions candidates)
           (filter (partial apply safe?))
           (map (partial apply conj))))))

(defn print-positions [board-size positions]
  (println "---------")
  (->> (range board-size)
       (map (fn [x]
              (map (fn [y]
                     (if (positions [x y])
                       "X"
                       "_"))
                   (range board-size))))
       (run! println)))

; 2.59
(defn adjoin-set [s e]
  (if (some #{e} s)
    s
    (conj s e)))

(defn union-set [s1 s2]
  (reduce adjoin-set s1 s2))

; 2.61
(defn adjoin-set-ordered [s e]
  (cond
    (or (not (seq s)) (< e (first s)))
    (cons e s)

    (= e (first s))
    s
    :else
    (cons (first s) (adjoin-set-ordered (rest s) e))))

; 2.62
(defn union-set-ordered [s1 s2]
  (let [a (first s1)
        b (first s2)]
    (cond
      (nil? a)
      s2

      (nil? b)
      s1

      (< a b)
      (cons a (union-set-ordered (rest s1) s2))

      (> a b)
      (cons b (union-set-ordered s1 (rest s2)))

      :else
      (cons a (union-set-ordered (rest s1) (rest s2))))))

; 2.67
(defn left-branch [tree]  (first tree))
(defn right-branch [tree] (second tree))
(defn symbols [tree] (tree 2))
(defn weight [tree] (tree 3))
(defn make-leaf [symbol weight] [nil nil [symbol] weight])
(defn leaf? [tree] (and
                     (not (left-branch tree))
                     (not (right-branch tree))))

(defn make-code-tree [left right]
  [left
   right
   (concat (symbols left) (symbols right))
   (+ (weight left) (weight right))])

(defn decode [tree bits]
  (letfn [(choose-branch [bit branch]
            ((if (= bit 0) left-branch right-branch)
             branch))
          (decode-helper [bits current-branch]
            (if (not (seq bits))
              '()
              (let [next-branch (choose-branch (first bits) current-branch)]
                (if (leaf? next-branch)
                  (concat (symbols next-branch) (decode-helper (rest bits) tree))
                  (decode-helper (rest bits) next-branch)))))]
    (decode-helper bits tree)))

(comment
  (def sample-tree (make-code-tree (make-leaf 'A 4)
                                   (make-code-tree (make-leaf 'B 2) (make-code-tree
                                                                      (make-leaf 'D 1)
                                                                      (make-leaf 'C 1)))))
  (def sample-bits '(0 1 1 0 0 1 0 1 0 1 1 1 0))
  (def sample-message '(A D A B B C A))
  (= (decode sample-tree sample-bits) sample-message))

; 2.68
(defn encode [tree message]
  (letfn [(encode-symbol-helper [symbol path branch]
            (cond
              (= [symbol] (symbols branch))
              path

              :else
              (let [left (left-branch branch)
                    right (right-branch branch)]
                (or
                  (and left (encode-symbol-helper symbol (conj path 0) left))
                  (and right (encode-symbol-helper symbol (conj path 1) right))))))
          (encode-symbol [symbol]
            (if-let [res (encode-symbol-helper symbol [] tree)]
              res
              (throw (Exception. (format "Uh oh, couldn't find symbol = %s" symbol)))))]
    (mapcat encode-symbol message)))

(comment
  (def sample-tree (make-code-tree (make-leaf 'A 4)
                                   (make-code-tree (make-leaf 'B 2) (make-code-tree
                                                                      (make-leaf 'D 1)
                                                                      (make-leaf 'C 1)))))
  (def sample-bits '(0 1 1 0 0 1 0 1 0 1 1 1 0))
  (def sample-message '(A D A B B C A))
  (= (encode sample-tree sample-message) sample-bits))

; 2.69
;; Our initial implementation went from higher to lower
;; This ends up producing an unbalanced tree though,
;; which can become costly. i.e 5 1 1 1 1 will produce
;; a right-leaning tree, while in huffman it will be balanced

(defn generate-huffman-tree-right-leaning [pairs]
  (letfn [(generate-helper [branches]
            (if (<= (count branches) 1)
              (first branches)
              (make-code-tree
                (first branches)
                (generate-helper (rest branches)))))]
    (generate-helper (->> pairs
                          (map (partial apply make-leaf))
                          (sort-by weight)
                          reverse))))

(defn generate-huffman-tree [pairs]
  (letfn [(generate-helper [branches]
            (if (<= (count branches) 1)
              (first branches)
              (let [sorted (sort-by weight branches)
                    [heavier lighter] (reverse (take 2 sorted))
                    next (drop 2 sorted)]
                (generate-helper
                  (conj
                    next
                    (make-code-tree heavier lighter))))))]
    (generate-helper (map (partial apply make-leaf) pairs))))

(comment
  (def pairs '([A 5] [B 2] [C 1] [D 1] [E 1]))
  (generate-huffman-tree pairs))

; 2.73 abd
(defn variable? [expr] (symbol? expr))
(defn same-variable? [a b] (and (variable? a) (= a b)))
(defn operator [expr] (first expr))
(defn operands [expr] (rest expr))
(defn make-sum [a b] (list '+ a b))
(defn make-product [a b] (list '* a b))

(def deriv-fns (atom {}))
(defn install-operator [operator fn]
  (swap! deriv-fns #(assoc % operator fn)))

(defn deriv [expr var]
  (cond
    (number? expr)
    0

    (variable? expr)
    (if (same-variable? expr var) 1 0)

    :else
      ((get @deriv-fns (operator expr))
           (operands expr) var)))

(defn multiplier [operands] (first operands))
(defn multiplicand [operands] (second operands))
(defn deriv-product [operands var]
  (make-sum
    (make-product
      (multiplier operands)
      (deriv (multiplicand operands) var))
    (make-product
      (multiplicand operands)
      (deriv (multiplier operands) var))))

(defn install-deriv-product []
  (install-operator '* deriv-product))

(defn left [operands] (first operands))
(defn right [operands] (second operands))
(defn deriv-sum [operands var]
  (make-sum
    (deriv (left operands) var)
    (deriv (right operands) var)))

(defn install-deriv-sum []
  (install-operator '+ deriv-sum))

(comment
  (do (install-deriv-product)
      (install-deriv-sum))
  (deriv '(* 2 x) 'x)
  (deriv '(+ x x) 'x)
  (deriv '(* 2 x) 'y))

; 2.75
(defn make-from-mag-ang [mag ang]
  (fn [op]
    (condp = op
      'magnitude mag
      'angle ang)))