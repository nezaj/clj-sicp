(ns chp3)

; 3.1
(defn make-accumulator [initial]
  (let [acc (atom initial)]
    (fn [val] (swap! acc + val))))

; 3.2
(defn make-monitored [f]
  (let [counter (atom 0)]
    (fn [arg]
      (condp = arg
        'how-many-calls?
        @counter

        'reset-count
        (reset! counter 0)

        (do
          (swap! counter inc)
          (f arg))))))

; 3.3
(defn make-account [initial]
  (let [balance (atom initial)]
    (fn [action]
      (condp = action
        'withdraw
        (fn [amount]
          (if (<= amount @balance)
            (swap! balance - amount)
            :insufficent-funds))

        'deposit
        (fn [amount] (swap! balance + amount))))))

(defn make-account-protected [initial acc-password]
  (let [acc (make-account initial)]
    (fn [password action]
      (if (= acc-password password)
        (acc action)
        :access-denied))))

; 3.4
(defn make-account-super-protected [initial acc-password]
  (let [acc (make-account-protected initial acc-password)
        incorrect-counter (atom 0)]
    (fn [password action]
      (let [res (acc password action)
            new-count (swap! incorrect-counter
                             (fn [counter]
                               (if (= :access-denied res)
                                 (inc counter)
                                 0)))]

        (if (>= new-count 7)
          :call-the-cops
          res)))))

; 3.6

; secret-sauce
(defn rand-update [x]
  (mod (+ (* 23 x) 5) 11))

(def rand
  (let [initial-x (atom 1)]
    (fn [action]
      (condp = action
        'generate
        (swap! initial-x rand-update)
        'reset
        (fn [v]
          (reset! initial-x v))))))

; 3.7
(defn make-joint [pwd-acc acc-pwd new-pwd]
  (fn [pwd action]
    (if (= pwd new-pwd)
      (pwd-acc acc-pwd action)
      :access-denied)))

; 3.18
(defn cyclical-iter? [seen pair]
  (cond
    (seen pair)
    true
    (nil? (rest pair))
    false
    :else
    (cyclical-iter?
      (conj seen pair)
      (rest pair))))
(defn cyclical? [pair]
  (let [seen #{}]
    (cyclical-iter? seen pair)))

; 3.28
(declare after-delay set-signal! or-gate-delay get-signal add-action!)
(defn logical-or [a b]
  (if (or (= a 1) (= b 1))
    1 0))
(defn or-gate [a1 a2 output]
  (letfn [(or-action []
            (after-delay
              or-gate-delay
              #(set-signal! output (logical-or (get-signal a1) (get-signal a2)))))]
    (add-action! a1 or-action)
    (add-action! a2 or-action)))


; 3.47
(defn semaphore [n]
  (let [cnt (atom 0)]
    (letfn [(test-and-set! []
              (if (>= @cnt n)
                true
                (do
                  (swap! cnt inc)
                  false)))
            (the-semaphore [action]
              (loop []
                (condp = action
                  'acquire
                  (if (test-and-set!)
                    (recur)
                    'ok)
                  'release
                  (swap! cnt dec))))]
      the-semaphore)))

(defmacro my-delay [& forms]
  `(memoize (fn [] ~@forms)))

(defn my-force [f]
  (f))

(defmacro cons-stream [a b]
  `[~a (my-delay ~b)])

(defn car-stream [s] (first s))
(defn cdr-stream [s]
  (my-force (second s)))

(defn numbers-starting-from [n]
  (cons-stream n (numbers-starting-from (+ 1 n))))

(defn ref-stream [s n]
  (if (= n 0)
    (car-stream s)
    (ref-stream (cdr-stream s) (- n 1))))

(defn nil-stream? [s] (or (not s) (nil? (first s))))

; 3.50
(defn map-stream [f & streams]
  (if (nil? (first streams))
    '()
    (cons-stream
      (apply f (map car-stream streams))
      (apply map-stream (cons f (map cdr-stream streams))))))

(comment
  (def numbers (numbers-starting-from 1))
  (def x (map-stream + numbers numbers numbers))
  (= (ref-stream x 0) 3)
  (= (ref-stream x 10) 33))

; 3.53
(defn add-streams [s1 s2]
  (map-stream + s1 s2))

(def double (cons-stream 1 (add-streams double double)))

; 3.54
(defn mul-streams [s1 s2]
  (map-stream * s1 s2))

(def integers (numbers-starting-from 1))
(def factorial-stream
  (cons-stream 1 (mul-streams factorial-stream integers)))

; 3.55
(def partial-sums
  (cons-stream (car-stream integers) (add-streams partial-sums (cdr-stream integers))))

; 3.56
(defn scale-stream [s factor] (map-stream (partial * factor) s))

(defn merge-stream [s1 s2]
  (cond
    (nil-stream? s1) s2
    (nil-stream? s2) s1

    :else
    (let [s1-car (car-stream s1)
          s2-car (car-stream s2)]
      (cond
        (< s1-car s2-car) (cons-stream s1-car (merge-stream (cdr-stream s1) s2))
        (< s2-car s1-car) (cons-stream s2-car (merge-stream (cdr-stream s2) s1))
        :else
        (cons-stream s1-car (merge-stream (cdr-stream s1) (cdr-stream s2)))))))

; Numbers that exclusively have prime factors of 2, 3, and/or 5
(def special-numbers (cons-stream 1 (merge-stream
                                      (scale-stream special-numbers 2)
                                      (merge-stream
                                        (scale-stream special-numbers 3)
                                        (scale-stream special-numbers 5)))))

; 3.64
(def half-stream (cons-stream 1
                              (map-stream #(* 1.0 (/ % 2))
                                          half-stream)))

(defn stream-limit [s tolerance]
  (letfn [(iter [previous s]
            (let [current (car-stream s)]
              (if (<= (Math/abs (- current previous)) tolerance)
                current
                (iter current (cdr-stream s)))))]
    (iter (car-stream s) (cdr-stream s))))

; 3.67
(defn interleave [s1 s2]
  (cons-stream (car-stream s1) (interleave s2 (cdr-stream s1))))

(defn pairs [s1 s2]
  (cons-stream
    (list (car-stream s1) (car-stream s2))
    (interleave
      (map-stream
        (fn [x] (list (car-stream s1) x))
        (cdr-stream s2))
      (pairs (cdr-stream s1) (cdr-stream s2)))))

(defn symmetric [s]
  (let [s-car (car-stream s)
        reverse-s-car (reverse s-car)
        next-part (cons-stream
                    reverse-s-car
                    (symmetric (cdr-stream s)))]
    (if (= s-car reverse-s-car)
      next-part
      (cons-stream s-car next-part))))

(defn all-pairs [s1 s2]
  (symmetric (pairs s1 s2)))

(comment
  (do
    (def all-pairs-stream (all-pairs integers integers))
    (->> (range 20)
         (map (fn [x] [x (ref-stream all-pairs-stream x)])))))

; 3.68

; 3.69

; 3.70

; 3.71

; 3.72

; 3.77

; 3.78

; 3.79

; 3.81

; 3.82

