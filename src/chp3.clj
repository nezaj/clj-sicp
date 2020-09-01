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
    (cyclical-iter?
      (conj seen pair)
      (rest pair))))
(defn cyclical? [pair]
  (let [seen #{}]
    (cyclical-iter? seen pair)))

; 3.28