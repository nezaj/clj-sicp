(ns logic-interpreter
  (:require [logic-seed :as seed]))

(def empty-db {:facts {}})
(defonce db (atom empty-db))

; assertions
; -----------

(def fact-kw first)
(def pattern-kw first)

(defn assert! [db fact]
  (swap! db update-in
         [:facts (fact-kw fact)]
         (partial into #{fact})))

(defn bootstrap [db]
  (reset! db empty-db)
  (doseq [fact seed/facts]
    (assert! db fact)))

(comment (bootstrap db))

(defn all-assertions [db] (mapcat second (:facts @db)))
(defn index-assertions [db key] (get (:facts @db) key))
(defn fetch-assertions [db pattern]
  (or (index-assertions db (pattern-kw pattern))
      (all-assertions db)))

(comment
  (do
    (bootstrap db)
    [(fetch-assertions db [:job])
     (fetch-assertions db [:moop])]))

(def frame-contains? contains?)
(def frame-value get)

(defn frame-value-rec [m sym]
  (loop [sym sym]
    (let [v (frame-value m sym)]
      (if (symbol? v)
        (recur v)
        v))))

(def frame-extend assoc)

(declare pattern-match)
(defn extend-if-consistent [sym fact frame]
  (if (frame-contains? frame sym)
    (pattern-match (frame-value frame sym) fact frame)
    (frame-extend frame sym fact)))

(defn pattern-match [pattern fact frame]
  (cond
    (not frame) nil
    (= pattern fact) frame

    (symbol? pattern) (extend-if-consistent pattern fact frame)

    (and (sequential? pattern) (sequential? fact))
    (pattern-match (rest pattern)
                   (rest fact)
                   (pattern-match (first pattern) (first fact) frame))
    :else
    nil))

(comment
  (println
    (pattern-match [:job '?x] [:job "hacker"] {}))
  (println
    (pattern-match [:job '?x] [:job "hacker"] {'?x "accountant"}))
  (println
    (pattern-match [:job '?x :l33t] [:job "hacker" :l33t] {})))

(defn search-assertions [db pattern frame]
  (keep #(pattern-match pattern % frame)
        (fetch-assertions db pattern)))

(comment
  (do
    (bootstrap db)
    (println (search-assertions db [:job '?x ["computer" '?type]] {}))))


(declare qeval)

;; and
;; ----------

(defn and-query [db queries frames]
  (reduce (fn [filtered-frames query]
            (qeval db query filtered-frames))
          frames
          queries))

(comment
  (do
    (bootstrap db)
    (println (qeval db
                    '(and
                       [:job ?x ["computer" ?type]]
                       [:address ?x ["Slumerville" ?addr ?num]])
                    [{}]))))

;; or
;; ----------

(defn or-query [db queries frames]
  (mapcat #(qeval db % frames) queries))

(comment
  (do
    (bootstrap db)
    (qeval db
           '(or
             [:address ?x ["Boston" ?addr ?num]]
             [:address ?x ["Slumerville" ?addr ?num]])
           [{}])))

;; not
;; ----------

(defn not-query [db queries frames]
  (reduce (fn [filtered-frames query]
            (remove
              (fn [frame] (seq (qeval db query [frame]))) filtered-frames))
          frames
          queries))

(comment
  (do
    (bootstrap db)
    (qeval db
           '(and
              [:job ?x ["computer" ?type]]
              (not
                [:address ?x ["Slumerville" ?addr ?num]]))
           [{}])))

;; lisp-value
;; ----------

(defn actualize [frame pat]
  (cond
    (symbol? pat)
    (frame-value-rec frame pat)

    (sequential? pat)
    (mapv (partial actualize frame) pat)

    :else pat))

(comment
  (actualize {'?x '?y
              '?y "Joe"}
             [:job '?x]))

(defn lisp-value-query [_db body frames]
  (let [f (eval (first body))
        args (rest body)]
    (filter
      (fn [frame]
        (apply f (actualize frame args)))
      frames)))

(comment
  (do
    (bootstrap db)
    (qeval db
           '(and
             [:salary ?name ?amount]
             (lisp-value > ?amount 50000))
           [{}])))

;; qeval
;; ----------

(def query-type first)
(def query-contents rest)

(defn simple-query [db query frames]
  (mapcat (partial search-assertions db query) frames))

(defn qeval [db query frames]
  (let [type->query-fn {'and and-query
                        'or or-query
                        'not not-query
                        'lisp-value lisp-value-query}
        f (type->query-fn (query-type query))]
    (if f
      (f db (query-contents query) frames)
      (simple-query db query frames))))

(comment
  (do
    (bootstrap db)
    (qeval db
           [:job '?x ["computer" '?type]]
           [{}])))
