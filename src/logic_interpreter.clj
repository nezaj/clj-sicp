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