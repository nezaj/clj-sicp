(ns logic-interpreter
  (:require [logic-seed :as seed]
            [clojure.pprint :as pprint]))

(def empty-db {:facts {} :rules {}})
(defonce db (atom empty-db))

(def pattern-kw first)
(defn pattern-var? [x]
  (and (symbol? x)
       (= \? (first (name x)))))

(def frame-contains? contains?)
(def frame-value get)

(defn frame-value-rec [m sym]
  (loop [sym sym]
    (let [v (frame-value m sym)]
      (if (pattern-var? v)
        (recur v)
        v))))

(def frame-extend assoc)

;; db
; ---
(def rule-conclusion first)
(def rule-body second)
(def rule-kw (comp first rule-conclusion))
(defn rule! [db rule]
  (swap! db update-in
         [:rules (rule-kw rule)]
         (partial into #{rule})))

(def fact-kw first)
(defn assert! [db fact]
  (swap! db update-in
         [:facts (fact-kw fact)]
         (partial into #{fact})))

(defn bootstrap [db]
  (reset! db empty-db)
  (doseq [fact seed/facts]
    (assert! db fact))
  (doseq [rule seed/rules]
    (rule! db rule)))

(comment (bootstrap db))

(defn all-rules [db] (mapcat second (:rules @db)))
(defn index-rules [db key] (get (:rules @db) key #{}))
(defn fetch-rules [db pattern]
  (if (keyword? (pattern-kw pattern))
    (index-rules db (pattern-kw pattern))
    (all-rules db)))

(comment
  (do
    (rule! db '[:same ?x ?x])
    (rule! db '[:lives-near ?person-1 ?person-2]
           '(and [:address ?person-1
                  [?town ?address-1 ?number-1]]
                 [:address ?person-2
                  [?town ?address-2 ?number-2]]
                 (not [:same ?person-1 person-2])))
    (fetch-rules db [])))

(defn all-assertions [db] (mapcat second (:facts @db)))
(defn index-assertions [db key] (get (:facts @db) key #{}))
(defn fetch-assertions [db pattern]
  (if (keyword? (pattern-kw pattern))
    (index-assertions db (pattern-kw pattern))
    (all-assertions db)))

(comment
  (do
    (bootstrap db)
    [(fetch-assertions db [:job])
     (fetch-assertions db [:moop])]))

(declare pattern-match)
(defn search-assertions [db pattern frame]
  (keep #(pattern-match pattern % frame)
        (fetch-assertions db pattern)))

(comment
  (do
    (bootstrap db)
    (println (search-assertions db [:job '?x ["computer" '?type]] {}))))

;; pattern-match
;; --------------

(defn extend-if-consistent [sym fact frame]
  (if (frame-contains? frame sym)
    (pattern-match (frame-value frame sym) fact frame)
    (frame-extend frame sym fact)))

(defn pattern-match [pattern fact frame]
  (cond
    (not frame) nil
    (= pattern fact) frame

    (pattern-var? pattern) (extend-if-consistent pattern fact frame)

    (and (sequential? pattern) (sequential? fact))
    (pattern-match (rest pattern)
                   (rest fact)
                   (pattern-match (first pattern) (first fact) frame))
    :else
    nil))

(comment
  (println
    (pattern-match '[:job ?x] [:job "hacker"] {}))
  (println
    (pattern-match '[:job ?x] [:job "hacker"] {'?x "accountant"}))
  (println
    (pattern-match '[:job ?x :l33t] [:job "hacker" :l33t] {})))

(declare qeval)

;; rules
;; -------------

(def application-id-counter (atom 0))
(defn uniq [] (swap! application-id-counter inc))

(defn rename-variables-in [rule]
  (let [id (uniq)]
    (letfn [(walk [exp]
              (cond
                (pattern-var? exp)
                (symbol (str (name exp) "-" id))

                (sequential? exp)
                (map walk exp)

                :else
                exp))]
      (walk rule))))

(comment
  (rename-variables-in '[
                         [:wheel ?person]
                         (and [:supervisor ?middle-manager ?person]
                              [:supervisor ?x ?middle-manager])]))

(declare unify-match)
(defn apply-rule [db pattern rule frame]
  (let [cleaned-rule (rename-variables-in rule)
        unified-frame (unify-match pattern (rule-conclusion cleaned-rule) frame)]
    (when unified-frame
      (if (rule-body cleaned-rule)
        (qeval db (rule-body cleaned-rule) [unified-frame])
        [unified-frame]))))

(defn search-rules [db pattern frame]
  (mapcat #(apply-rule db pattern % frame) (fetch-rules db pattern)))

(comment
  (do
    (bootstrap db)
    (search-rules db '[:lives-near ?x ["Bitdiddle Ben"]] {})))

(comment
  (qeval
    db
    '(and [:address ["Bitdiddle Ben"] [?town ?addr-1 ?num-1]]
          [:address ?person-2 [?town ?addr-2 ?num-2]]
          (not [:same ["Bitdiddle Ben"] ?person-2]))
    [{}]))

;; unify-match
;; --------------
(defn depends-on? [frame pat sym]
  (cond
    (sequential? pat)
    (some #(depends-on? frame % sym) pat)

    (pattern-var? pat)
    (if (= pat sym)
      true
      (and (frame-contains? frame pat)
           (depends-on? frame (frame-value pat) sym)))

    :else
    false))

(defn extend-if-possible [sym-a pat-b frame]
  (cond
    (frame-contains? frame sym-a)
    (unify-match (frame-value frame sym-a) pat-b frame)

    (pattern-var? pat-b)
    (if (frame-contains? frame pat-b)
      (unify-match sym-a (frame-value frame pat-b) frame)
      (frame-extend frame sym-a pat-b))

    (depends-on? frame pat-b sym-a) nil

    :else
    (frame-extend frame sym-a pat-b)))

(defn unify-match [pat-a pat-b frame]
  (cond
    (not frame) nil
    (= pat-a pat-b) frame

    (pattern-var? pat-a)
    (extend-if-possible pat-a pat-b frame)

    (pattern-var? pat-b)
    (extend-if-possible pat-b pat-a frame)

    (and (sequential? pat-a) (sequential? pat-b))
    (unify-match (rest pat-a)
                 (rest pat-b)
                 (unify-match (first pat-a) (first pat-b) frame))))

(comment
  [(unify-match '[:wheel ?who] '[:wheel ?person] {})
   (unify-match '[:wheel "Joe"] '[:wheel ?person] {})
   (unify-match '[:x ?who] '[:y ?what] {})
   (unify-match '[:x ?who] '[:x :x] {})
   (unify-match '[:x ?who] '[:x [:foo ?what]] {})])

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

;; unique
;; ----------

(defn unique-query [db queries frames]
  (reduce (fn [filtered-frames query]
            (->> filtered-frames
                 (map #(qeval db query [%]))
                 (filter #(= 1 (count %)))
                 flatten))
          frames
          queries))

(comment
  (do
    (bootstrap db)
    [(qeval db
            '(unique
               [:job ?x ["computer" "wizard"]])
            [{}])
     (qeval db
            '(unique
               [:job ?x ["computer" ?who]])
            [{}])
     (qeval db
            '(and [:job ?x ?j] (unique [:job ?anyone ?j]))
            [{}])
     ]))


;; lisp-value
;; ----------

(defn actualize [frame pat]
  (cond
    (pattern-var? pat)
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
  (concat
    (mapcat (partial search-assertions db query) frames)
    (mapcat (partial search-rules db query) frames)))

(defn qeval [db query frames]
  (let [type->query-fn {'and and-query
                        'or or-query
                        'not not-query
                        'lisp-value lisp-value-query
                        'unique unique-query}
        f (type->query-fn (query-type query))]
    (if f
      (f db (query-contents query) frames)
      (simple-query db query frames))))

(comment
  (do
    (bootstrap db)
    (qeval db
           '[:job ?x ["computer" ?type]]
           [{}])))

;; loop
;; ----------

(defn exit? [s]
  (= s 'exit))

(defn repl-loop
  ([]
   (bootstrap db)
   (repl-loop db))
  ([db]
   (println (str
              "Hello! Type something in, and get ready to repl. \n"
              "Type 'exit' to quit"
              "\n"
              "🔥 >"))
   (loop []
     (let [query (read-string (read-line))]
       (if (exit? query)
         (println "👋🏼 Goodbye")
         (do
           (pprint/pprint
             (map #(actualize % query) (qeval db query [{}])))
           (recur)))))))
