(ns lisp-interperter)

(defn self-evaluating? [exp]
  (or (number? exp) (string? exp)))

(defn variable? [exp]
  (symbol? exp))

(defn scheme-eval [exp env]
  (cond
    (self-evaluating? exp)
    exp

    (variable? exp)
    (lookup-veriable-value exp env)

    :else
    "Not supported")
  exp)

(defn parse [s]
 (scheme-eval
   (read-string s)
   {}))

(defn repl-loop []
  (loop []
    (print "> ")
    (let [input (read-line)]
      (println (parse input))
      (recur))))

(comment
  (parse "1"))