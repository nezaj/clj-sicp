(ns lisp-interpreter
  (:require [clojure.pprint :as pprint]))

(declare scheme-eval)

(def empty-list? (partial = ()))
(defn self-evaluating? [exp]
  ((some-fn
     number?
     string?
     boolean?
     empty-list?) exp))

(defn variable? [exp]
  (symbol? exp))

(def scheme-false? (partial = false))
(def scheme-true? (comp not scheme-false?))

(defn zip-kv [ks vs]
  (format "Incorrect arity: got %s but need %s"
          (count vs)
          (count ks))
  (into {} (map (fn [x y] [x y]) ks vs)))

(defn tag-of? [tag exp]
  (= tag (first exp)))

;; ----
;; Env

(defn env-exists? [env]
  (not (nil? env)))

(defn env-data [env]
  (:data env))

(defn env-sym-bound? [env sym]
  (contains? @(env-data env) sym))

(defn env-sym-value [env sym]
  (get @(env-data env) sym))

(defn env-parent [env]
  (:parent env))

(defn env-create [data parent]
  {:data (atom data) :parent parent})

(defn env-create-root []
  (env-create {'+ + '- -
               '* * '/ /
               '= =
               'true? scheme-true?
               'false? scheme-false?}
              nil))

(defn env-define! [env k v]
  (swap! (env-data env) assoc k v))

(defn env-unbind! [env k]
  (swap! (env-data env) dissoc k))

(defn find-env-for-sym [env sym]
  (if-not (env-exists? env)
    nil
    (if (env-sym-bound? env sym)
      env
      (find-env-for-sym (env-parent env) sym))))

(defn lookup-variable-value [env sym]
  (if-let [env' (find-env-for-sym env sym)]
    (env-sym-value env' sym)
    (throw (Exception. (format "unbound variable: %s" sym)))))

(comment
  (let [parent (env-create {'a "moop"} nil)
        child (env-create {'b "lol"} parent)]
    (println "Lookup b:" (lookup-variable-value child 'b))
    (println "Lookup a:" (lookup-variable-value child 'a))
    (println "Lookup c should fail\n---")
    (lookup-variable-value child 'c)))

;; ----
;; If

(def if-form? (partial tag-of? 'if))
(defn if-predicate [exp] (second exp))
(defn if-consequent [exp] (nth exp 2))
(defn if-alternative [exp] (nth exp 3))

(defn eval-if-form [env exp]
  (let [predicate (if-predicate exp)
        consequent (if-consequent exp)
        alternative (if-alternative exp)]
    (if (scheme-true? (scheme-eval env predicate))
      (scheme-eval env consequent)
      (scheme-eval env alternative))))

(comment
  (let [empty-env (env-create {} nil)]
    (println (eval-if-form empty-env '(if true 1 2)))
    (println (eval-if-form empty-env '(if false 1 2)))))

;; ----
;; Cond

(def cond-exp-pairs second)
(def cond-test-exp first)
(def cond-body-exp second)
(def cond-else-exp? (partial = 'else))
(def cond-form? (partial tag-of? 'cond))

(defn cond-form->if-form [exp]
  (->> (cond-exp-pairs exp)
       reverse
       (reduce
         (fn [ret pair]
           (let [test-exp (cond-test-exp pair)
                 body-exp (cond-body-exp pair)]
             (if (cond-else-exp? test-exp)
               body-exp
               (list 'if
                     test-exp
                     body-exp
                     ret))))
         false)))

(comment
  (cond-form->if-form
    '(cond (((foo? a) (a-answer))
            ((foo? b) (b-answer)))))
  (cond-form->if-form
    '(cond (((foo? a) (a-answer))
            ((foo? b) (b-answer))
            (else (else-answer))))))

(defn eval-cond-form [env exp]
  (scheme-eval env (cond-form->if-form exp)))

(comment
  (let [empty-env (env-create {} nil)]
    (println (eval-cond-form empty-env '(cond ((true 1) (else 2)))))
    (println (eval-cond-form empty-env '(cond ((false 1) (else 2)))))))


;; ----
;; Definition

(def definition? (partial tag-of? 'define))
(def definition-val #(nth % 2))

(def variable-definition? (comp variable? second))
(defn definition-variable [exp]
  (let [v (second exp)]
    (assert (variable? v) (format "Expected %s to be a variable" v))
    v))

(defn eval-definition-variable [env exp]
  (let [variable (definition-variable exp)
        val-exp (definition-val exp)]
    (env-define!
      env
      variable
      (scheme-eval env val-exp))
    variable))

(comment
  (let [exp '(define foo 1)]
    (println (definition? exp))
    (println (variable-definition? exp))
    (println (definition-variable exp))
    (println (definition-val exp))))

(def fn-definition?  (comp seq? second))
(def fn-definition-variable (comp first second))
(def fn-definition-args (comp rest second))

(comment
  (let [exp '(define (foo a) (+ a 1))]
    (println (definition? exp))
    (println (fn-definition? exp))
    (println (fn-definition-variable exp))
    (println (fn-definition-args exp))
    (println (definition-val exp))))

(defn eval-definition-fn [env exp]
  (scheme-eval
    env
    (list 'define
          (fn-definition-variable exp)
          (list 'lambda (fn-definition-args exp) (definition-val exp)))))

(defn eval-definition
  "We support the following function definitions:

  (define foo a)
  Assocs 'foo to a

  (define (foo a) (+ a b))
  Sugar for variable definition
  where the value becomes a lambda.

  Equivalent too:
  (define foo (lambda (a) (+ a b)))
  "
  [env exp]
  (cond
    (fn-definition? exp)
    (eval-definition-fn env exp)

    (variable-definition? exp)
    (eval-definition-variable env exp)

    :else
    (throw (Exception. (format "Unexpected define: %s" exp)))))

(comment
  (let [env (env-create-root)]
    (println (eval-definition env '(define foo 1)))
    (println (eval-definition env '(define (bar a) (+ a 1))))
    (println (scheme-eval env '(bar foo)))
    env))

;; ----
;; Unbind

(def unbind? (partial tag-of? 'unbind!))
(def unbind-var second)

(defn eval-unbind [env exp]
  (let [sym (unbind-var exp)
        env' (find-env-for-sym env sym)]
    (when env'
      (env-unbind! env' (unbind-var exp))
      true)))

(comment
  (let [env (env-create-root)]
    (scheme-eval env '(define foo "moop"))
    (println (scheme-eval env 'foo))
    (println (scheme-eval env '(unbind! foo)))
    (println (scheme-eval env 'foo))))

;; ------------
;; lambda

(def lambda? (partial tag-of? 'lambda))
(defn lambda-vars [exp]
  (let [syms (second exp)]
    (assert (every? variable? syms)
            (format "Expected %s to be all variables" syms))
    syms))

(def lambda-body #(nth % 2))
(comment
  (let [exp '(lambda (a b) (+ a b))]
    (println (lambda? exp))
    (println (lambda-vars exp))
    (println (lambda-body exp))))

(defn eval-lambda [env exp]
  (let [vars (lambda-vars exp)
        body (lambda-body exp)]
    (fn [& xs]
      (let [fn-env (env-create (zip-kv vars xs) env)]
        (scheme-eval fn-env body)))))

(comment
  (scheme-eval
    (env-create-root)
    '((lambda (a b) (+ a b)) 1 2)))

;; ------------
;; application

(def application? seq?)
(def application-operator first)
(def application-operands rest)

(comment
  (let [exp '(+ 1 2)]
    (println (application? exp))
    (println (application-operator exp))
    (println (application-operands exp))))

(defn eval-application [env exp]
  (let [operator (application-operator exp)
        operands (application-operands exp)]
    (apply
      (scheme-eval env operator)
      (map (partial scheme-eval env) operands))))

(comment
  (eval-application (env-create-root) '(+ 1 (* 2 2))))

;; ------------
;; do

(def do-form? (partial tag-of? 'do))
(def do-bodies rest)
(defn eval-do [env exp]
  (->> (do-bodies exp)
       (map (partial scheme-eval env))
       doall
       last))

(comment
  (eval-do (env-create-root)
           '(do
              (define a 1)
              (+ a 1))))

;; ------------
;; and/or

(defn and-form->if-form [clauses]
  (let [head (first clauses)
        tail (rest clauses)]
    (list 'if
          (list 'false? head)
          false
          (if (nil? (seq tail))
            head
            (and-form->if-form tail)))))

(def and-form? (partial tag-of? 'and))
(defn eval-and-form [env exp]
  (scheme-eval env (and-form->if-form (rest exp))))

(def or-form? (partial tag-of? 'or))

(defn or-form->if-form [clauses]
  (let [head (first clauses)
        tail (rest clauses)]
    (if (nil? head)
      false
      `(~(symbol "let") ((head# ~head))
         (if head# head# ~(or-form->if-form tail))))))

(defn eval-or-form [env exp]
  (scheme-eval env (or-form->if-form (rest exp))))

(comment
  (do
    (println (scheme-eval (env-create-root) '(and (= 1 2) 4)))
    (println (scheme-eval (env-create-root) '(and (= 2 2) 4)))
    (println (scheme-eval (env-create-root) '(or (= 1 2) 4)))
    (println (scheme-eval (env-create-root) '(or 4 (= 2 3))))
    (println (scheme-eval (env-create-root) '(or (= 1 2) (= 2 3))))))

;; ------------
;; let

(def let-form? (partial tag-of? 'let))
(defn let-var-names [exp] (map first (second exp)))
(defn let-var-values [exp] (map second (second exp)))
(def let-bodies (partial drop 2))

(defn eval-let
  "let transforms:

  (let ((a 1)
        (b 2))
    (+ a b)
    (* a b))

  into a lambda invocation:

  ((lambda (a b)
           (do (+ a b)
               (* a b)))
   1 2)"
  [env exp]
  (let [var-names (let-var-names exp)
        var-values (let-var-values exp)
        bodies (let-bodies exp)]
    (scheme-eval
      env
      (cons (list 'lambda
                  var-names
                  (cons 'do bodies))
            (map (partial scheme-eval env) var-values)))))
(comment
  (let [exp '(let ((a 1)
                   (b 1))
               (define x (+ a b))
               (+ x 1))]
    (println (let-form? exp))
    (println (let-var-names exp))
    (println (let-var-values exp))
    (println (let-bodies exp))
    (scheme-eval (env-create-root) exp)))

;; ------------
;; let*

(def let*-vars second)
(def let*-bodies #(nth % 2))
(def let*-form? (partial tag-of? 'let*))

(defn let*-form->nested-let [exp]
  (reduce (fn [acc var-def]
            (list 'let (list var-def)
                  acc))
          (let*-bodies exp)
          (reverse (let*-vars exp))))

(defn eval-let* [env exp]
  (scheme-eval env (let*-form->nested-let exp)))

(comment
  (println (scheme-eval (env-create-root)
                        '(let* ((x 3) (y (+ x 2)) (z (+ x y 5)))
                           (* x z)))))

;; ------------
;; scheme-eval

(defn scheme-eval [env exp]
  (cond
    (self-evaluating? exp)
    exp

    (variable? exp)
    (lookup-variable-value env exp)

    (if-form? exp)
    (eval-if-form env exp)

    (cond-form? exp)
    (eval-cond-form env exp)

    (and-form? exp)
    (eval-and-form env exp)

    (or-form? exp)
    (eval-or-form env exp)

    (definition? exp)
    (eval-definition env exp)

    (unbind? exp)
    (eval-unbind env exp)

    (lambda? exp)
    (eval-lambda env exp)

    (do-form? exp)
    (eval-do env exp)

    (let-form? exp)
    (eval-let env exp)

    (let*-form? exp)
    (eval-let* env exp)

    (application? exp)
    (eval-application env exp)

    :else
    "Not supported"))

(defn exit? [s]
  (= s 'exit))

(defn repl-loop
  ([] (repl-loop (env-create-root)))
  ([env]
   (println (str
              "Hello! Type something in, and get ready to repl. \n"
              "Type 'exit' to quit"
              "\n"
              "🔥 >"))
   (loop []
     (let [form (read-string (read-line))]
       (if (exit? form)
         (println "👋🏼 Goodbye")
         (do
           (pprint/pprint (scheme-eval env form))
           (recur)))))))

(defn bootstrap-repl [forms]
  (let [env (env-create-root)]
    (doseq [form forms] (scheme-eval env form))
    (repl-loop env)))

(comment
  (bootstrap-repl
    ['(define x 1)
     '(define y 1)
     '(define f (lambda (a b) (* a b)))]))
