(ns amb-interpreter
  (:require [clojure.pprint :as pprint]))

(declare amb-eval)

(def empty-list? (partial = ()))
(defn self-evaluating? [exp]
  ((some-fn
     number?
     string?
     boolean?
     empty-list?) exp))

(defn variable? [exp]
  (symbol? exp))

(def amb-false? (partial = false))
(def amb-true? (comp not amb-false?))

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

(defn with-continuations [f]
  (fn [succ-cb fail-cb & xs]
    (succ-cb (apply f xs) fail-cb)))

(defn env-create-root []
  (let [prims (->> [['+ +]
                    ['- -]
                    ['* *]
                    ['/ /]
                    ['= =]
                    ['> >]
                    ['>= >=]
                    ['< <]
                    ['<= <=]
                    ['true? amb-true?]
                    ['false? amb-false?]
                    ['cons cons]
                    ['car first]
                    ['cdr rest]
                    ['not false?]
                    ['null? nil?]
                    ['list list]
                    ['sqrt (fn [x] (Math/sqrt x))]
                    ['floor (fn [x] (Math/floor x))]
                    ['integer? integer?]
                    ['int int]]
                   (map (fn [[sym f]] [sym (with-continuations f)]))
                   (into {}))]
    (env-create prims nil)))

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

(defn lookup-variable-value [env sym succ-cb fail-cb]
  (if-let [env' (find-env-for-sym env sym)]
    (succ-cb (env-sym-value env' sym) fail-cb)
    (throw (Exception. (format "unbound variable: %s" sym)))))

;; ----
;; If

(def if-form? (partial tag-of? 'if))
(defn if-predicate [exp] (second exp))
(defn if-consequent [exp] (nth exp 2))
(defn if-alternative [exp] (nth exp 3))

(defn eval-if-form [env exp succ-cb fail-cb]
  (let [predicate (if-predicate exp)
        consequent (if-consequent exp)
        alternative (if-alternative exp)]
    (amb-eval env predicate
              (fn [evaled-p fail-cb2]
                 (if (amb-true? evaled-p)
                   (amb-eval env consequent succ-cb fail-cb2)
                   (amb-eval env alternative succ-cb fail-cb2)))
              fail-cb)))

;; ----
;; amb

(def amb-form? (partial tag-of? 'amb))
(def amb-options rest)

(defn eval-amb-form [env exp succ-cb fail-cb]
  (letfn [(try-next [opts]
            (let [next-opt (first opts)]
              (if-not next-opt
                (fail-cb)
                (amb-eval
                  env
                  next-opt
                  succ-cb
                  (fn [] (try-next (rest opts)))))))]
    (try-next (amb-options exp))))

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

(defn eval-cond-form [env exp succ-cb fail-cb]
  (amb-eval env (cond-form->if-form exp) succ-cb fail-cb))


;; ----
;; Definition

(def definition? (partial tag-of? 'define))
(def definition-val #(nth % 2))

(def variable-definition? (comp variable? second))
(defn definition-variable [exp]
  (let [v (second exp)]
    (assert (variable? v) (format "Expected %s to be a variable" v))
    v))

(defn eval-definition-variable [env exp succ-cb fail-cb]
  (let [variable (definition-variable exp)
        val-exp (definition-val exp)]
    (amb-eval
      env val-exp
      (fn [evaled-v fail-cb2]
        (env-define! env variable evaled-v)
        (succ-cb
          variable
          fail-cb2))
      fail-cb)))

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

(defn eval-definition-fn [env exp succ-cb fail-cb]
  (amb-eval
    env
    (list 'define
          (fn-definition-variable exp)
          (list 'lambda (fn-definition-args exp) (definition-val exp)))
    succ-cb
    fail-cb))

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
  [env exp succ-cb fail-cb]
  (cond
    (fn-definition? exp)
    (eval-definition-fn env exp succ-cb fail-cb)

    (variable-definition? exp)
    (eval-definition-variable env exp succ-cb fail-cb)

    :else
    (throw (Exception. (format "Unexpected define: %s" exp)))))

;; ----
;; Unbind

(def unbind? (partial tag-of? 'unbind!))
(def unbind-var second)

(defn eval-unbind [env exp succ-cb fail-cb]
  (let [sym (unbind-var exp)
        env' (find-env-for-sym env sym)]
    (succ-cb
      (when env'
        (env-unbind! env' (unbind-var exp))
        true)
      fail-cb)))

;; ------------
;; lambda

(def lambda? (partial tag-of? 'lambda))
(defn lambda-vars [exp]
  (let [syms (second exp)]
    (assert (every? variable? syms)
            (format "Expected %s to be all variables" syms))
    syms))

(def lambda-body #(nth % 2))

(defn eval-lambda [env exp succ-cb fail-cb]
  (let [vars (lambda-vars exp)
        body (lambda-body exp)]
    (succ-cb
      (fn [succ-cb fail-cb & xs]
        (let [fn-env (env-create (zip-kv vars xs) env)]
          (amb-eval fn-env body succ-cb fail-cb)))
      fail-cb)))

;; ------------
;; do

(def do-form? (partial tag-of? 'do))
(def do-bodies rest)

(defn eval-sequence
  ([env forms succ-cb fail-cb]
   (eval-sequence env forms succ-cb fail-cb []))
  ([env forms succ-cb fail-cb evaled-forms]
   (let [a (first forms)
         next-forms (rest forms)]
     (assert (seq forms) (format "Expected a to be a form. forms = %s" forms))
     (amb-eval
       env
       a
       (fn [evaled-a fail-cb2]
         (let [evaled-forms' (into evaled-forms [evaled-a])]
           (if (seq next-forms)
             (eval-sequence env next-forms succ-cb fail-cb2 evaled-forms')
             (succ-cb evaled-forms' fail-cb2))))
       fail-cb))))

(defn eval-do [env exp succ-cb fail-cb]
  (eval-sequence
    env
    (do-bodies exp)
    (fn [vs fail-cb2]
      (succ-cb (last vs) fail-cb2)) fail-cb))


;; ------------
;; application

(def application? seq?)
(def application-operator first)
(def application-operands rest)

(defn eval-application [env exp succ-cb fail-cb]
  (let [operator (application-operator exp)
        operands (application-operands exp)]
    (amb-eval
      env
      operator
      (fn [evaled-operator fail-cb2]
        (eval-sequence env operands
                       (fn [evaled-operands fail-cb3]
                         (apply evaled-operator (into [succ-cb fail-cb3] evaled-operands)))
                       fail-cb2))
      fail-cb)))

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
(defn eval-and-form [env exp succ-cb fail-cb]
  (amb-eval env (and-form->if-form (rest exp)) succ-cb fail-cb))

(def or-form? (partial tag-of? 'or))

(defn or-form->if-form [clauses]
  (let [head (first clauses)
        tail (rest clauses)]
    (if (nil? head)
      false
      `(~(symbol "let") ((head# ~head))
         (if head# head# ~(or-form->if-form tail))))))

(defn eval-or-form [env exp succ-cb fail-cb]
  (amb-eval env (or-form->if-form (rest exp)) succ-cb fail-cb))

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
  [env exp succ-cb fail-cb]
  (let [var-names (let-var-names exp)
        var-values (let-var-values exp)
        bodies (let-bodies exp)]
    (eval-sequence
      env
      var-values
      (fn [evaled-vals fail-cb2]
        (amb-eval
          env
          (cons (list 'lambda
                      var-names
                      (cons 'do bodies))
                evaled-vals)
          succ-cb
          fail-cb2))
      fail-cb)))

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

(defn eval-let* [env exp succ-cb fail-cb]
  (amb-eval env (let*-form->nested-let exp) succ-cb fail-cb))

;; ------------
;; amb-eval

(defn amb-eval [env exp succ-cb fail-cb]
  (cond
    (self-evaluating? exp)
    (succ-cb exp fail-cb)

    (variable? exp)
    (lookup-variable-value env exp succ-cb fail-cb)

    (if-form? exp)
    (eval-if-form env exp succ-cb fail-cb)

    (amb-form? exp)
    (eval-amb-form env exp succ-cb fail-cb)

    (cond-form? exp)
    (eval-cond-form env exp succ-cb fail-cb)

    (and-form? exp)
    (eval-and-form env exp succ-cb fail-cb)

    (or-form? exp)
    (eval-or-form env exp succ-cb fail-cb)

    (definition? exp)
    (eval-definition env exp succ-cb fail-cb)

    (unbind? exp)
    (eval-unbind env exp succ-cb fail-cb)

    (lambda? exp)
    (eval-lambda env exp succ-cb fail-cb)

    (do-form? exp)
    (eval-do env exp succ-cb fail-cb)

    (let-form? exp)
    (eval-let env exp succ-cb fail-cb)

    (let*-form? exp)
    (eval-let* env exp succ-cb fail-cb)

    (application? exp)
    (eval-application env exp succ-cb fail-cb)

    :else
    "Not supported"))

(defn repl-loop
  ([] (repl-loop (env-create-root)))
  ([env]
   (println (str
              "Hello! Type something in, and get ready to repl. \n"
              "Type 'exit' to quit"
              "\n"
              "🔥 >"))
   (letfn [(internal-loop [try-again]
             (let [input (read-string (read-line))]
               (condp = input
                 'try-again (try-again)
                 'exit nil
                 (amb-eval
                   env
                   input
                   (fn [val next-alternative]
                     (println ">" val)
                     (internal-loop next-alternative))
                   (fn []
                     (println "> No more values")
                     (repl-loop))))))]
     (internal-loop
       (fn []
         (println "Nothing to try")
         (repl-loop env))))))

(defn bootstrap-repl [forms]
  (let [env (env-create-root)]
    (eval-sequence
      env
      forms
      (fn [_ fail-cb]
        (repl-loop env))
      (fn []
        (println "couldn't bootstrap!")))))

(comment
  (bootstrap-repl
    '[(define xs (cons 1 (cons 2 (cons 3 ()))))
      (define (require p)
              (if (false? p)
                (amb)
                true))
      (define (an-element-of items)
              (do
                (require (not (= items ())))
                (amb (car items) (an-element-of (cdr items)))))
      (an-element-of xs)]))


(comment
  (repl-loop))

(comment
  (do
    (define xs (cons 1 (cons 2 (cons 3 ()))))
    (define (require p) (if (false? p) (amb) true))
    (define (an-element-of items) (do (require (not (= items ()))) (amb (car items) (an-element-of (cdr items)))))))
