(ns machine-evaluator
  (:require [machine-simulator :as ms]))

(declare simulate-machine)

;; primitives

(defn tag-of? [tag exp]
  (= tag (first exp)))

(def empty-list? (partial = ()))
(def empty-list list)
(def empty-rest? (comp empty-list? rest))

(defn self-evaluating? [exp]
  ((some-fn
     number?
     string?
     boolean?
     empty-list?) exp))

(def variable? symbol?)

(def scheme-false? (partial = false))
(def scheme-true? (comp not scheme-false?))

(comment
  [(-> (simulate-machine 1) :registry-map)
   (-> (simulate-machine "hello") :registry-map)])

;; env

(defn env-exists? [env]
  (not (nil? env)))

(defn env-data [env]
  (:data env))

(defn env-define! [env k v]
  (swap! (env-data env) assoc k v))

(defn env-sym-bound? [env sym]
  (contains? @(env-data env) sym))

(defn env-sym-value [env sym]
  (get @(env-data env) sym))

(defn env-parent [env]
  (:parent env))

(defn env-create [data parent]
  {:data (atom data) :parent parent})

(defn env-create-root []
  (let [entries [['+ +]
                 ['- -]
                 ['* *]
                 ['/ /]
                 ['= =]
                 ['> >]
                 ['< <]
                 ['>= >=]
                 ['<= <=]
                 ['true? scheme-true?]
                 ['false? scheme-false?]]]
    (env-create (->> entries
                     (map (fn [[sym f]]
                            [sym
                             (with-meta f {:primitive-procedure? true})]))
                     (into {}))
                nil)))

(defn zip-kv [ks vs]
  (format "Incorrect arity: got %s but need %s"
          (count vs)
          (count ks))
  (into {} (map (fn [x y] [x y]) ks vs)))

(defn env-extend [env syms vals]
  (env-create (zip-kv syms vals) env))

(defn find-env-for-sym [env sym]
  (if-not (env-exists? env)
    nil
    (if (env-sym-bound? env sym)
      env
      (find-env-for-sym (env-parent env) sym))))

;; variable

(defn lookup-variable-value [env sym]
  (if-let [env' (find-env-for-sym env sym)]
    (env-sym-value env' sym)
    (throw (Exception. (format "unbound variable: %s" sym)))))

(comment
  (-> (simulate-machine '+) :registry-map))

;; definition

(def definition? (partial tag-of? 'define))
(def definition-val #(nth % 2))

(defn definition-variable [exp]
  (let [v (second exp)]
    (assert (variable? v) (format "Expected %s to be a variable" v))
    v))

(comment
  (-> (simulate-machine '(define x +)) :registry-map))

;; if-form
(def if-form? (partial tag-of? 'if))
(defn if-predicate [exp] (second exp))
(defn if-consequent [exp] (nth exp 2))
(defn if-alternative [exp] (nth exp 3))

(comment
  [(-> (simulate-machine '(if true 1 0)) :registry-map)
   (-> (simulate-machine '(if false 1 0)) :registry-map)])

;; lambda
(def lambda-form? (partial tag-of? 'lambda))
(defn lambda-vars [exp]
  (let [syms (second exp)]
    (assert (every? variable? syms)
            (format "Expected %s to be all variables" syms))
    syms))

(def lambda-body (partial drop 2))

(defn make-procedure [env params body]
  (with-meta (fn []
               {:env env :params params :body body})
             {:compound-procedure? true}))

(def compound-procedure? (comp :compound-procedure? meta))
(def primitive-procedure? (comp :primitive-procedure? meta))

(comment
  (-> (simulate-machine '(lambda (a) (+ a 1))) :registry-map))

;; application
(def application? seq?)
(def application-operator first)
(def application-operands rest)

(comment
  [(-> (simulate-machine '(+ 1 (+ 1 1))) :registry-map)
   (-> (simulate-machine '((lambda (a) (+ a 1)) 1)) :registry-map)
   (-> (simulate-machine
         '((lambda (start)
                  (define iter-fct
                          (lambda (product counter)
                                  (if (> counter start)
                                    product
                                    (iter-fct (* counter product)
                                               (+ counter 1)))))
                  (iter-fct 1 1))
          20))
       :registry-map)
   (-> (simulate-machine
         '((lambda (start)
                   (define fct
                           (lambda (n)
                                   (if (= n 1)
                                     n
                                     (* n (fct (- n 1))))))
                   (fct start))
           20))
       :registry-map)])


;; op-map
(def op-map
  {'self-evaluating?          self-evaluating?
   'variable?                 variable?
   'lookup-variable-value     lookup-variable-value
   'definition?               definition?
   'definition-variable       definition-variable
   'definition-val            definition-val
   'env-define!               env-define!
   'env-extend                env-extend
   'if-form?                  if-form?
   'if-predicate              if-predicate
   'if-consequent             if-consequent
   'if-alternative            if-alternative
   'scheme-true?              scheme-true?
   'scheme-false?             scheme-false?
   'lambda-form?              lambda-form?
   'lambda-body               lambda-body
   'lambda-vars               lambda-vars
   'make-procedure            make-procedure
   'application?              application?
   'application-operator      application-operator
   'application-operands      application-operands
   'empty-list                empty-list
   'empty-list?               empty-list?
   'first                     first
   'empty-rest?               empty-rest?
   'conj                      conj
   'rest                      rest
   'primitive-procedure?      primitive-procedure?
   'compound-procedure?       compound-procedure?
   'reverse                   reverse
   'apply-primitive-procedure apply
   'procedure-params          (fn [f] (:params (f)))
   'procedure-body            (fn [f] (:body (f)))
   'procedure-env             (fn [f] (:env (f)))})

(defn simulate-machine [exp]
  (ms/run
    {
     'exp      exp
     'val      nil
     'env      (env-create-root)
     'unev     nil
     'continue nil
     'arglist  nil
     'proc     nil}
    (merge ms/default-op-map op-map)
    '(
      (assign continue (label done))
      (goto (label eval-dispatch))

      ev-self-eval
      (assign val (reg exp))
      (goto (reg continue))

      ev-variable
      (assign val
              (op lookup-variable-value)
              (reg env)
              (reg exp))
      (goto (reg continue))

      ev-definition
      (assign unev (op definition-variable) (reg exp))
      (save unev)
      (assign exp (op definition-val) (reg exp))
      (save env)
      (save continue)
      (assign continue (label ev-definition-1))
      (goto (label eval-dispatch))

      ev-definition-1
      (restore continue)
      (restore env)
      (restore unev)
      (perform (op env-define!) (reg env) (reg unev) (reg val))
      (assign val (const :ok))
      (goto (reg continue))

      ev-if
      (save exp)
      (save env)
      (save continue)
      (assign continue (label ev-if-decide))
      (assign exp (op if-predicate) (reg exp))
      (goto (label eval-dispatch))

      ev-if-decide
      (restore continue)
      (restore env)
      (restore exp)
      (test (op scheme-true?) (reg val))
      (branch (label ev-if-consequent))
      (goto (label ev-if-alternative))

      ev-if-consequent
      (assign exp (op if-consequent) (reg exp))
      (goto (label eval-dispatch))

      ev-if-alternative
      (assign exp (op if-alternative) (reg exp))
      (goto (label eval-dispatch))

      ev-lambda
      (assign unev (op lambda-vars) (reg exp))
      (assign exp (op lambda-body) (reg exp))
      (assign val (op make-procedure) (reg env) (reg unev) (reg exp))
      (goto (reg continue))

      ev-application
      (save continue)
      (save env)
      (assign unev (op application-operands) (reg exp))
      (save unev)
      (assign exp (op application-operator) (reg exp))
      (assign continue (label ev-app-did-operator))
      (goto (label eval-dispatch))

      ev-app-did-operator
      (restore unev)
      (restore env)
      (assign arglist (op empty-list))
      (assign proc (reg val))
      (test (op empty-list?) (reg unev))
      (branch (label apply-dispatch))
      (save proc)
      (goto (label ev-app-operand-loop))

      ev-app-operand-loop
      (save arglist)
      (assign exp (op first) (reg unev))
      (test (op empty-rest?) (reg unev))
      (branch (label ev-app-last-arg))
      (save env)
      (save unev)
      (assign continue (label ev-app-accumulate-arg))
      (goto (label eval-dispatch))

      ev-app-accumulate-arg
      (restore unev)
      (restore env)
      (restore arglist)
      (assign arglist (op conj) (reg arglist) (reg val))
      (assign unev (op rest) (reg unev))
      (goto (label ev-app-operand-loop))

      ev-app-last-arg
      (assign continue (label ev-app-accumulate-last-arg))
      (goto (label eval-dispatch))

      ev-app-accumulate-last-arg
      (restore arglist)
      (assign arglist (op conj) (reg arglist) (reg val))
      (restore proc)
      (goto (label apply-dispatch))

      apply-dispatch
      (assign arglist (op reverse) (reg arglist))
      (test (op compound-procedure?) (reg proc))
      (branch (label compound-apply))
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-apply))
      (goto (label unknown-procedure-type))

      primitive-apply
      (assign val (op apply-primitive-procedure) (reg proc) (reg arglist))
      (restore continue)
      (goto (reg continue))

      compound-apply
      (assign unev (op procedure-params) (reg proc))
      (assign env (op procedure-env) (reg proc))
      (assign env (op env-extend) (reg env) (reg unev) (reg arglist))
      (assign unev (op procedure-body) (reg proc))
      (goto (label ev-sequence))

      ev-sequence
      (assign exp (op first) (reg unev))
      (test (op empty-rest?) (reg unev))
      (branch (label ev-sequence-last-exp))
      (save unev)
      (save env)
      (assign continue (label ev-sequence-continue))
      (goto (label eval-dispatch))

      ev-sequence-continue
      (restore env)
      (restore unev)
      (assign unev (op rest) (reg unev))
      (goto (label ev-sequence))

      ev-sequence-last-exp
      (restore continue)
      (goto (label eval-dispatch))

      unknown-procedure-type
      (assign val (const "Unknown procedure type!"))
      (goto (reg continue))

      unknown-expression-type
      (assign val (const "Unknown expression!"))
      (goto (reg continue))

      eval-dispatch
      (test (op self-evaluating?) (reg exp))
      (branch (label ev-self-eval))

      (test (op variable?) (reg exp))
      (branch (label ev-variable))

      (test (op definition?) (reg exp))
      (branch (label ev-definition))

      (test (op if-form?) (reg exp))
      (branch (label ev-if))

      (test (op lambda-form?) (reg exp))
      (branch (label ev-lambda))

      (test (op application?) (reg exp))
      (branch (label ev-application))

      (goto (label unknown-expression-type))

      done)))