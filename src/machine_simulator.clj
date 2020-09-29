(ns machine-simulator)

(declare make-execution-proc)

; build instructions
; -------------

(def instruction-label first)
(def instruction-body second)
(def body-tag first)

(defn ->label+instruction [raw-instructions]
  (reduce (fn [res part]
            (if (symbol? part)
              (conj res [part nil])
              (let [last-label (instruction-label (last res))]
                (conj res [last-label part]))))
          []
          raw-instructions))

(comment
  (->label+instruction '((assign)
                         label-one
                         (test)
                         (branch)
                         label-two
                         (goto))))

(defn make-instruction-sequence
  "Takes a list of raw instructions
  '(label-one
    (assign foo (const 1))
    (goto label-one))

    and transforms them into assembled instruction tuples:

    '(
      [label-one (assign foo (const 1)) f]
      [label-one (goto label-one) f]
     )

     f are procedures, which transform a machine
  "
  [raw-instructions]
  (mapv (fn [ins]
          (into ins [(make-execution-proc
                       (instruction-body ins))]))
        (->label+instruction raw-instructions)))

(comment
  (make-instruction-sequence
    '((assign foo (const 1))
      label-one
      (test (op =) (const 1) (reg foo))
      (branch (label label-two))
      label-two)))

(defn tag-of? [sym body] (= sym (body-tag body)))

; assign data model
; -------------

(def assign-reg-name
  "(assign foo ...)"
  second)

(comment (assign-reg-name '(assign foo (const 1))))

(def assign-value-exp
  "((op *) (const 1) (reg b) ..."
  (partial drop 2))

(comment (assign-value-exp '(assign foo (op *) (const 1) (const 2))))
(comment (assign-value-exp '(assign foo (const 1))))

(def operation-exp?
  "determines if assign using an `op`"
  (comp (partial tag-of? 'op) #(nth % 2)))

(def assign-operation-exp (partial drop 2))
(comment (assign-operation-exp '(assign foo (op *) (const 3) (const 2))))

(def assign-primitive-exp #(nth % 2))

(operation-exp? '(assign foo (op *) (const 1) (const 2)))

(defn label-idx [transformed-instructions label]
  (->> transformed-instructions
       (map-indexed (fn [i x] [i (instruction-label x)]))
       (filter (comp (partial = label) last))
       ffirst))

(comment
  (label-idx '[[nil (assign)] [label-one nil] [label-two nil]]
             'label-two))

; make expressions
; -------------

(defn make-primitive-exp [prim-exp]
  (fn [{:keys [registry-map instructions] :as data}]
    (let [res (condp tag-of? prim-exp
                'const
                (second prim-exp)
                'reg
                (get registry-map (second prim-exp))
                'label
                (label-idx instructions (second prim-exp)))]
      res)))

(comment
  (let [f (make-assign-proc '(assign foo (const 3)))]
    (f {:registry-map {'foo 0}
        :op-map {}
        :pc 0}))
  (let [f (make-assign-proc '(assign foo (reg bar)))]
    (f {:registry-map {'foo 0 'bar "hello"}
        :op-map {}
        :pc 0
        :instructions []}))
  (let [f (make-assign-proc '(assign foo (label bar)))]
    (f {:registry-map {'foo 0}
        :op-map {}
        :pc 0
        :instructions '[[nil (assign)] [bar nil]]})))

(def operation-sym (comp second first))
(def operation-args rest)
(defn make-operation-exp [value-exp]
  (let [op-sym (operation-sym value-exp)
        op-arg-fns (map make-primitive-exp (operation-args value-exp))]
    (fn [{:keys [op-map] :as data}]
      (let [op-fn (get op-map op-sym)
            evaled-args (map (fn [f] (f data)) op-arg-fns)]
        (apply op-fn evaled-args)))))

(comment
  (let [f (make-assign-proc '(assign foo (op *) (const 3) (reg bar)))]
    (f {:registry-map {'bar 2 'foo 0}
        :pc 0
        :op-map {'* *}
        :instructions []})))

; assign
; -------------
(defn make-assign-proc [body]
  (let [reg-name (assign-reg-name body)
        value-proc (if (operation-exp? body)
                     (make-operation-exp (assign-operation-exp body))
                     (make-primitive-exp (assign-primitive-exp body)))]
    (fn [data]
      (let [new-value (value-proc data)]
        (-> data
            (assoc-in [:registry-map reg-name] new-value)
            (update :pc inc))))))

(def test-condition rest)

; test
; -------------
(defn make-test-proc [body]
  (let [condition-proc (make-operation-exp (test-condition body))]
    (fn [data]
      (-> data
          (assoc :flag (condition-proc data))
          (update :pc inc)))))

(comment
  (let [f (make-test-proc '(test (op =) (const 3) (reg bar)))]
    (f {:registry-map {'bar 2 'foo 0}
        :pc 0
        :op-map {'= =}
        :instructions []}))
  (let [f (make-test-proc '(test (op =) (const 3) (reg bar)))]
    (f {:registry-map {'bar 3 'foo 0}
        :pc 0
        :op-map {'= =}
        :instructions []})))

; branch
; -------------
(def branch-dest second)
(defn make-branch-proc [body]
  (let [dest-fn (make-primitive-exp (branch-dest body))]
    (fn [data]
      (if (:flag data)
        (assoc data :pc (dest-fn data))
        (update data :pc inc)))))

(comment
  (let [f (make-branch-proc '(branch (label foo)))]
    (f {:registry-map {}
        :pc 0
        :flag false
        :op-map {'= =}
        :instructions '[[nil (assign)] [nil (assign)] [foo nil]]}))
  (let [f (make-branch-proc '(branch (label foo)))]
    (f {:registry-map {'bar 3 'foo 0}
        :pc 0
        :flag true
        :op-map {'= =}
        :instructions '[[nil (assign)] [nil (assign)] [foo nil]]})))

; goto
; -------------
(def goto-dest second)
(defn make-goto-proc [body]
  (let [dest-fn (make-primitive-exp (goto-dest body))]
    (fn [data] (assoc data :pc (dest-fn data)))))

(comment
  (let [f (make-goto-proc '(goto (label foo)))]
    (f {:registry-map {}
        :pc 0
        :flag false
        :op-map {'= =}
        :instructions '[[nil (assign)] [nil (assign)] [foo nil]]}))
  (let [f (make-goto-proc '(goto (reg foo)))]
    (f {:registry-map {'foo 3}
        :pc 0
        :flag false
        :op-map {'= =}
        :instructions '[[nil (assign)] [nil (assign)] [foo nil]]})))

; save
; -------------

(def save-reg-name second)
(defn make-save-proc [body]
  (let [reg-name (save-reg-name body)]
    (fn [data]
      (let [reg-v (get-in data [:registry-map reg-name])]
        (-> data
            (update :stack conj reg-v)
            (update :pc inc))))))
(comment
  (let [f (make-save-proc '(save foo))]
    (f {:registry-map {'foo 3}
        :pc 0
        :flag false
        :op-map {'= =}
        :instructions []
        :stack []})))

; restore
; -------------
(def restore-reg-name second)
(defn make-restore-proc [body]
  (let [reg-name (restore-reg-name body)]
    (fn [data]
      (let [v (last (:stack data))]
        (-> data
            (update :stack pop)
            (assoc-in [:registry-map reg-name] v)
            (update :pc inc))))))
(comment
  (let [f (make-restore-proc '(restore foo))]
    (f {:registry-map {'foo 3}
        :pc 0
        :flag false
        :op-map {'= =}
        :instructions []
        :stack [10]})))

; nil
; -------------
(defn make-nil-proc []
  (fn [data] (update data :pc inc)))

(comment
  (let [f (make-nil-proc)]
    (f {:registry-map {'foo 3}
        :pc 0
        :flag false
        :op-map {'= =}
        :instructions []
        :stack [10]})))

; analyze
; -------------

(def assign? (partial tag-of? 'assign))
(def test? (partial tag-of? 'test))
(def branch? (partial tag-of? 'branch))
(def goto? (partial tag-of? 'goto))
(def save? (partial tag-of? 'save))
(def restore? (partial tag-of? 'restore))

(defn make-execution-proc [body]
  (cond
    (assign? body)
    (make-assign-proc body)

    (test? body)
    (make-test-proc body)

    (branch? body)
    (make-branch-proc body)

    (goto? body)
    (make-goto-proc body)

    (save? body)
    (make-save-proc body)

    (restore? body)
    (make-restore-proc body)

    (nil? body)
    (make-nil-proc)

    :else
    (throw (Exception. (format "Unsupported instruction label %s" body)))))


; run
; -------------

(def instruction-fn #(nth % 2))

(defn run [registry-map op-map raw-instructions]
  (loop [data {:registry-map registry-map
               :op-map op-map
               :stack []
               :pc 0
               :flag nil
               :instructions (make-instruction-sequence raw-instructions)}]
    (if-let [f (instruction-fn
                 (nth (:instructions data) (:pc data) nil))]
      (recur (f data))
      data)))

(def default-op-map {'* * '/ /
                     '> > '>= >=
                     '< < '<= <=
                     '+ + '- -
                     '= =})
(comment
  (:registry-map (run
                   {'res 1 'counter 3 'base 10}
                   default-op-map
                   '(
                      loop

                      (test (op =) (reg counter) (const 0))
                      (branch (label done))
                      (assign res (op *) (reg base) (reg res))
                      (assign counter (op -) (reg counter) (const 1))
                      (goto (label loop))

                      done))))
