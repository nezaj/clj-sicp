(ns machine-simulator)

(def instruction-label first)
(def instruction-body second)

(def body-tag first)

(defn transform-instructions [instructions]
  (reduce (fn [res part]
            (if (symbol? part)
              (conj res [part nil])
              (let [last-label (instruction-label (last res))]
                (conj res [last-label part]))))
          []
          instructions))

(declare make-execution-proc)

(defn tag-of? [sym body] (= sym (body-tag body)))
(def assign? (partial tag-of? 'assign))
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

(def assign-primitive-exp #(nth % 2))

(operation-exp? '(assign foo (op *) (const 1) (const 2)))

(defn make-operation-exp [value-exp]
  (fn [data]))

(defn label-idx [transformed-instructions label]
  (->> transformed-instructions
       (map-indexed (fn [i x] [i (instruction-label x)]))
       (filter (comp (partial = label) last))
       ffirst))

(comment
  (label-idx '[[nil (assign)] [label-one nil] [label-two nil]]
             'label-two))

(defn make-primitive-exp [prim-exp]
  (fn [{:keys [registry-map instructions] :as data}]
    (condp #(tag-of? %1 %2) prim-exp
      'const
      (second prim-exp)
      'reg
      (get registry-map (second prim-exp))
      'label
      (label-idx instructions (second prim-exp)))))

(defn make-assign-proc [body]
  (let [reg-name (assign-reg-name body)
        value-proc (if (operation-exp? body)
                     (make-operation-exp (assign-operation-exp body))
                     (make-primitive-exp (assign-primitive-exp body)))]
    (fn [{:keys [registry-map] :as data}]
      (let [new-value (value-proc data)]
        (-> registry-map
            (assoc reg-name new-value)
            (update :pc inc))))))

(comment
  (let [f (make-assign-proc '(assign foo (const 3)))]
    (f {:registry-map {:pc 0 'foo 0}
        :op-map {}
        :instructions []}))
  (let [f (make-assign-proc '(assign foo (reg bar)))]
    (f {:registry-map {:pc 0 'foo 0 'bar "hello"}
        :op-map {}
        :instructions []}))
  (let [f (make-assign-proc '(assign foo (label bar)))]
    (f {:registry-map {:pc 0 'foo 0}
        :op-map {}
        :instructions '[[nil (assign)] [bar nil]]})))

(defn make-execution-proc [transformed-instruction]
  (let [body (instruction-body transformed-instruction)]
    (cond
      (assign? body)
      (make-assign-proc body)
      :else
      (throw (Exception. "Unsupported instruction label")))))

(comment
  (transform-instructions '((assign)
                            label-one
                            (test)
                            (branch)
                            label-two
                            (goto))))