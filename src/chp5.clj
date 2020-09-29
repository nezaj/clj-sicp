(ns chp5)

;; Registers
; 5.1 + 5.2
'(controller
  do
    (test (op >) (reg counter) (reg n))
    (branch (label done))
    (assign product (op *) (reg product) (reg counter))
    (assign counter (op +) (reg counter) (const 1))
    (goto (label do))
  done)

; 5.4a
'(controller
    (assign continue (label exp-done))
   exp-loop
    (test (op =) (reg n) (const 0))
    (branch (label base-case))
    (save continue)
    (assign n (op -) (reg n) (const 1))
    (assign continue (label after-exp))
    (goto (label exp-loop))
   base-case
    (assign res (const 1))
    (goto (reg continue))
   after-exp
     (restore continue)
     (assign (reg res) (op *) (reg res) (reg base))
     (goto (reg continue))
   exp-done)

; 5.4b
'(controller
   do
      (test (op =) (reg counter) (const 0))
      (branch (label done))
      (assign res (op *) (reg base) (reg res))
      (assign counter (op -) (reg counter) (const 1))
   done)

;; Machine evaluator
; 5.23
; 5.24
; 5.25
; 5.25
; 5.26b
; 5.27
; 5.28
; 5.29

;; Compilation
; 5.31
; 5.32b
; 5.36
; 5.45
; 5.46