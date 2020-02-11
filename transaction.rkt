#lang rosette/safe

; The execution of a transaction group is defined as the change
; of the account state from S to S'.
; Transactions in an atomic transaction group will be executed sequentially.
; For example, the first transaction will change the account state from S to
; S_0 (if successful). Then, the second transaction will take S_0
; as its initial state, and change the account state to S_1
; (if successful). If any transaction failed, the end account state
; will be rolled back to S.

; eval single transaction
(define (txn-eval account-universe current-round)
  ; TODO: conditions that the evaluation can go through
  account-universe)

; eval a transaction group with error
(define (txn-group-eval-with-error account-universe current-round txn-group)
  (if (empty? txn-group)
      account-universe
      (let* ([txn (car txn-group)]
             [result (txn-eval account-universe current-round)])
        (if (not result)
            #f
            (txn-group-eval-with-error result current-round (cdr txn-group))))))

; eval a transaction group
(define (txn-group-eval account-universe current-round txn-group)
  (let ([result (txn-group-eval-with-error account-universe current-round txn-group)])
    (if (not result)
        account-universe ; roll-back if evaluate to #f
        result)))