#lang rosette/safe

; The execution of a transaction is defined as the change
; of the account state from S to S'.
; Each transaction will be executed sequentially. For example,
; the first transaction will change the account state from S to
; S_0 (if successful). Then, the second transaction will take S_0
; as its initial state, and change the account state to S_1
; (if successful). If any transaction failed, the end account state
; will be rolled back to S.

(define (txn-evel account-state txn-group)
  account-state)
