#lang rosette/safe

(require "ledger.rkt" "symbolic.rkt" "config.rkt")

; We verify several safety property of the implemented racket verifier itself:
; 1. After a transaction, the total amount of Algos in the system should be unchanged
; 2. After a transaction, the total amount of any asset in the system should be unchanged

; computing total algos
(define (total-algos state)
  (foldl + 0
         (map account-state-balance (ledger-state-accounts state))))

(define (total-asset state asset)
  (foldl + 0
         (map (λ (ac) (list-ref (account-state-assets ac) asset)) (ledger-state-accounts state))))

; for any payment transaction, total algo never changed
(define sym-ledger-state
  (ledger-state (gen-sym-account-states) (list (gen-sym-lease) (gen-sym-lease))))

(define (ledger-precondition)
  (assert (let ([ta (total-algos sym-ledger-state)])
          (&& (> ta 0)
              (< ta 10000000000)))))

(define sym-txn
  (gen-sym-txn (list)))

; compute the symbolic output
(define result-ledger-state
  (txn-group-eval sym-ledger-state (gen-sym-round) (list sym-txn) (gen-sym-global-params)))

; now check the property, expect unsat
(ledger-precondition)
(verify (= (total-algos sym-ledger-state)
           (total-algos result-ledger-state)))

(define-symbolic asset integer?)
(assert (>= asset 0))
(assert (< asset asset-capacity))
(verify (= (total-asset sym-ledger-state asset)
           (total-asset result-ledger-state asset)))


