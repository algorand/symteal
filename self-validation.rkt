#lang rosette

(require "ledger.rkt" "symbolic.rkt" "config.rkt"
         rackunit rackunit/text-ui rosette/lib/roseunit
         rosette/solver/smt/boolector)

(current-solver (boolector))

(current-bitwidth 64)

; We verify several safety property of the implemented racket verifier itself:
; 1. After a transaction, the total amount of Algos in the system should be unchanged
; 2. After a transaction, the total amount of any asset in the system should be unchanged

(define (ledger-precondition)
  (assert (let ([ta (total-algos sym-ledger-state)])
          (&& (> ta 0)
              (< ta algo-supply)))))

(define sym-txn
  (gen-sym-txn (list)))

(define sym-ledger-state
  (gen-sym-ledger-state))

; compute the symbolic output
(define result-ledger-state
  (txn-group-eval sym-ledger-state (gen-sym-round) (list sym-txn) (gen-sym-global-params)))

(ledger-precondition)

; verify algo balance invariant
; cpu time: 56380 real time: 56727 gc time: 29262
(time (verify (assert (= (total-algos sym-ledger-state)
                   (total-algos result-ledger-state)))))

(define-symbolic asset integer?)
(assert (>= asset 0))
(assert (< asset asset-capacity))

; verify asset balance invariant
; cpu time: 692190 real time: 695380 gc time: 419965
(time (verify (assert (= (total-asset sym-ledger-state asset)
                         (total-asset result-ledger-state asset)))))

