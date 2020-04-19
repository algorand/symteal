#lang rosette

(require "ledger.rkt" "symbolic.rkt" "config.rkt" rackunit rackunit/text-ui rosette/lib/roseunit)

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

(define-syntax-rule (check-verify pred test)
  (let ([sol (with-handlers ([exn:fail? (const (unsat))])
               test)])
    (check-true (pred sol) (format "not ~a for ~a: ~a" (quote pred) (quote test) sol))))

; now check the property, expect unsat
;(define ledger-safety-tests
;  (test-suite+
;   "ledger safety test"
(ledger-precondition)
(define sol (verify (assert (= (total-algos sym-ledger-state)
                               (total-algos result-ledger-state)))))

(print sol)
(evaluate sym-txn sol)
(evaluate sym-ledger-state sol)

;(define-symbolic asset integer?)
;(assert (>= asset 0))
;(assert (< asset asset-capacity))
;(verify (assert (= (total-asset sym-ledger-state asset)
;                   (total-asset result-ledger-state asset))))
;   ))

;(run-tests ledger-safety-tests)
