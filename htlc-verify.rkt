#lang rosette/safe

(require "teal.rkt" "symbolic.rkt" "htlc.rkt")

;  define template variables
;  - tmpl_rcv: the address to send funds to when the preimage is supplied
; (define tmpl_rcv 22)
(define-symbolic tmpl_rcv integer?)

;  - TMPL_HASHFN (Deprecated): the specific hash function (either sha256 or keccak256) to apply
;  - tmpl_hashimg: the image of the hash function
(define tmpl_hashimg
  (keccak256-hash 42))

;  - tmpl_timeout: the round at which the account expires
; (define tmpl_timeout 500)
(define-symbolic tmpl_timeout integer?)

;  - tmpl_own: the address to refund funds to on timeout
; (define tmpl_own 33)
(define-symbolic tmpl_own integer?)

;  - tmpl_fee: maximum fee used by the transaction
; (define tmpl_fee 5000)
(define-symbolic tmpl_fee integer?)


(define-symbolic sym-arg-0 integer?)

(define mock-txn
  (gen-sym-txn (list sym-arg-0)))

(define mock-global-params
  (global-params 1000 1000 1000 0))

(define mock-eval-params
  (eval-params mock-txn (list mock-txn) mock-global-params 0))

(define mock-cxt
  (context mock-eval-params
           (list)
           (htlc-contract tmpl_rcv tmpl_hashimg tmpl_timeout tmpl_own tmpl_fee)
           0
           0))

; let's prove a simple property first
; if close-remainder-to (CRT) is set to anything other than tmpl_rcv or tmpl_own
; this teal program will evaluate to false, i.e. cannot find a assignment to
; make this teal program to true
(assert (! (= tmpl_rcv (txn-content-close_remainder_to mock-txn))))
(assert (! (= tmpl_own (txn-content-close_remainder_to mock-txn))))
(verify (assert (not (teal-eval mock-cxt)))) ; expect unsat
(clear-asserts!) 

; next, we show that this program will evaluate to true in 2 cases
; case 1: CRT is tmpl_rcv, and the hash-preimage is provided (and other conditions)
(define case1
  (&& (= tmpl_rcv (txn-content-close_remainder_to mock-txn))
      (= sym-arg-0 42)
      (<= (txn-content-fee mock-txn) tmpl_fee)
      (= (txn-content-type_enum mock-txn) 1)
      (= (txn-content-receiver mock-txn) 0)
      (= (txn-content-amount mock-txn) 0)))

(assert case1)
(verify (assert (teal-eval mock-cxt))) ; expect unsat
(clear-asserts!)

; case 2: CRT is owner
(define case2
  (&& (= tmpl_own (txn-content-close_remainder_to mock-txn))
      (> (txn-content-first_valid mock-txn) tmpl_timeout)
      (<= (txn-content-fee mock-txn) tmpl_fee)
      (= (txn-content-type_enum mock-txn) 1)
      (= (txn-content-receiver mock-txn) 0)
      (= (txn-content-amount mock-txn) 0)))

(assert case2)
(verify (assert (teal-eval mock-cxt))) ; expect unsat
(clear-asserts!)


; finally, we show that if neither case 1 and case 2 is true,
; then this program will be evaluated to false
(assert (! case1))
(assert (! case2))

; we need a bit assumption about the hash function here
(define-symbolic a b integer?)
(define hash-property
    (forall (list a b)
            (if (! (= a b))
                (! (= (keccak256-hash a) (keccak256-hash b)))
                (= (keccak256-hash a) (keccak256-hash b)))))
(assert hash-property)
(verify (assert (not (teal-eval mock-cxt)))) ; expect unsat
