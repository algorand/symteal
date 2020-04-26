#lang rosette/safe

(require "teal.rkt" "symbolic.rkt" "htlc.rkt"
         "config.rkt" "util.rkt" rosette/solver/smt/boolector)

; TODO: fix boolector problem
;(current-bitwidth 64)
;(current-solver (boolector))

;  define template variables
;  - tmpl_rcv: the address to send funds to when the preimage is supplied
; (define tmpl_rcv 22)
(define-symbolic tmpl_rcv bv64)

;  - TMPL_HASHFN (Deprecated): the specific hash function (either sha256 or keccak256) to apply
;  - tmpl_hashimg: the image of the hash function
(define tmpl_hashimg
  (keccak256-hash (bv 42 bv64)))

;  - tmpl_timeout: the round at which the account expires
; (define tmpl_timeout 500)
(define-symbolic tmpl_timeout bv64)

;  - tmpl_own: the address to refund funds to on timeout
; (define tmpl_own 33)
(define-symbolic tmpl_own bv64)

;  - tmpl_fee: maximum fee used by the transaction
; (define tmpl_fee 5000)
(define-symbolic tmpl_fee bv64)


(define-symbolic sym-arg-0 bv64)

(define mock-txn
  (gen-sym-txn (list sym-arg-0)))

(define mock-global-params
  (global-params (uint 1000) (uint 1000) (uint 1000) (uint 0)))

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
(assert (! (bveq tmpl_rcv (txn-content-close_remainder_to mock-txn))))
(assert (! (bveq tmpl_own (txn-content-close_remainder_to mock-txn))))
(verify (assert (not (teal-eval mock-cxt)))) ; expect unsat


(clear-asserts!) 

; next, we show that this program will evaluate to true in 2 cases
; case 1: CRT is tmpl_rcv, and the hash-preimage is provided (and other conditions)
(define case1
  (&& (bveq tmpl_rcv (txn-content-close_remainder_to mock-txn))
      (bveq sym-arg-0 (bv 42 bv64))
      (bvule (txn-content-fee mock-txn) tmpl_fee)
      (bveq (txn-content-type_enum mock-txn) (bv 1 bv64))
      (bveq (txn-content-receiver mock-txn) (bv 0 bv64))
      (bveq (txn-content-amount mock-txn) (bv 0 bv64))))

(assert case1)
(verify (assert (teal-eval mock-cxt))) ; expect unsat
;(clear-asserts!)

; case 2: CRT is owner
(define case2
  (&& (bveq tmpl_own (txn-content-close_remainder_to mock-txn))
      (bvugt (txn-content-first_valid mock-txn) tmpl_timeout)
      (bvule (txn-content-fee mock-txn) tmpl_fee)
      (bveq (txn-content-type_enum mock-txn) (bv 1 bv64))
      (bveq (txn-content-receiver mock-txn) (bv 0 bv64))
      (bveq (txn-content-amount mock-txn) (bv 0 bv64))))

(assert case2)
(verify (assert (teal-eval mock-cxt))) ; expect unsat
(clear-asserts!)


; finally, we show that if neither case 1 and case 2 is true,
; then this program will be evaluated to false
(assert (! case1))
(assert (! case2))

; we need a bit assumption about the hash function here
(define-symbolic a b bv64)
(define hash-property
    (forall (list a b)
            (if (! (bveq a b))
                (! (bveq (keccak256-hash a) (keccak256-hash b)))
                (bveq (keccak256-hash a) (keccak256-hash b)))))
(assert hash-property)
(verify (assert (not (teal-eval mock-cxt)))) ; expect unsat
