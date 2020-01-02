#lang rosette/safe

(require "teal.rkt")
(require "symbolic.rkt")

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

(define hltc-contract
  (list
   (txn 1) ; Fee
   (int tmpl_fee)
   (le)
   (txn 16) ; TypeEnum
   (int 1)
   (eq)
   (land)
   (txn 7) ; Receiver
   (global 3) ; ZeroAddress
   (eq)
   (land)
   (txn 8) ; Amount
   (int 0)
   (eq)
   (land)
   (txn 9) ; CloseRemainderTo
   (addr tmpl_rcv)
   (eq)
   (arg 0)
   (keccak256)
   (byte tmpl_hashimg)
   (eq)
   (land)
   (txn 9) ; CloseRemainderTo
   (addr tmpl_own)
   (eq)
   (txn 2) ; FirstValid
   (int tmpl_timeout)
   (gt)
   (land)
   (lor)
   (land)
   ))

(define-symbolic sym-arg-0 integer?)

(define mock-txn-content
  (txn-content (list sym-arg-0) sym-sender sym-fee sym-fv sym-fvt sym-lv sym-note
               sym-lease sym-receiver sym-amount sym-crt sym-vpk
               sym-spk sym-vf sym-vl sym-vkd sym-type sym-te sym-xa
               sym-aa sym-as sym-ar sym-act sym-gi sym-tid))

(define mock-global-params
  (global-params 1000 1000 1000 0))

(define mock-eval-params
  (eval-params mock-txn-content (list mock-txn-content) mock-global-params 0))

(define mock-cxt
  (context mock-eval-params (list) hltc-contract 0 0))

; let's prove a simple property first
; if close-remainder-to (CRT) is set to anything other than tmpl_rcv or tmpl_own
; this teal program will evaluate to false, i.e. cannot find a assignment to
; make this teal program to true
(assert (! (= tmpl_rcv sym-crt)))
(assert (! (= tmpl_own sym-crt)))
(verify (assert (not (teal-eval mock-cxt)))) ; expect unsat
(clear-asserts!) 

; next, we show that this program will evaluate to true in 2 cases
; case 1: CRT is tmpl_rcv, and the hash-preimage is provided (and other conditions)
(define case1
  (&& (= tmpl_rcv sym-crt)
      (= sym-arg-0 42)
      (<= sym-fee tmpl_fee)
      (= sym-te 1)
      (= sym-receiver 0)
      (= sym-amount 0)))

(assert case1)
(verify (assert (teal-eval mock-cxt))) ; expect unsat
(clear-asserts!)

; case 2: CRT is owner
(define case2
  (&& (= tmpl_own sym-crt)
      (> sym-fv tmpl_timeout)
      (<= sym-fee tmpl_fee)
      (= sym-te 1)
      (= sym-receiver 0)
      (= sym-amount 0)))

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
