#lang rosette/safe

(require "teal.rkt")
(require "symbolic.rkt")

;  define template variables
;  - tmpl_rcv: the address to send funds to when the preimage is supplied
(define tmpl_rcv 22)

;  - TMPL_HASHFN (Deprecated): the specific hash function (either sha256 or keccak256) to apply
;  - tmpl_hashimg: the image of the hash function
(define tmpl_hashimg
  (keccak256-hash 42))

;  - tmpl_timeout: the round at which the account expires
(define tmpl_timeout 500)

;  - tmpl_own: the address to refund funds to on timeout
(define tmpl_own 33)

;  - tmpl_fee: maximum fee used by the transaction
(define tmpl_fee 5000)

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

(define mock-txn-content
  (txn-content sym-sender sym-fee sym-fv sym-fvt sym-lv sym-note
               sym-lease sym-receiver sym-amount sym-crt sym-vpk
               sym-spk sym-vf sym-vl sym-vkd sym-type sym-te sym-xa
               sym-aa sym-as sym-ar sym-act sym-gi sym-tid))

(define mock-global-params
  (global-params 1000 1000 1000 0 1))

(define mock-eval-params
  (eval-params mock-txn-content mock-global-params (list 42)))

(define mock-cxt
  (context mock-eval-params (list) hltc-contract 0 0))

; let's verify the simple fact
; if close-remainder-to is set to anything other than 22 or 33
; this teal program will evaluate to false
(assert (! (= 22 sym-crt)))
(assert (! (= 33 sym-crt)))
(verify (assert (not (teal-eval mock-cxt))))