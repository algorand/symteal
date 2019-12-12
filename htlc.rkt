#lang rosette/safe

(require "syntax.rkt")

;  define template variables
;  - tmpl_rcv: the address to send funds to when the preimage is supplied
(define tmpl_rcv 22)

;  - TMPL_HASHFN (Deprecated): the specific hash function (either sha256 or keccak256) to apply
;  - tmpl_hashimg: the image of the hash function
(define tmpl_hashimg 42)

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
