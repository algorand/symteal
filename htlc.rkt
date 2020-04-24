#lang rosette/safe

(require "teal.rkt" "config.rkt")

(provide htlc-contract)

(define (htlc-contract tmpl_rcv tmpl_hashimg tmpl_timeout tmpl_own tmpl_fee)
  (list
   (txn 1) ; Fee
   (int tmpl_fee)
   (le)
   (txn 16) ; TypeEnum
   (int (bv 1 bv64))
   (eq)
   (land)
   (txn 7) ; Receiver
   (global 3) ; ZeroAddress
   (eq)
   (land)
   (txn 8) ; Amount
   (int (bv 0 bv64))
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
