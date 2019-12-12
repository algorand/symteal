#lang rosette/safe

(require "syntax.rkt")

;  define template variables
;  - tmpl_rcv1: the first recipient in the split account
(define tmpl_rcv1 1)

;  - tmpl_rcv2: the second recipient in the split account
(define tmpl_rcv2 2)

;  - tmpl_rat1: fraction of money to be paid to the first recipient
(define tmpl_rat1 50)

;  - tmpl_rat2: fraction of money to be paid to the second recipient
(define tmpl_rat2 50)

;  - tmpl_minpay: minimum amount to be paid out of the account
(define tmpl_minpay 1000)

;  - tmpl_timeout: the round at which the account expires
(define tmpl_timeout 5000)

;  - tmpl_own: the address to refund funds to on timeout
(define tmpl_own 22)

;  - tmpl_fee: half of the maximum fee used by each split forwarding group transaction 
(define tmpl_fee 5000)

(define split-contract
  (list
   (txn 16) ; TypeEnum
   (int 1)
   (eq)
   (txn 1) ; Fee
   (int tmpl_fee)
   (le)
   (land)
   (global 4) ; GroupSize
   (int 2)
   (eq)
   (bnz 18) ; jump to (gtxn 0 sender)
   (txn 9) ; CloseRemainderTo
   (addr tmpl_own)
   (eq)
   (txn 7) ; Receiver
   (global 3) ; ZeroAddress
   (eq)
   (land)
   (txn 8) ; Amount
   (int 0)
   (eq)
   (land)
   (txn 2) ; FirstValid
   (int tmpl_timeout)
   (gt)
   (land)
   (int 1)
   (bnz 28) ; jump to the last instruction
   (gtxn 0 0) ; Receiver
   (gtxn 1 0) ; Sender
   (eq)
   (txn 9) ; CloseRemainderTo
   (global 3) ; ZeroAddress
   (eq)
   (land)
   (gtxn 0 7) ; Receiver
   (addr tmpl_rcv1)
   (eq)
   (land)
   (gtxn 1 7) ; Receiver
   (addr tmpl_rcv2)
   (eq)
   (land)
   (gtxn 0 8) ; Amount
   (int tmpl_rat2)
   (mul)
   (gtxn 1 8) ; Amount
   (int tmpl_rat1)
   (mul)
   (eq)
   (land)
   (gtxn 0 8) ; Amount
   (int tmpl_minpay)
   (ge)
   (land)
   (land) 
  ))