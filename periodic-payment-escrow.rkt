#lang rosette/safe

(require "transaction.rkt")

; define tempalte variables
; - tmpl_rcv : address which is authorized to make withdrawals
;(define tmpl_rcv 1)
(define-symbolic tmpl_rcv integer?)

; - tmpl_period: the time between a pair of withdrawal periods
; (define tmpl_period 1000)
(define-symbolic tmpl_period integer?)

; - tmpl_dur: the duration of a withdrawal period
; (define tmpl_dur 500)
(define-symbolic tmpl_dur integer?)

; - tmpl_amt: the maximum number of funds allowed for a single withdrawal
;(define tmpl_amt 500)
(define-symbolic tmpl_amt integer?)

; - tmpl_lease: string to use for the transaction lease
; (define tmpl_lease 42)
(define-symbolic tmpl_lease integer?)

; - tmpl_timeout: the round at which the account expires
; (define tmpl_timeout 100000)
(define-symbolic tmpl_timeout integer?)

; - tmpl_fee: maximum fee used by the withdrawal transactions
; (define tmpl_fee 4000)
(define-symbolic tmpl_fee integer?)

(define periodic-payment
  (list
   (txn TypeEnum)
   (int 1)
   (eq)
   (txn Fee)
   (int tmpl_fee)
   (le)
   (land)
   (txn FirstValid)
   (int tmpl_period)
   (mod)
   (int 0)
   (eq)
   (land)
   (txn LastValid)
   (int tmpl_dur)
   (txn FirstValid)
   (plus)
   (eq)
   (land)
   (txn Lease)
   (byte tmpl_lease)
   (eq)
   (land) ;  is Payment and ok Fee and on period and correct duration and good lease
   (txn CloseRemainderTo)
   (global ZeroAddress)
   (eq)
   (txn Receiver)
   (addr tmpl_rcv)
   (eq)
   (land)
   (txn Amount)
   (int tmpl_amt)
   (eq)
   (land) ; no close and good Receiver and good amount
   (txn CloseRemainderTo)
   (addr tmpl_rcv)
   (eq)
   (txn Receiver)
   (global ZeroAddress)
   (eq)
   (land)
   (txn FirstValid)
   (int tmpl_timeout)
   (gt)
   (land)
   (txn Amount)
   (int 0)
   (eq)
   (land) ; good close to and after timeout and 0 Amount
   (lor)  ; normal payment or close   
   (land) ; (normal payment or close) and preamble checks 
))   
