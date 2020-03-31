#lang rosette/safe

(require "ledger.rkt")

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
