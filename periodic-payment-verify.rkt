#lang rosette/safe

(require "ledger.rkt" "symbolic.rkt" "config.rkt" "periodic-payment.rkt")
(provide (all-defined-out))

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

; - tmpl_amt: amount of microAlgos in a single withdrawal
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

; now we prove that this program implement periodic payment

; NOTE: this is fine as long as r:build-list and r:for are only used in concrete execution 
(require (only-in racket
                  [build-list r:build-list]
                  [for r:for]))

; general precondition of ledger state
; total algobalance is in (0, algosupply]
; total assetbalance is in (0, asset-supply-cap]
; TODO: move this to a general place
(define (ledger-precondition state)
  (begin
    (r:for ([i (r:build-list asset-capacity (λ (e) e))])
           (let ([as (total-asset state i)])
             (assert (&& (> as 0)
                         (<= as asset-supply-cap)))))
    (let ([ag (total-algos state)])
      (assert (&& (> ag 0)
                  (<= ag algo-supply))))))

; now, without loss of generality, we can bind this escrow to a specific account
(define escrow-account 1)

; define a symbolic ledger state that bind the program to the escrow account
(define sym-ledger-state
  (set-program (gen-sym-ledger-state)
               escrow-account
               (periodic-payment tmpl_rcv tmpl_period tmpl_dur
                                 tmpl_amt tmpl_lease tmpl_timeout
                                 tmpl_fee)))

; start round
(define-symbolic start-round integer?)
(assert (< start-round tmpl_timeout))
(assert (>= start-round 0))

; initial balance of the escrow account
(define init-amount
   (* (ceiling (/ tmpl_timeout tmpl_period)) tmpl_amt))

; periodic payment account invariant
; s₀: initial balance in the account
; r₀: starting round
; r: current round
; p: period
; a: amount of each payment
(define (pp-lower-bound s₀ r₀ r p a)
  (- s₀ (* (ceiling (/ (- r r₀) p)) a)))

; unfortunately, this cannot be directly used as a loop invariant
; we need to strengthen it
; e: escrow address
(define (pp-invariant state s₀ r₀ r p a e)
  (assert (|| (&& (= (modulo (- r r₀) p) 0) ; case 1: where withdraw could happen
                   (>= (algo-balance state e) (pp-lower-bound s₀ r₀ r p a)))
              (&& (! (= (modulo (- r r₀) p) 0))
                  (|| (>= (algo-balance state e) (+ (pp-lower-bound s₀ r₀ r p a) e))
                      (&& (>= (algo-balance state e) (pp-lower-bound s₀ r₀ r p a))
                          (lease-valid? state e tmpl_lease (* (ceiling (/ (- r r₀) p)) p))))))))

(define-symbolic current-round integer?)
(assert (>= current-round start-round))
(assert (<= current-round tmpl_timeout))

; assert pp invariant
(pp-invariant sym-ledger-state init-amount start-round current-round tmpl_period tmpl_amt escrow-account)

(define sym-txn
  (gen-sym-txn (list)))

(define result-state
  (txn-group-eval sym-ledger-state current-round (list sym-txn) (gen-sym-global-params)))

; assert ledger precondition
(ledger-precondition sym-ledger-state)

(verify (pp-invariant sym-ledger-state init-amount start-round (+ 1 current-round) tmpl_period tmpl_amt escrow-account))


  
