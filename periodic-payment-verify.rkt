#lang rosette/safe

(require "ledger.rkt" "symbolic.rkt" "config.rkt"
         "periodic-payment.rkt" rosette/solver/smt/boolector)
(provide (all-defined-out))

;(current-solver (boolector))

(current-bitwidth 64)

; define tempalte variables
; - tmpl_rcv : address which is authorized to make withdrawals
(define-symbolic tmpl_rcv bv64)
;(define tmpl_rcv 2)

; - tmpl_period: the time between a pair of withdrawal periods
(define-symbolic tmpl_period bv64)
;(define tmpl_period 1000)

; - tmpl_dur: the duration of a withdrawal period
(define-symbolic tmpl_dur bv64)
;(define tmpl_dur 500)

; - tmpl_amt: amount of microAlgos in a single withdrawal
(define-symbolic tmpl_amt bv64)
;(define tmpl_amt 500)

; - tmpl_lease: string to use for the transaction lease
(define-symbolic tmpl_lease bv64)
;(define tmpl_lease 42)

; - tmpl_timeout: the round at which the account expires
(define-symbolic tmpl_timeout bv64)
;(define tmpl_timeout 10000)

; - tmpl_fee: maximum fee used by the withdrawal transactions
(define-symbolic tmpl_fee bv64)
;(define tmpl_fee 4000)

; now, without loss of generality, we can bind this escrow to a specific account
(define escrow-account 1)

; now we prove that this program implement periodic payment
(define pp-program
  (periodic-payment tmpl_rcv tmpl_period tmpl_dur
                    tmpl_amt tmpl_lease tmpl_timeout
                    tmpl_fee))

; define a symbolic ledger state that bind the program to the escrow account
(define sym-ledger-state
  (set-program (gen-sym-ledger-state)
               escrow-account
               (periodic-payment tmpl_rcv tmpl_period tmpl_dur
                                 tmpl_amt tmpl_lease tmpl_timeout
                                 tmpl_fee)))

; start round
;(define start-round (uint 0))
(define-symbolic start-round bv64)
(assert (bvult start-round tmpl_timeout))
(assert (bvuge start-round (uint 0)))

(define (bvudiv-ceiling a b)
  (if (bvugt (bvurem a b) (uint 0))
      (bvadd (bvudiv a b) (uint 1))
      (bvudiv a b)))

; initial balance of the escrow account
(define init-amount
   (bvmul (bvudiv-ceiling tmpl_timeout tmpl_period) tmpl_amt))

; periodic payment account invariant
; s₀: initial balance in the account
; r₀: starting round
; r: current round
; p: period
; a: amount of each payment
(define (pp-lower-bound s₀ r₀ r p a)
  (bvsub s₀ (bvmul (bvudiv-ceiling  (bvsub r r₀) p) a)))

; naive invariant
(define (pp-naive-invariant state s₀ r₀ r p a e)
  (assert (bvugt (algo-balance state e)
              (pp-lower-bound s₀ r₀ r p a))))

; naive invariant without assert
(define (pp-naive-invariant? state s₀ r₀ r p a e)
  (bvuge (algo-balance state e)
      (pp-lower-bound s₀ r₀ r p a)))

; unfortunately, this cannot be directly used as a loop invariant
; we need to strengthen it
; e: escrow address
(define (pp-invariant state s₀ r₀ r p a e)
  (assert (|| (&& (bveq (bvurem (bvsub r r₀) p) (uint 0)) ; case 1: where withdraw could happen
                   (bvuge (algo-balance state e) (pp-lower-bound s₀ r₀ r p a)))
              (&& (! (bveq (bvurem (bvsub r r₀) p) 0))
                  (|| (bvuge (algo-balance state e) (bvadd (pp-lower-bound s₀ r₀ r p a) e))
                      (&& (bvuge (algo-balance state e) (pp-lower-bound s₀ r₀ r p a))
                          (lease-valid? state e tmpl_lease (bvmul (bvudiv-ceiling (bvsub r r₀) p) p))))))))


(define current-round (uint 1))
;(define-symbolic current-round integer?)
;(assert (>= current-round start-round))
;(assert (<= current-round tmpl_timeout))

(define sym-txn
  (gen-sym-txn (list)))

; min_txn_fee min_balance max_txn_life zero_address
(define mock-global-params
  (global-params (uint 0) (uint 0) (uint 1000) (uint 0)))

; set the pre-condition
;(pp-naive-invariant sym-ledger-state init-amount start-round
;                    current-round tmpl_period tmpl_amt (uint escrow-account))

; assert ledger precondition
(ledger-precondition sym-ledger-state)

;(define result-state
;  (txn-group-eval sym-ledger-state current-round (list sym-txn) mock-global-params))

;(print result-state)

;(pp-naive-invariant? result-state init-amount start-round (+ 1 current-round) tmpl_period tmpl_amt escrow-account)

;(list init-amount start-round current-round tmpl_period tmpl_amt escrow-account)

;(verify (pp-naive-invariant result-state init-amount start-round
;                            (bvadd (uint 1) current-round) tmpl_period tmpl_amt
;                            (uint escrow-account)))

  
