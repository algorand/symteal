#lang rosette/safe

(require rackunit rackunit/text-ui "ledger.rkt" "config.rkt" "periodic-payment.rkt" "test-util.rkt")
(provide (all-defined-out))

; define tempalte variables
; - test_rcv : address which is authorized to make withdrawals
(define test_rcv 2)

; - test_period: the time between a pair of withdrawal periods
(define test_period 1000)

; - test_dur: the duration of a withdrawal period
(define test_dur 500)

; - test_amt: amount of microAlgos in a single withdrawal
(define test_amt 500)

; - test_lease: string to use for the transaction lease
(define test_lease 42)

; - test_timeout: the round at which the account expires
(define test_timeout 10000)

; - test_fee: maximum fee used by the withdrawal transactions
(define test_fee 4000)

; now, without loss of generality, we can bind this escrow to a specific account
(define escrow-account 1)

(define pp-program
  (periodic-payment test_rcv test_period test_dur
                    test_amt test_lease test_timeout
                    test_fee))

(define mock-state
  (ledger-state (list (account-state 0 '(0 0 0) '())                    ;accounts
                      (account-state 4500 '(5000000 5000000 1000000) pp-program)
                      (account-state 8000000 '(8000000 8000000 8000000) '())
                      (account-state 1000 '(1000 3000000 4000000) '()))
                '((2 666 200)
                  (1 55 200))))

; start round
(define start-round 0)

; initial balance of the escrow account
(define init-amount
   (* (ceiling (/ test_timeout test_period)) test_amt))

; periodic payment account invariant
; s₀: initial balance in the account
; r₀: starting round
; r: current round
; p: period
; a: amount of each payment
(define (pp-lower-bound s₀ r₀ r p a)
  (- s₀ (* (ceiling (/ (- r r₀) p)) a)))

; naive invariant
(define (pp-naive-invariant state s₀ r₀ r p a e)
  (>= (algo-balance state e)
      (pp-lower-bound s₀ r₀ r p a)))

; unfortunately, this cannot be directly used as a loop invariant
; we need to strengthen it
; e: escrow address
(define (pp-invariant state s₀ r₀ r p a e)
  (|| (&& (= (modulo (- r r₀) p) 0) ; case 1: where withdraw could happen
                   (>= (algo-balance state e) (pp-lower-bound s₀ r₀ r p a)))
              (&& (! (= (modulo (- r r₀) p) 0))
                  (|| (>= (algo-balance state e) (+ (pp-lower-bound s₀ r₀ r p a) e))
                      (&& (>= (algo-balance state e) (pp-lower-bound s₀ r₀ r p a))
                          (lease-valid? state e test_lease (* (ceiling (/ (- r r₀) p)) p)))))))

(define current-round 1)

; min_txn_fee min_balance max_txn_life zero_address
(define mock-global-params
  (global-params 0 0 1000 0))

; type_enum: 1
(define mock-algo-txn
  (txn-content '() 1 1000 0 0 500 ;args sender fee first_valid first_valid_time last_valid
               0 42 2 500 0 0 0 ;note lease receiver amount close_re_to vote_pk selection_pk
               0 0 0 0 1 0 0 ;vote_f vote_l vote_k_d type type_enum xfer_asset asset_amt
               0 0 0 0 ;asset_sender asset_receiver asset_close_to tx_id
               ))

(define mock-eval-params (eval-params mock-algo-txn (list mock-algo-txn) mock-global-params 0))

(define result-state
  (txn-group-eval mock-state current-round (list mock-algo-txn) mock-global-params))

(ledger-precondition? mock-state)

(pp-naive-invariant result-state init-amount start-round (+ current-round 1) test_period test_amt escrow-account)

(algo-balance result-state escrow-account)
  
