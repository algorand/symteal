#lang racket

(require rackunit rackunit/text-ui lens "transaction.rkt")

(define-struct-lenses txn-content)
(define-struct-lenses ledger-state)

(define (asset-balance state asset account)
  (list-ref (list-ref (ledger-state-accounts state) account) asset))

(define (algo-balance state account)
  (asset-balance state 0 account))

(define mock-state
  (ledger-state '((0 0 0)                     ;accounts
                  (5000000 5000000 1000000)
                  (8000000 8000000 8000000)
                  (1000 3000000 4000000))
                '((1 42 1000)
                  (2 666 200))))

; type_enum: 1
(define mock-algo-txn
  (txn-content '() 0 1000 1000 0 2000 ;args sender fee first_valid first_valid_time last_valid
               0 0 0 0 0 0 0 ;note lease receiver amount close_re_to vote_pk selection_pk
               0 0 0 0 1 0 0 ;vote_f vote_l vote_k_d type type_enum xfer_asset asset_amt
               0 0 0 0 ;asset_sender asset_receiver asset_close_to tx_id
               ))

; type_enum: 4
(define mock-asset-txn
  (txn-content '() 0 1000 1000 0 2000 ;args sender fee first_valid first_valid_time last_valid
               0 0 0 0 0 0 0 ;note lease receiver amount close_re_to vote_pk selection_pk
               0 0 0 0 4 2 0 ;vote_f vote_l vote_k_d type type_enum xfer_asset asset_amt
               0 0 0 0 ;asset_sender asset_receiver asset_close_to tx_id
               ))


(define transaction-tests
  (test-suite
   "Tests for transaction.rkt"

   (test-case
     "test algo move"
     (define state-1
       (algo-move mock-state 2 0 0 1000 1000000))
     (check-eq? (algo-balance state-1 2) 6999000)
     (check-not-false (algo-move mock-state 3 3 0 1000 0))
     (check-false (algo-move mock-state 3 3 0 1001 0))
     (check-false (algo-move mock-state 3 3 0 0 1001)))

   (test-case
       "test asset move"
     (define state-1
       (asset-move mock-state 1 2 1 0 1000 1000000))
     (check-eq? (asset-balance state-1 1 2) 7000000)
     (check-eq? (algo-balance state-1 2) 7999000)
     (check-not-false (asset-move mock-state 1 3 3 0 1000 3000000))
     (check-false (asset-move mock-state 2 3 3 0 1001 0))
     (check-false (asset-move mock-state 2 3 3 0 1000 4000001)))

   (test-case
       "test txn eval"
     ; algo transaction succeed
     (define txn-1
       (lens-set txn-content-amount-lens
                 (lens-set txn-content-receiver-lens 
                           (lens-set txn-content-sender-lens mock-algo-txn 2)
                           1)
                 1000000))
     (define state-1 (txn-eval mock-state 1000 txn-1))
     (check-eq? (algo-balance state-1 1) 6000000)
     (check-eq? (algo-balance state-1 2) 6999000)
     (check-eq? (ledger-state-leases state-1) '())

     ; round not valid
     (check-false (txn-eval mock-state 999 txn-1))
     (check-false (txn-eval mock-state 2001 txn-1))
     
     ; not enough balance
     (define txn-2
       (lens-set txn-content-amount-lens
                 (lens-set txn-content-sender-lens txn-1 3)
                 1))
     (check-false (txn-eval mock-state 1000 txn-2))

     ; close account
     (define txn-3
       (lens-set txn-content-close_remainder_to-lens txn-1 3))
     (define state-3 (txn-eval mock-state 1000 txn-3))
     (check-eq? (algo-balance state-3 2) 0)
     (check-eq? (algo-balance state-3 3) 7000000)
     (check-eq? (algo-balance state-3 1) 6000000)

     ; asset transaction succeed
     (define txn-4
       (lens-set txn-content-asset_amount-lens
                 (lens-set txn-content-asset_receiver-lens
                           (lens-set txn-content-asset_sender-lens mock-asset-txn 1)
                           3)
                 1000000))
     (define state-4 (txn-eval mock-state 1000 txn-4))
     (check-eq? (asset-balance state-4 2 1) 0)
     (check-eq? (asset-balance state-4 2 3) 5000000)
     (check-eq? (algo-balance state-4 1) 4999000)

     ; round not valid
     (check-false (txn-eval mock-state 999 txn-4))
     (check-false (txn-eval mock-state 2001 txn-4))

     ; asset transaction fail since not enough fee
     (define txn-5
       (lens-set txn-content-fee-lens
                 (lens-set txn-content-asset_sender-lens txn-4 3)
                 1001))
     (check-false (txn-eval mock-state 1000 txn-5))

     ; asset transaction fail since not enough balance
     (define txn-6 (lens-set txn-content-asset_amount-lens txn-4 1000001))
     (check-false (txn-eval mock-state 1000 txn-6))

     ; asset close to
     (define txn-7
       (lens-set txn-content-asset_amount-lens
                 (lens-set txn-content-asset_close_to-lens txn-4 2)
                 500000))
     (define state-7 (txn-eval mock-state 1000 txn-7))
     (check-eq? (asset-balance state-7 2 1) 0)
     (check-eq? (asset-balance state-7 2 2) 8500000)
     (check-eq? (asset-balance state-7 2 3) 4500000)
     (check-eq? (algo-balance state-7 1) 4999000))

   (test-case
       "test txn group eval"

     (define txn-1
       (lens-set txn-content-amount-lens
                 (lens-set txn-content-receiver-lens 
                           (lens-set txn-content-sender-lens mock-algo-txn 2)
                           1)
                 1000000))
     
     (define txn-2
       (lens-set txn-content-asset_amount-lens
                 (lens-set txn-content-asset_receiver-lens
                           (lens-set txn-content-asset_sender-lens mock-asset-txn 1)
                           3)
                 1000000))

     ; group txn succeed
     (define group-state-1 (txn-group-eval mock-state 1000 (list txn-1 txn-2)))
     (check-equal? group-state-1 (txn-eval (txn-eval mock-state 1000 txn-1) 1000 txn-2))

     ; group txn failed
     (define txn-3 (lens-set txn-content-asset_amount-lens txn-2 50000000))
     (check-false (txn-group-eval-with-error mock-state 1000 (list txn-1 txn-3)))
     (check-equal? mock-state (txn-group-eval mock-state 1000 (list txn-1 txn-3)))
     (check-equal? mock-state (txn-group-eval mock-state 1000 (list txn-3 txn-1)))
     )
   
   ))

(run-tests transaction-tests)
  
  