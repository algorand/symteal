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

(define mock-txn
  (txn-content '() 0 1000 1000 0 2000 ;args sender fee first_valid first_valid_time last_valid
               0 0 0 0 0 0 0 ;note lease receiver amount close_re_to vote_pk selection_pk
               0 0 0 0 1 0 0 ;vote_f vote_l vote_k_d type type_enum xfer_asset asset_amt
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
     (define txn-1
       (lens-set txn-content-amount-lens
                 (lens-set txn-content-receiver-lens 
                           (lens-set txn-content-sender-lens mock-txn 2)
                           1)
                 1000000))
     (define state-1 (txn-eval mock-state 1000 txn-1))
     (check-eq? (algo-balance state-1 1) 6000000)
     (check-eq? (algo-balance state-1 2) 6999000)
     )
   ))

(run-tests transaction-tests)
  
  
