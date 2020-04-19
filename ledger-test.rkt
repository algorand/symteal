#lang racket

(require rackunit rackunit/text-ui lens "ledger.rkt" "htlc.rkt" "periodic-payment.rkt")

(define-struct-lenses txn-content)
            
(define mock-state
  (ledger-state (list (account-state 0 '(0 0 0) '())                    ;accounts
                      (account-state 5000000 '(5000000 5000000 1000000) '())
                      (account-state 8000000 '(8000000 8000000 8000000) '())
                      (account-state 1000 '(1000 3000000 4000000) '()))
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

(define mock-global-params
  (global-params 0 0 1000 0))

(define ledger-tests
  (test-suite
   "Tests for ledger.rkt"

   (test-case
     "test algo move"
     (define state-1
       (algo-move mock-state 0 2 1 0 1000 1000000))
     (check-eq? (algo-balance state-1 2) 6999000)
     (check-eq? (algo-balance state-1 0) 1000)
     (check-not-false (algo-move mock-state 0 3 3 0 1000 0))
     (check-false (algo-move mock-state 0 3 3 0 1001 0))
     (check-false (algo-move mock-state 0 3 3 0 0 1001)))

   (test-case
       "test asset move"
     (define state-1
       (asset-move mock-state 0 1 2 1 0 1000 1000000))
     (check-eq? (asset-balance state-1 2 1) 7000000)
     (check-eq? (algo-balance state-1 2) 7999000)
     (check-eq? (algo-balance state-1 0) 1000)
     (check-not-false (asset-move mock-state 1 3 3 0 1000 3000000))
     (check-false (asset-move mock-state 2 3 3 0 1001 0))
     (check-false (asset-move mock-state 2 3 3 0 1000 4000001)))

   (test-case
       "test valid lease"
     (check-true (lease-valid? mock-state 1 42 999))
     (check-true (lease-valid? mock-state 2 666 200))
     (check-false (lease-valid? mock-state 2 666 201)))
   
   (test-case
       "test txn eval"
     ; algo transaction succeed
     (define txn-1
       (lens-set txn-content-amount-lens
                 (lens-set txn-content-receiver-lens 
                           (lens-set txn-content-sender-lens mock-algo-txn 2)
                           1)
                 1000000))
     (define state-1 (txn-eval mock-state 1000 txn-1 (list txn-1) 0 mock-global-params))
     (check-eq? (algo-balance state-1 1) 6000000)
     (check-eq? (algo-balance state-1 2) 6999000)
     (check-eq? (ledger-state-leases state-1) '())

     ; round not valid
     (check-false (txn-eval mock-state 999 txn-1 (list txn-1) 0 mock-global-params))
     (check-false (txn-eval mock-state 2001 txn-1 (list txn-1) 0 mock-global-params))
     
     ; not enough balance
     (define txn-2
       (lens-set txn-content-amount-lens
                 (lens-set txn-content-sender-lens txn-1 3)
                 1))
     (check-false (txn-eval mock-state 1000 txn-2 (list txn-1) 0 mock-global-params))

     ; close account
     (define txn-3
       (lens-set txn-content-close_remainder_to-lens txn-1 3))
     (define state-3 (txn-eval mock-state 1000 txn-3 (list txn-3) 0 mock-global-params))
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
     (define state-4 (txn-eval mock-state 1000 txn-4 (list txn-4) 0 mock-global-params))
     (check-eq? (asset-balance state-4 1 2) 0)
     (check-eq? (asset-balance state-4 3 2) 5000000)
     (check-eq? (algo-balance state-4 1) 4999000)

     ; round not valid
     (check-false (txn-eval mock-state 999 txn-4 (list txn-4) 0 mock-global-params))
     (check-false (txn-eval mock-state 2001 txn-4 (list txn-4) 0 mock-global-params))

     ; asset transaction fail since not enough fee
     (define txn-5
       (lens-set txn-content-fee-lens
                 (lens-set txn-content-asset_sender-lens txn-4 3)
                 1001))
     (check-false (txn-eval mock-state 1000 txn-5 (list txn-5) 0 mock-global-params))

     ; asset transaction fail since not enough balance
     (define txn-6 (lens-set txn-content-asset_amount-lens txn-4 1000001))
     (check-false (txn-eval mock-state 1000 txn-6 (list txn-6) 0 mock-global-params))

     ; asset close to
     (define txn-7
       (lens-set txn-content-asset_amount-lens
                 (lens-set txn-content-asset_close_to-lens txn-4 2)
                 500000))
     (define state-7 (txn-eval mock-state 1000 txn-7 (list txn-7) 0 mock-global-params))
     (check-eq? (asset-balance state-7 1 2) 0)
     (check-eq? (asset-balance state-7 2 2) 8500000)
     (check-eq? (asset-balance state-7 3 2) 4500000)
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
     (define group-state-1 (txn-group-eval mock-state 1000 (list txn-1 txn-2) mock-global-params))
     (check-equal? group-state-1 (txn-eval (txn-eval mock-state 1000 txn-1 (list txn-1) 0 mock-global-params)
                                           1000 txn-2 (list txn-2) 0 mock-global-params))

     ; group txn failed
     (define txn-3 (lens-set txn-content-asset_amount-lens txn-2 50000000))
     (check-false (txn-group-eval-with-error mock-state 1000 (list txn-1 txn-3) 0 mock-global-params))
     (check-equal? mock-state (txn-group-eval mock-state 1000 (list txn-1 txn-3) mock-global-params))
     (check-equal? mock-state (txn-group-eval mock-state 1000 (list txn-3 txn-1) mock-global-params))
     )

   (test-case
       "test logic sig txns"

     (define htlc (htlc-contract 3 (keccak256-hash 42) 5000 2 5000))
     (define logic-mock-state (set-program mock-state 1 htlc))
     (define txn-1
       (lens-set txn-content-args-lens
                 (lens-set txn-content-sender-lens
                           (lens-set txn-content-close_remainder_to-lens mock-algo-txn 3)
                           1)
                 `(42)))
     (define state-1 (txn-eval logic-mock-state 1000 txn-1 (list txn-1) 0 mock-global-params))
     (check-eq? (algo-balance state-1 1) 0)
     (check-eq? (algo-balance state-1 3) 5000000)
     (define txn-2
       (lens-set txn-content-close_remainder_to-lens txn-1 2))
     (check-false (txn-eval logic-mock-state 1000 txn-2 (list txn-2) 0 mock-global-params))

     ; periodic payment 
     (define pp (periodic-payment 2 500 500 50000 42 10000 1000))
     (define pp-mock-state (set-program mock-state 1 pp))
     (define txn-3
       (lens-set
        txn-content-last_valid-lens
        (lens-set txn-content-amount-lens
                  (lens-set txn-content-lease-lens
                            (lens-set txn-content-sender-lens
                                      (lens-set txn-content-receiver-lens mock-algo-txn 2)
                                      1)
                            42)
                  50000)
        1500))
     (define state-2 (txn-eval pp-mock-state 1000 txn-3 (list txn-3) 0 mock-global-params))
     (check-eq? (algo-balance state-2 1) 4949000)
     (check-eq? (algo-balance state-2 2) 8050000)
     (define txn-3-1
       (lens-set txn-content-first_valid-lens
                 (lens-set txn-content-last_valid-lens txn-3 2000)
                 1500))
     (define state-3 (txn-eval state-2 1500 txn-3-1 (list txn-3-1) 0 mock-global-params))
     (check-eq? (algo-balance state-3 1) 4898000)
     (check-eq? (algo-balance state-3 2) 8100000)
     )
 
   ))

;(run-tests ledger-tests)

