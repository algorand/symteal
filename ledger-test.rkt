#lang rosette

(require rackunit rackunit/text-ui lens
         "ledger.rkt" "htlc.rkt" "periodic-payment.rkt"
         "util.rkt" "config.rkt")

(define-struct-lenses txn-content)

; for convenience
(define (uint v)
  (bv v bv64))

(define (check-bveq? v1 v2)
  (check-true (bveq v1 v2)))
            
(define mock-state
  (ledger-state (list (account-state (uint 0) `(,(uint 0) ,(uint 0) ,(uint 0)) '())  ;accounts
                      (account-state (uint 5000000) `(,(uint 5000000) ,(uint 5000000) ,(uint 1000000))
                                     '())
                      (account-state (uint 8000000) `(,(uint 8000000) ,(uint 8000000) ,(uint 8000000))
                                     '())
                      (account-state (uint 1000) `(,(uint 1000) ,(uint 3000000) ,(uint 4000000))
                                     '()))
                `(,(lease (uint 1) (uint 42) (uint 1000))
                  ,(lease (uint 2) (uint 666) (uint 200)))))

; type_enum: 1
(define mock-algo-txn
  (txn-content '() (uint 0) (uint 1000) (uint 1000) (uint 0) ;args sender fee first_valid f_v_time
               (uint 2000) (uint 0) (uint 0) (uint 0) (uint 0) ;last_valid note lease receiver amount
               (uint 0) (uint 0) (uint 0) (uint 0) (uint 0) ;c_r_t vote_pk selection_pk vote_f vote_l
               (uint 0) (uint 0) (uint 1) (uint 0) (uint 0) ;vote_k_d type type_enum xfer_asset asset_amt
               (uint 0) (uint 0) (uint 0) (uint 0) ;asset_sender asset_receiver asset_close_to tx_id
               ))

; type_enum: 4
(define mock-asset-txn 
  (txn-content '() (uint 0) (uint 1000) (uint 1000) (uint 0) ;args sender fee first_valid f_v_time
               (uint 2000) (uint 0) (uint 0) (uint 0) (uint 0) ;last_valid note lease receiver amount
               (uint 0) (uint 0) (uint 0) (uint 0) (uint 0) ;c_r_t vote_pk selection_pk vote_f vote_l
               (uint 0) (uint 0) (uint 4) (uint 2) (uint 0) ;vote_k_d type type_enum xfer_asset asset_amt
               (uint 0) (uint 0) (uint 0) (uint 0) ;asset_sender asset_receiver asset_close_to tx_id
               ))

(define mock-global-params
  (global-params (uint 0) (uint 0) (uint 1000) (uint 0)))

(define ledger-tests
  (test-suite
   "Tests for ledger.rkt"

   (test-case
     "test algo move"
     (define state-1
       (algo-move mock-state (uint 0) (uint 2) (uint 1) (uint 0) (uint 1000) (uint 1000000)))
     (check-bveq? (algo-balance state-1 (uint 2)) (uint 6999000))
     (check-bveq? (algo-balance state-1 (uint 0)) (uint 1000))
     (check-not-false (algo-move mock-state (uint 0) (uint 3) (uint 3) (uint 0) (uint 1000) (uint 0)))
     (check-false (algo-move mock-state (uint 0) (uint 3) (uint 3) (uint 0) (uint 1001) (uint 0)))
     (check-false (algo-move mock-state (uint 0) (uint 3) (uint 3) (uint 0) (uint 0) (uint 1001))))

   (test-case
       "test asset move"
     (define state-1
       (asset-move mock-state (uint 0) (uint 1) (uint 2) (uint 1) (uint 0) (uint 1000) (uint 1000000)))
     (check-bveq? (asset-balance state-1 (uint 2) (uint 1)) (uint 7000000))
     (check-bveq? (algo-balance state-1 (uint 2)) (uint 7999000))
     (check-bveq? (algo-balance state-1 (uint 0)) (uint 1000))
     (check-not-false
      (asset-move mock-state (uint 0) (uint 1) (uint 3) (uint 3) (uint 0) (uint 1000) (uint 3000000)))
     (check-false
      (asset-move mock-state (uint 0) (uint 2) (uint 3) (uint 3) (uint 0) (uint 1001) (uint 0)))
     (check-false
      (asset-move mock-state (uint 0) (uint 2) (uint 3) (uint 3) (uint 0) (uint 1000) (uint 4000001))))

   (test-case
       "test valid lease"
     (check-true (lease-valid? mock-state (uint 1) (uint 42) (uint 999)))
     (check-true (lease-valid? mock-state (uint 2) (uint 666) (uint 200)))
     (check-false (lease-valid? mock-state (uint 2) (uint 666) (uint 201))))
   
   (test-case
       "test txn eval"
     ; algo transaction succeed
     (define txn-1
       (lens-set txn-content-amount-lens
                 (lens-set txn-content-receiver-lens 
                           (lens-set txn-content-sender-lens mock-algo-txn (uint 2))
                           (uint 1))
                 (uint 1000000)))
     (define state-1 (txn-eval mock-state (uint 1000) txn-1 (list txn-1) 0 mock-global-params))
     (check-bveq? (algo-balance state-1 (uint 1)) (uint 6000000))
     (check-bveq? (algo-balance state-1 (uint 2)) (uint 6999000))
     (check-equal? (ledger-state-leases state-1) `(,(lease (uint 1) (uint 42) (uint 1000))))

     ; round not valid
     (check-false (txn-eval mock-state (uint 999) txn-1 (list txn-1) 0 mock-global-params))
     (check-false (txn-eval mock-state (uint 2001) txn-1 (list txn-1) 0 mock-global-params))
     
     ; not enough balance
     (define txn-2
       (lens-set txn-content-amount-lens
                 (lens-set txn-content-sender-lens txn-1 (uint 3))
                 (uint 1)))
     (check-false (txn-eval mock-state (uint 1000) txn-2 (list txn-1) 0 mock-global-params))

     ; close account
     (define txn-3
       (lens-set txn-content-close_remainder_to-lens txn-1 (uint 3)))
     (define state-3 (txn-eval mock-state (uint 1000) txn-3 (list txn-3) 0 mock-global-params))
     (check-bveq? (algo-balance state-3 (uint 2)) (uint 0))
     (check-bveq? (algo-balance state-3 (uint 3)) (uint 7000000))
     (check-bveq? (algo-balance state-3 (uint 1)) (uint 6000000))

     ; asset transaction succeed
     (define txn-4
       (lens-set txn-content-asset_amount-lens
                 (lens-set txn-content-asset_receiver-lens
                           (lens-set txn-content-asset_sender-lens mock-asset-txn (uint 1))
                           (uint 3))
                 (uint 1000000)))
     (define state-4 (txn-eval mock-state (uint 1000) txn-4 (list txn-4) 0 mock-global-params))
     (check-bveq? (asset-balance state-4 (uint 1) (uint 2)) (uint 0))
     (check-bveq? (asset-balance state-4 (uint 3) (uint 2)) (uint 5000000))
     (check-bveq? (algo-balance state-4 (uint 1)) (uint 4999000))

     ; round not valid
     (check-false (txn-eval mock-state (uint 999) txn-4 (list txn-4) 0 mock-global-params))
     (check-false (txn-eval mock-state (uint 2001) txn-4 (list txn-4) 0 mock-global-params))

     ; asset transaction fail since not enough fee
     (define txn-5
       (lens-set txn-content-fee-lens
                 (lens-set txn-content-asset_sender-lens txn-4 (uint 3))
                 (uint 1001)))
     (check-false (txn-eval mock-state (uint 1000) txn-5 (list txn-5) 0 mock-global-params))

     ; asset transaction fail since not enough balance
     (define txn-6 (lens-set txn-content-asset_amount-lens txn-4 (uint 1000001)))
     (check-false (txn-eval mock-state (uint 1000) txn-6 (list txn-6) 0 mock-global-params))

     ; asset close to
     (define txn-7
       (lens-set txn-content-asset_amount-lens
                 (lens-set txn-content-asset_close_to-lens txn-4 (uint 2))
                 (uint 500000)))
     (define state-7 (txn-eval mock-state (uint 1000) txn-7 (list txn-7) 0 mock-global-params))
     (check-bveq? (asset-balance state-7 (uint 1) (uint 2)) (uint 0))
     (check-bveq? (asset-balance state-7 (uint 2) (uint 2)) (uint 8500000))
     (check-bveq? (asset-balance state-7 (uint 3) (uint 2)) (uint 4500000))
     (check-bveq? (algo-balance state-7 (uint 1)) (uint 4999000)))

   (test-case
       "test txn group eval"

     (define txn-1
       (lens-set txn-content-amount-lens
                 (lens-set txn-content-receiver-lens 
                           (lens-set txn-content-sender-lens mock-algo-txn (uint 2))
                           (uint 1))
                 (uint 1000000)))
     
     (define txn-2
       (lens-set txn-content-asset_amount-lens
                 (lens-set txn-content-asset_receiver-lens
                           (lens-set txn-content-asset_sender-lens mock-asset-txn (uint 1))
                           (uint 3))
                 (uint 1000000)))

     ; group txn succeed
     (define group-state-1 (txn-group-eval mock-state (uint 1000) (list txn-1 txn-2) mock-global-params))
     (check-equal? group-state-1 (txn-eval (txn-eval mock-state (uint 1000) txn-1 (list txn-1) 0 mock-global-params)
                                           (uint 1000) txn-2 (list txn-2) 0 mock-global-params))

     ; group txn failed
     (define txn-3 (lens-set txn-content-asset_amount-lens txn-2 (uint 50000000)))
     (check-false (txn-group-eval-with-error mock-state (uint 1000) (list txn-1 txn-3) 0 mock-global-params))
     (check-equal? mock-state (txn-group-eval mock-state (uint 1000) (list txn-1 txn-3) mock-global-params))
     (check-equal? mock-state (txn-group-eval mock-state (uint 1000) (list txn-3 txn-1) mock-global-params))
     )

   (test-case
       "test logic sig txns"

     (define htlc (htlc-contract (uint 3) (keccak256-hash (uint 42)) (uint 5000) (uint 2) (uint 5000)))
     (define logic-mock-state (set-program mock-state 1 htlc))
     (define txn-1
       (lens-set txn-content-args-lens
                 (lens-set txn-content-sender-lens
                           (lens-set txn-content-close_remainder_to-lens mock-algo-txn (uint 3))
                           (uint 1))
                 `(,(uint 42))))
     (define state-1 (txn-eval logic-mock-state (uint 1000) txn-1 (list txn-1) 0 mock-global-params))
     (check-bveq? (algo-balance state-1 (uint 1)) (uint 0))
     (check-bveq? (algo-balance state-1 (uint 3)) (uint 5000000))
     (define txn-2
       (lens-set txn-content-close_remainder_to-lens txn-1 (uint 2)))
     (check-false (txn-eval logic-mock-state (uint 1000) txn-2 (list txn-2) 0 mock-global-params))

     ; periodic payment 
     (define pp (periodic-payment (uint 2) (uint 500) (uint 500)
                                  (uint 50000) (uint 43) (uint 10000) (uint 1000)))
     (define pp-mock-state (set-program mock-state 1 pp))
     (define txn-3
       (lens-set
        txn-content-last_valid-lens
        (lens-set txn-content-amount-lens
                  (lens-set txn-content-lease-lens
                            (lens-set txn-content-sender-lens
                                      (lens-set txn-content-receiver-lens mock-algo-txn (uint 2))
                                      (uint 1))
                            (uint 43))
                  (uint 50000))
        (uint 1500)))
     (define state-2 (txn-eval pp-mock-state (uint 1000) txn-3 (list txn-3) 0 mock-global-params))
     (check-bveq? (algo-balance state-2 (uint 1)) (uint 4949000))
     (check-bveq? (algo-balance state-2 (uint 2)) (uint 8050000))
     (define txn-3-1
       (lens-set txn-content-first_valid-lens
                 (lens-set txn-content-last_valid-lens txn-3 (uint 2000))
                 (uint 1500)))
     (define state-3 (txn-eval state-2 (uint 1501) txn-3-1 (list txn-3-1) 0 mock-global-params))
     (check-bveq? (algo-balance state-3 (uint 1)) (uint 4898000))
     (check-bveq? (algo-balance state-3 (uint 2)) (uint 8100000))
     )
 
   ))

(run-tests ledger-tests)

