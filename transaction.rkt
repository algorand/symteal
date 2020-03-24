#lang rosette/safe

(require "teal.rkt")

(provide (all-from-out "teal.rkt")
         (struct-out ledger-state)
         algo-move
         asset-move
         txn-eval
         txn-group-eval-with-error
         txn-group-eval)

; a ledger state consists of account states and leases
; accounts: list of list of integers
; leases: list of (sender, lease, lastValid)
(struct ledger-state (accounts leases)  #:transparent)

; The execution of a transaction group is defined as the change
; of the account state from S to S'.
; Transactions in an atomic transaction group will be executed sequentially.
; For example, the first transaction will change the account state from S to
; S_0 (if successful). Then, the second transaction will take S_0
; as its initial state, and change the account state to S_1
; (if successful). If any transaction failed, the end account state
; will be rolled back to S.

; move algo
(define (algo-move state sender receiver close fee amount)
  (let* ([account-state (λ (state account) (list-ref (ledger-state-accounts state) account))]
         [account-balance (λ (state account) (car (account-state state account)))]
         [update-balance (λ (state account delta)
                           (ledger-state (list-set (ledger-state-accounts state)
                                                   account
                                                   (list-set (account-state state account)
                                                             0
                                                             (+ (account-balance state account) delta)))
                                         (ledger-state-leases state)))])
    (if (< (account-balance state sender) (+ amount fee))
        #f ; move didn't happen if sender balance cannot afford amount plus fee
        (if (= 0 close)
            (let ([state-1 (update-balance state sender (- (+ amount fee)))])
              (update-balance state-1 receiver amount))
            (let* ([state-1 (update-balance state sender (- (account-balance state sender)))]
                   [state-2 (update-balance state-1 receiver amount)])
              (update-balance state-2 close (- (account-balance state sender) amount fee)))))))

; move asset
(define (asset-move state asset sender receiver close fee amount)
  (let* ([account-state (λ (state account) (list-ref (ledger-state-accounts state) account))]
         [account-balance (λ (state account) (car (list-ref (ledger-state-accounts state) account)))]
         [update-balance (λ (state account delta)
                           (ledger-state (list-set (ledger-state-accounts state)
                                                   account
                                                   (list-set (account-state state account)
                                                             0
                                                             (+ (account-balance state account) delta)))
                                         (ledger-state-leases state)))]
         [asset-balance (λ (state account asset) (list-ref (account-state state account) asset))]
         [update-asset (λ (state account asset delta)
                         (ledger-state (list-set (ledger-state-accounts state)
                                                 account
                                                 (list-set (account-state state account)
                                                           asset
                                                           (+ (asset-balance state account asset) delta)))
                                       (ledger-state-leases state)))])
    (if (or (< (asset-balance state sender asset) amount) (< (account-balance state sender) fee))
        #f ; move didn't happen if sender asset balance cannot cover amount or sender algo balance cannot cover fee
        (if (= 0 close)
            (let* ([state-1 (update-balance state sender (- fee))]
                   [state-2 (update-asset state-1 sender asset (- amount))])
              (update-asset state-2 receiver asset amount))
            (let* ([state-1 (update-balance state sender (- fee))]
                   [state-2 (update-asset state-1 sender asset (- (asset-balance state sender asset)))]
                   [state-3 (update-asset state-2 receiver asset amount)])
            (update-asset state-3 close asset (- (asset-balance state sender asset) amount)))))))
              
; invalidate leases
(define (invalidate-leases state current-round)
  (ledger-state (ledger-state-accounts state)
                (filter (λ (a) (>= (car (cdr a)) current-round)) (ledger-state-leases state))))

; return the lease with '(sender lease ?), #f if not exist 
(define (find-lease state sender lease)
  (memf (λ (a) (and (= (car a) sender)
                    (= (car (cdr a)) lease)))
        (ledger-state-leases state)))
  
; add a lease to state
(define (add-lease state sender lease last-valid)
  (if (= lease 0)
      state
      (ledger-state (ledger-state-accounts state)
                    (cons (list sender lease last-valid) (ledger-state-leases state)))))

; eval single transaction
; currently support algo and asset payment
; it did a "lazy evaluation" on leases:
; when a transaction failed, it did nothing
; when a transaction suceed, it invalides expired leases, then, adds new lease if necessary 
; TODO: support more asset txn type, e.g. freeze and clawback
(define (txn-eval state current-round txn)
  (number-match (txn-content-type_enum txn)
    [1 (let ([sender (txn-content-sender txn)]
             [receiver (txn-content-receiver txn)]
             [crt (txn-content-close_remainder_to txn)]
             [amount (txn-content-amount txn)]
             [fee (txn-content-fee txn)]
             [lease (txn-content-lease txn)]
             [first-valid (txn-content-first_valid txn)]
             [last-valid (txn-content-last_valid txn)])
         (cond
           [(or (< current-round first-valid) (> current-round last-valid)) #f]
           [else (let ([state-1 (invalidate-leases state current-round)])
                   (if (find-lease state-1 sender lease)
                       #f
                       (let ([state-2 (algo-move state-1 sender receiver crt fee amount)])
                         (if (not state-2)
                             #f
                             (add-lease state-2 sender lease last-valid)))))]))] ; algo payment
    [4 (let ([sender (txn-content-asset_sender txn)]
             [receiver (txn-content-asset_receiver txn)]
             [crt (txn-content-asset_close_to txn)]
             [amount (txn-content-asset_amount txn)]
             [fee (txn-content-fee txn)]
             [asset (txn-content-xfer_asset txn)]
             [first-valid (txn-content-first_valid txn)]
             [last-valid (txn-content-last_valid txn)]
             [lease (txn-content-lease txn)])
         (cond
           [(or (< current-round first-valid) (> current-round last-valid)) #f]
           [else (let ([state-1 (invalidate-leases state current-round)])
                   (if (find-lease state-1 sender lease)
                       #f
                       (let ([state-2 (asset-move state asset sender receiver crt fee amount)])
                         (if (not state-2)
                             #f
                             (add-lease state-2 lease sender last-valid)))))]))] ; asset transfer
    [else #f]))

; eval a transaction group with error
(define (txn-group-eval-with-error state current-round txn-group)
  (if (empty? txn-group)
      state
      (let* ([txn (car txn-group)]
             [result (txn-eval state current-round txn)])
        (if result
            (txn-group-eval-with-error result current-round (cdr txn-group))
            #f))))

; eval a transaction group
(define (txn-group-eval state current-round txn-group)
  (let ([result (txn-group-eval-with-error state current-round txn-group)])
    (if result result state))) ; roll-back if evaluate to #f
