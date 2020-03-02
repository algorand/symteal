#lang rosette/safe

(require "teal.rkt")

; a ledger state consists of account states and leases
; accounts: list of list of integers
; leases: list of (round, lease)
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

(define (asset-move state asset sender receiver close fee amount)
  (let* ([account-state (λ (state account) (list-ref (ledger-state-accounts state) account))]
         [account-balance (λ (state account) (car (list-ref (ledger-state-accounts state) account)))]
         [update-balance (λ (state account delta)
                           (ledger-state (list-set (ledger-state-accounts state)
                                                   account
                                                   (+ (account-balance state account) delta))
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
              
        

; eval single transaction
; TODO: it will first invalidate outdates leases
; currently support algo and asset payment
; TODO: support more asset txn type, e.g. freeze and clawback
(define (txn-eval state current-round txn)
  (number-match (txn-content-type_enum txn)
    [1 (let* ([sender (txn-content-sender txn)]
              [receiver (txn-content-receiver txn)]
              [crt (txn-content-close_remainder_to txn)]
              [amount (txn-content-amount txn)]
              [fee (txn-content-fee txn)]
              [account-universe (ledger-state-accounts state)]
              [sender-balance (car (list-ref account-universe sender))])
         (cond
           [(or (< current-round (txn-content-first_valid txn)) (> current-round (txn-content-last_valid txn))) #f]
           [else (algo-move state sender receiver crt fee amount)]))] ; algo payment
    [4 (let* ([sender (txn-content-asset_sender txn)]
              [receiver (txn-content-asset_receiver txn)]
              [crt (txn-content-asset_close_to txn)]
              [amount (txn-content-asset_amount txn)]
              [fee (txn-content-fee txn)]
              [asset (txn-content-xfer_asset txn)])
         (cond
           [(or (< current-round (txn-content-first_valid txn)) (> current-round (txn-content-last_valid txn))) #f]
           [else (asset-move state asset sender receiver crt fee amount)]))] ; asset transfer
    [else #f]))

; eval a transaction group with error
(define (txn-group-eval-with-error account-universe current-round txn-group)
  (if (empty? txn-group)
      account-universe
      (let* ([txn (car txn-group)]
             [result (txn-eval account-universe current-round txn)])
        (if (not result)
            #f
            (txn-group-eval-with-error result current-round (cdr txn-group))))))

; eval a transaction group
(define (txn-group-eval account-universe current-round txn-group)
  (let ([result (txn-group-eval-with-error account-universe current-round txn-group)])
    (if (not result)
        account-universe ; roll-back if evaluate to #f
        result)))