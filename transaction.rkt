#lang rosette/safe

(require "teal.rkt")

; The execution of a transaction group is defined as the change
; of the account state from S to S'.
; Transactions in an atomic transaction group will be executed sequentially.
; For example, the first transaction will change the account state from S to
; S_0 (if successful). Then, the second transaction will take S_0
; as its initial state, and change the account state to S_1
; (if successful). If any transaction failed, the end account state
; will be rolled back to S.

; algo's index is 0
(define (move account-universe index sender receiver amount)
  (let ([sender-balance (list-ref (list-ref account-universe sender) index)]
        [receiver-balance (list-ref (list-ref account-universe receiver) index)])
    (if (> amount sender-balance)
        #f
        (list-set (list-set account-universe
                            receiver
                            (list-set (list-ref account-universe receiver) index (+ receiver-balance amount)))  
                  sender
                  (list-set (list-ref account-universe sender) index (- sender-balance amount))))))


; eval single transaction
; currently support algo and asset payment
; TODO: support more asset txn type, e.g. freeze and clawback
(define (txn-eval account-universe current-round txn)
  (number-match (txn-content-type_enum txn)
    [1 (let ([sender (txn-content-sender txn)]
             [receiver (txn-content-receiver txn)]
             [crt (txn-content-close_remainder_to txn)]
             [amount (txn-content-amount)]
             [sender-balance (car (list-ref account-universe sender))])
         (cond
           [(and (= amount 0) (not (= crt 0))) (move account-universe 0 sender crt sender-balance)]
           [(and (>= amount 0) (= crt 0)) (move account-universe 0 sender receiver amount)]
           [else #f]))] ; algo payment
    [4 (let ([sender (txn-content-asset_sender txn)]
             [receiver (txn-content-asset_receiver txn)]
             [crt (txn-content-asset_close_to txn)]
             [amount (txn-content-asset_amount txn)]
             [asset (txn-content-xfer_asset txn)]
             [sender-balance (list-ref (list-ref account-universe0 sender) index)])
         (cond
           [(and (= amount 0) (not (= crt 0))) (move account-universe asset sender crt sender-balance)]
           [(and (>= amount 0) (= crt 0)) (move account-universe asset sender receiver amount)]
           [else #f]))] ; asset transfer
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