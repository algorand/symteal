#lang rosette/safe

(require lens "teal.rkt" "syntax.rkt" "config.rkt")

(provide (all-from-out "teal.rkt") (struct-out ledger-state)
         (struct-out account-state) (all-defined-out))

(require (only-in racket
                  [println r:println]))

; account state:
; balance: algo balance
; assets: a list of algo balances
; program: teal program (only if this is a escrow account)
(struct account-state (balance assets program) #:transparent)

; a ledger state consists of account states and leases
; accounts: list of account-states
; leases: list of (sender, lease, lastValid)
(struct ledger-state (accounts leases)  #:transparent)

(define (state-of-account state account)
  (list-ref (ledger-state-accounts state) account))

; algo balance of an account
(define (algo-balance state account)
  (account-state-balance (state-of-account state account)))

; asset balance of an account's certain asset
(define (asset-balance state account asset)
  (list-ref (account-state-assets (state-of-account state account)) asset))

; update algo balance with delta
(define (update-balance state account delta)
  (ledger-state (list-set (ledger-state-accounts state)
                          account
                          (account-state
                           (+ (algo-balance state account) delta)
                           (account-state-assets (state-of-account state account))
                           (account-state-program (state-of-account state account))))
                (ledger-state-leases state)))

; update asset balance with delta
(define (update-asset state account asset delta)
  (ledger-state (list-set (ledger-state-accounts state)
                          account
                          (account-state (algo-balance state account)
                                         (list-set (account-state-assets
                                                    (state-of-account state account))
                                                   asset
                                                   (+ (asset-balance state account asset) delta))
                                         (account-state-program (state-of-account state account))))
                (ledger-state-leases state)))

; The execution of a transaction group is defined as the change
; of the account state from S to S'.
; Transactions in an atomic transaction group will be executed sequentially.
; For example, the first transaction will change the account state from S to
; S_0 (if successful). Then, the second transaction will take S_0
; as its initial state, and change the account state to S_1
; (if successful). If any transaction failed, the end account state
; will be rolled back to S.

; move algo
(define (algo-move state zero-address sender receiver close fee amount)
  (if (< (algo-balance state sender) (+ amount fee))
      #f ; move didn't happen if sender balance cannot afford amount plus fee
      (if (= 0 close)
          (let* ([state-1 (update-balance state sender (- (+ amount fee)))]
                 [state-2 (update-balance state-1 zero-address fee)])
            (update-balance state-2 receiver amount))
          (let* ([state-1 (update-balance state sender (- (algo-balance state sender)))]
                 [state-2 (update-balance state-1 receiver amount)]
                 [state-3 (update-balance state-2 zero-address fee)])
            (update-balance state-3 close (- (algo-balance state sender) amount fee))))))

; move asset
(define (asset-move state zero-address asset sender receiver close fee amount)
  (if (or (< (asset-balance state sender asset) amount) (< (algo-balance state sender) fee))
      #f ; move didn't happen if sender asset balance cannot cover amount or sender algo balance cannot cover fee
      (if (= 0 close)
          (let* ([state-1 (update-balance state sender (- fee))]
                 [state-2 (update-balance state-1 zero-address fee)]
                 [state-3 (update-asset state-2 sender asset (- amount))])
            (update-asset state-3 receiver asset amount))
          (let* ([state-1 (update-balance state sender (- fee))]
                 [state-2 (update-asset state-1 sender asset (- (asset-balance state sender asset)))]
                 [state-3 (update-asset state-2 receiver asset amount)]
                 [state-4 (update-balance state-3 zero-address fee)])
            (update-asset state-4 close asset (- (asset-balance state sender asset) amount))))))
              
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
; TODO: suppor min balance
(define (txn-eval state current-round txn txn-group index global)
  (let ([txn-eval-params (eval-params txn txn-group global index)]
        [zero-address (global-params-zero_address global)])
    (number-match
     (txn-content-type_enum txn)
     [1 (let ([sender (txn-content-sender txn)]
              [receiver (txn-content-receiver txn)]
              [crt (txn-content-close_remainder_to txn)]
              [amount (txn-content-amount txn)]
              [fee (txn-content-fee txn)]
              [lease (txn-content-lease txn)]
              [first-valid (txn-content-first_valid txn)]
              [last-valid (txn-content-last_valid txn)])
          (let ([program (account-state-program (list-ref (ledger-state-accounts state) sender))]
                [result (cond
                          [(or (< current-round first-valid) (> current-round last-valid)) #f]
                          [else (let ([state-1 (invalidate-leases state current-round)])
                                  (if (find-lease state-1 sender lease)
                                      #f
                                      (let ([state-2 (algo-move state-1 zero-address sender receiver crt fee amount)])
                                        (if (not state-2)
                                            #f
                                            (add-lease state-2 sender lease last-valid)))))])])
            (if (empty? program)
                (begin (if symteal-debug
                           (r:println "teal empty")
                           void)
                       result)
                (if (teal-eval (context txn-eval-params (list) program 0 0))
                    (begin
                      (if symteal-debug
                          (r:println "teal eval true")
                          void)
                      result)
                    (begin (if symteal-debug
                               (r:println "teal eval false")
                               void)
                               #f)))))]
    [4 (let ([sender (txn-content-asset_sender txn)]
             [receiver (txn-content-asset_receiver txn)]
             [crt (txn-content-asset_close_to txn)]
             [amount (txn-content-asset_amount txn)]
             [fee (txn-content-fee txn)]
             [asset (txn-content-xfer_asset txn)]
             [first-valid (txn-content-first_valid txn)]
             [last-valid (txn-content-last_valid txn)]
             [lease (txn-content-lease txn)])
         (let ([program (account-state-program (list-ref (ledger-state-accounts state) sender))]
               [result (cond
                         [(or (< current-round first-valid) (> current-round last-valid)) #f]
                         [else (let ([state-1 (invalidate-leases state current-round)])
                                 (if (find-lease state-1 sender lease)
                                     #f
                                     (let ([state-2 (asset-move state zero-address asset sender receiver crt fee amount)])
                                       (if (not state-2)
                                           #f
                                           (add-lease state-2 lease sender last-valid)))))])])
           (if (empty? program)
               result
               (if (teal-eval (context txn-eval-params (list) program 0 0))
                   result
                   #f))))] 
    [else #f])))

; eval a transaction group with error
(define (txn-group-eval-with-error state current-round txn-group index global)
  (if (= index (length txn-group))
      state
      (let* ([txn (list-ref txn-group index)]
             [result (txn-eval state current-round txn txn-group index global)])
        (if result
            (txn-group-eval-with-error result current-round txn-group (+ index 1) global)
            #f))))

; eval a transaction group
(define (txn-group-eval state current-round txn-group global)
  (let ([result (txn-group-eval-with-error state current-round txn-group 0 global)])
    (if result result state))) ; roll-back if evaluate to #f

; computing total algos
(define (total-algos state)
  (foldl + 0
         (map account-state-balance (ledger-state-accounts state))))

; computing total supply of a particular asset
(define (total-asset state asset)
  (foldl + 0
         (map (λ (ac) (list-ref (account-state-assets ac) asset)) (ledger-state-accounts state))))

(define-struct-lenses ledger-state)
(define-struct-lenses account-state)

; set a teal program as an account's escrow logic
(define (set-program state account program)
  (let ([p-lens (lens-compose account-state-program-lens
                              (list-ref-lens account)
                              ledger-state-accounts-lens)])
    (lens-set p-lens state program)))

; check leases
; check if there is a lease by sender is valid in valid-round
; return #t or #f
(define (lease-valid? state sender lease valid-round)
  (not (false? (memf (λ (a) (and (= (car a) sender)
                                 (= (cadr a) lease)
                                 (>= (caddr a) valid-round)))
                     (ledger-state-leases state)))))
