#lang rosette/safe

(require "teal.rkt" "ledger.rkt" "config.rkt")
(provide (all-defined-out))

; define symbolic values

; sender
(define (sym-sender)
  (define-symbolic* sender bv64)
  (assert (< (bv->nat sender) universe-size))
  sender)

; fee
(define (sym-fee)
  (define-symbolic* fee bv64)
  fee)

; first valid
(define (sym-fv)
  (define-symbolic* first-valid bv64)
  first-valid)

; first valid time
(define (sym-fvt)
  (define-symbolic* first-valid-time bv64)
  first-valid-time)

; last valid
(define (sym-lv)
  (define-symbolic* last-valid bv64)
  last-valid)

; note
(define (sym-note)
  (define-symbolic* note bv64)
  note)

; lease
(define (sym-lease)
  (define-symbolic* lease bv64)
  lease)

; receiver
(define (sym-receiver)
  (define-symbolic* receiver bv64)
  (assert (< (bv->nat receiver) universe-size))
  receiver)

; amount
(define (sym-amount)
  (define-symbolic* amount bv64)
  amount)

; close remainder to
(define (sym-crt)
  (define-symbolic* close-remainder-to bv64)
  close-remainder-to)

; vote pk
(define (sym-vpk)
  (define-symbolic* vote-pk bv64)
  vote-pk)

; selection pk
(define (sym-spk)
  (define-symbolic* selection-pk bv64)
  selection-pk)

; vote first
(define (sym-vf)
  (define-symbolic* vote-first bv64)
  vote-first)

; vote last
(define (sym-vl)
  (define-symbolic* vote-last bv64)
  vote-last)

; vote key dilution
(define (sym-vkd)
  (define-symbolic* vote-key-delution bv64)
  vote-key-delution)

; type
(define (sym-type)
  (define-symbolic* type bv64)
  type)

; type enum
(define (sym-te)
  (define-symbolic* type-enum bv64)
  type-enum)

; xfer asset
(define (sym-xa)
  (define-symbolic* xfer-asset bv64)
  xfer-asset)

; asset amount
(define (sym-aa)
  (define-symbolic* asset-amount bv64)
  asset-amount)

; asset sender
(define (sym-as)
  (define-symbolic* asset-sender bv64)
  (assert (< (bv->nat asset-sender) universe-size))
  asset-sender)

; asset receiver
(define (sym-ar)
  (define-symbolic* asset-receiver bv64)
  (assert (< (bv->nat asset-receiver) universe-size))
  asset-receiver)

; asset close to
(define (sym-act)
  (define-symbolic* asset-close-to bv64)
  asset-close-to)

; tx id
(define (sym-tid)
  (define-symbolic* tx-id bv64)
  tx-id)

; generate a symbolic txn
(define (gen-sym-txn args)
  (txn-content args (sym-sender) (sym-fee) (sym-fv) (sym-fvt) (sym-lv) (sym-note)
               (sym-lease) (sym-receiver) (sym-amount) (sym-crt) (sym-vpk)
               (sym-spk) (sym-vf) (sym-vl) (sym-vkd) (sym-type) (sym-te) (sym-xa)
               (sym-aa) (sym-as) (sym-ar) (sym-act) (sym-tid)))

; algo balance
(define (sym-algo-balance)
  (define-symbolic* algo-balance bv64)
  algo-balance)

; asset balance
(define (sym-asset-balance)
  (define-symbolic* asset-balance bv64)
  asset-balance)

; this is safe :)
(require (only-in racket
                  [build-list r:build-list]
                  [for r:for]))

; generate symbolic transactions with indices
(define (gen-sym-txns-with-indices) (r:build-list group-capacity (λ (i) (cons (gen-sym-txn '()) i))))

; generate accounts
; the account universe is a list of
; '(algo-balance, asset-balance-1, ... , asset-balance-n)
(define (gen-sym-account-states)
  (r:build-list universe-size
                (λ _  (account-state (sym-algo-balance)
                                     (r:build-list asset-capacity (λ _ (sym-asset-balance)))
                                     (list)))))

; generate symbolic global params
(define (gen-sym-global-params)
  (define-symbolic* min-txn-fee bv64)
  (define-symbolic* min-balance bv64)
  (define-symbolic* max-txn-life bv64)
  (define-symbolic* zero-address bv64)
  (assert (< (bv->nat zero-address) universe-size))
  (global-params min-txn-fee min-balance max-txn-life zero-address))

; generate symbolic round number
(define (gen-sym-round)
  (define-symbolic* current-round bv64)
  current-round)

; generate symbolic lease
(define (gen-sym-lease)
  (define-symbolic* lease-sender bv64)
  (define-symbolic* lease-value bv64)
  (define-symbolic* lease-last-valid bv64)
  (assert (< (bv->nat lease-sender) universe-size))
  (list lease-sender lease-value lease-last-valid))

; generate symbolic ledger state
; TODO: maybe use choices over variable sized leases
(define (gen-sym-ledger-state)
  (ledger-state (gen-sym-account-states) (list (gen-sym-lease) (gen-sym-lease))))

; general precondition of ledger state
; total algobalance is in (0, algosupply]
; total assetbalance is in (0, asset-supply-cap]
; TODO: need to be revisited for bitvector
(define (ledger-precondition state)
  (begin
    (r:for ([i (r:build-list asset-capacity (λ (e) e))])
           (let ([as (total-asset state i)])
             (assert (&& (> as 0)
                         (<= as asset-supply-cap)))))
    (let ([ag (total-algos state)])
      (assert (&& (> ag 0)
                  (<= ag algo-supply))))))
