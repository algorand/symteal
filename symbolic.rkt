#lang rosette/safe

(require "teal.rkt" "config.rkt")
(provide (all-defined-out))

; define symbolic values

; sender
(define (sym-sender)
  (define-symbolic* sender integer?)
  (assert (>= sender 0))
  (assert (< sender universe-size))
  sender)

; fee
(define (sym-fee)
  (define-symbolic* fee integer?)
  fee)

; first valid
(define (sym-fv)
  (define-symbolic* first-valid integer?)
  first-valid)

; first valid time
(define (sym-fvt)
  (define-symbolic* first-valid-time integer?)
  first-valid-time)

; last valid
(define (sym-lv)
  (define-symbolic* last-valid integer?)
  last-valid)

; note
(define (sym-note)
  (define-symbolic* note integer?)
  note)

; lease
(define (sym-lease)
  (define-symbolic* lease integer?)
  lease)

; receiver
(define (sym-receiver)
  (define-symbolic* receiver integer?)
  (assert (> receiver 0)) ; zero-address is special
  (assert (< receiver universe-size))
  receiver)

; amount
(define (sym-amount)
  (define-symbolic* amount integer?)
  (assert (>= amount 0)) ; zero-address is special
  (assert (< amount uint64-max))
  amount)

; close remainder to
(define (sym-crt)
  (define-symbolic* close-remainder-to integer?)
  close-remainder-to)

; vote pk
(define (sym-vpk)
  (define-symbolic* vote-pk integer?)
  vote-pk)

; selection pk
(define (sym-spk)
  (define-symbolic* selection-pk integer?)
  selection-pk)

; vote first
(define (sym-vf)
  (define-symbolic* vote-first integer?)
  vote-first)

; vote last
(define (sym-vl)
  (define-symbolic* vote-last integer?)
  vote-last)

; vote key dilution
(define (sym-vkd)
  (define-symbolic* vote-key-delution integer?)
  vote-key-delution)

; type
(define (sym-type)
  (define-symbolic* type integer?)
  type)

; type enum
(define (sym-te)
  (define-symbolic* type-enum integer?)
  type-enum)

; xfer asset
(define (sym-xa)
  (define-symbolic* xfer-asset integer?)
  xfer-asset)

; asset amount
(define (sym-aa)
  (define-symbolic* asset-amount integer?)
  (assert (>= asset-amount 0))
  (assert (< asset-amount uint64-max))
  asset-amount)

; asset sender
(define (sym-as)
  (define-symbolic* asset-sender integer?)
  (assert (> asset-sender 0)) ; zero address is special
  (assert (< asset-sender universe-size))
  asset-sender)

; asset receiver
(define (sym-ar)
  (define-symbolic* asset-receiver integer?)
  (assert (> asset-receiver 0)) ; zero address is special
  (assert (< asset-receiver universe-size))
  asset-receiver)

; asset close to
(define (sym-act)
  (define-symbolic* asset-close-to integer?)
  asset-close-to)

; tx id
(define (sym-tid)
  (define-symbolic* tx-id integer?)
  tx-id)

; generate a symbolic txn
(define (gen-sym-txn args)
  (txn-content args (sym-sender) (sym-fee) (sym-fv) (sym-fvt) (sym-lv) (sym-note)
               (sym-lease) (sym-receiver) (sym-amount) (sym-crt) (sym-vpk)
               (sym-spk) (sym-vf) (sym-vl) (sym-vkd) (sym-type) (sym-te) (sym-xa)
               (sym-aa) (sym-as) (sym-ar) (sym-act) (sym-tid)))

; algo balance
(define (sym-algo-balance)
  (define-symbolic* algo-balance integer?)
  (assert (>= algo-balance 0))
  algo-balance)

; asset balance
(define (sym-asset-balance)
  (define-symbolic* asset-balance integer?)
  (assert (>= asset-balance 0))
  asset-balance)

; this is safe :)
(require (only-in racket [build-list r:build-list]))

; generate symbolic transactions with indices
(define sym-txns-with-indices (r:build-list 16 (λ (i) (cons (gen-sym-txn '()) i))))

; generate accounts
; the account universe is a list of
; '(algo-balance, asset-balance-1, ... , asset-balance-n)
(define gen-sym-account-universe
  (r:build-list universe-size (λ _  (cons (sym-algo-balance)
                                          (r:build-list asset-capacity (λ _ (sym-asset-balance)))))))


