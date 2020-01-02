#lang rosette/safe

(require "teal.rkt")
(provide (all-defined-out))

; define symbolic values

; sender
(define (sym-sender)
  (define-symbolic* sender integer?)
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
  receiver)

; amount
(define (sym-amount)
  (define-symbolic* amount integer?)
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


; asset aamount
(define (sym-aa)
  (define-symbolic* asset-amount integer?)
  asset-amount)

; asset sender
(define (sym-as)
  (define-symbolic* asset-sender integer?)
  asset-sender)

; asset receiver
(define (sym-ar)
  (define-symbolic* asset-receiver integer?)
  asset-receiver)

; asset close to
(define (sym-act)
  (define-symbolic* asset-close-to integer?)
  asset-close-to)

; group index
(define (sym-gi)
  (define-symbolic* group-index integer?)
  group-index)

; tx id
(define (sym-tid)
  (define-symbolic* tx-id integer?)
  tx-id)

; generate a symbolic txn
(define (gen-sym-txn args)
  (txn-content args (sym-sender) (sym-fee) (sym-fv) (sym-fvt) (sym-lv) (sym-note)
               (sym-lease) (sym-receiver) (sym-amount) (sym-crt) (sym-vpk)
               (sym-spk) (sym-vf) (sym-vl) (sym-vkd) (sym-type) (sym-te) (sym-xa)
               (sym-aa) (sym-as) (sym-ar) (sym-act) (sym-gi) (sym-tid)))
