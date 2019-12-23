#lang rosette/safe

(provide (all-defined-out))

; define symbolic values

; sender
(define-symbolic sym-sender integer?)

; fee
(define-symbolic sym-fee integer?)

; first valid
(define-symbolic sym-fv integer?)

; first valid time
(define-symbolic sym-fvt integer?)

; last valid
(define-symbolic sym-lv integer?)

; note
(define-symbolic sym-note integer?)

; lease
(define-symbolic sym-lease integer?)

; receiver
(define-symbolic sym-receiver integer?)

; amount
(define-symbolic sym-amount integer?)

; close remainder to
(define-symbolic sym-crt integer?)

; vote pk
(define-symbolic sym-vpk integer?)

; selection pk
(define-symbolic sym-spk integer?)

; vote first
(define-symbolic sym-vf integer?)

; vote last
(define-symbolic sym-vl integer?)

; vote key dilution
(define-symbolic sym-vkd integer?)

; type
(define-symbolic sym-type integer?)

; type enum
(define-symbolic sym-te integer?)

; xfer asset
(define-symbolic sym-xa integer?)

; asset aamount
(define-symbolic sym-aa integer?)

; asset sender
(define-symbolic sym-as integer?)

; asset receiver
(define-symbolic sym-ar integer?)

; asset close to
(define-symbolic sym-act integer?)

; group index
(define-symbolic sym-gi integer?)

; tx id
(define-symbolic sym-tid integer?)
