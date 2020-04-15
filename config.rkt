#lang rosette/safe

(provide (all-defined-out))

(define universe-size 4)

(define asset-capacity 2)

; the actual algorand group capacity is 16, use 4 here for performance
; need to update this number if the acutal use scenario requires more
; than 4 txns in a group
(define group-capacity 4)

(define algo-supply 10000000000)

(define asset-supply-cap 10000000000)
