#lang racket

(require "config.rkt" "ledger.rkt")
(provide (all-defined-out))

(define (ledger-precondition? state)
  (and
    (for/and ([i (build-list asset-capacity (λ (e) e))])
           (let ([as (total-asset state i)])
             (and (> as 0)
                 (<= as asset-supply-cap))))
    (let ([ag (total-algos state)])
      (and (> ag 0)
          (<= ag algo-supply)))))
