#lang rosette/safe

(require rosette/lib/match)
(require "syntax.rkt" "teal.rkt" "symbolic.rkt")

;  define template variables
;  - tmpl_rcv1: the first recipient in the split account
;(define tmpl_rcv1 0)
(define-symbolic tmpl_rcv1 integer?)

;  - tmpl_rcv2: the second recipient in the split account
;(define tmpl_rcv2 0)
(define-symbolic tmpl_rcv2 integer?)

;  - tmpl_rat1: fraction of money to be paid to the first recipient
;(define tmpl_rat1 0)
(define-symbolic tmpl_rat1 integer?)

;  - tmpl_rat2: fraction of money to be paid to the second recipient
;(define tmpl_rat2 0)
(define-symbolic tmpl_rat2 integer?)

;  - tmpl_minpay: minimum amount to be paid out of the account
;(define tmpl_minpay 1)
(define-symbolic tmpl_minpay integer?)

;  - tmpl_timeout: the round at which the account expires
;(define tmpl_timeout 5000)
(define-symbolic tmpl_timeout integer?)

;  - tmpl_own: the address to refund funds to on timeout
;(define tmpl_own 22)
(define-symbolic tmpl_own integer?)

;  - tmpl_fee: half of the maximum fee used by each split forwarding group transaction 
;(define tmpl_fee 0)
(define-symbolic tmpl_fee integer?)

(define split-contract
  (list
   (txn 16) ; TypeEnum
   (int 1)
   (eq)
   (txn 1) ; Fee
   (int tmpl_fee)
   (le)
   (land)
   (global 4) ; GroupSize
   (int 2)
   (eq)
   (bnz 18) ; jump to (gtxn 0 sender)
   (txn 9) ; CloseRemainderTo
   (addr tmpl_own)
   (eq)
   (txn 7) ; Receiver
   (global 3) ; ZeroAddress
   (eq)
   (land)
   (txn 8) ; Amount
   (int 0)
   (eq)
   (land)
   (txn 2) ; FirstValid
   (int tmpl_timeout)
   (gt)
   (land)
   (int 1)
   (bnz 28) ; jump to the last instruction
   (gtxn 0 0) ; Sender
   (gtxn 1 0) ; Sender
   (eq)
   (txn 9) ; CloseRemainderTo
   (global 3) ; ZeroAddress
   (eq)
   (land)
   (gtxn 0 7) ; Receiver
   (addr tmpl_rcv1)
   (eq)
   (land)
   (gtxn 1 7) ; Receiver
   (addr tmpl_rcv2)
   (eq)
   (land)
   (gtxn 0 8) ; Amount
   (int tmpl_rat2)
   (mul)
   (gtxn 1 8) ; Amount
   (int tmpl_rat1)
   (mul)
   (eq)
   (land)
   (gtxn 0 8) ; Amount
   (int tmpl_minpay)
   (ge)
   (land)
   (land) 
  ))

; this is not elegant, what may worth it to stay in rosette/safe
(define sym-txns-with-indices
  (list (cons (gen-sym-txn '()) 0)
        (cons (gen-sym-txn '()) 1)
        (cons (gen-sym-txn '()) 2)
        (cons (gen-sym-txn '()) 3)
        (cons (gen-sym-txn '()) 4)
        (cons (gen-sym-txn '()) 5)
        (cons (gen-sym-txn '()) 6)
        (cons (gen-sym-txn '()) 7)
        (cons (gen-sym-txn '()) 8)
        (cons (gen-sym-txn '()) 9)
        (cons (gen-sym-txn '()) 10)
        (cons (gen-sym-txn '()) 11)
        (cons (gen-sym-txn '()) 12)
        (cons (gen-sym-txn '()) 13)
        (cons (gen-sym-txn '()) 14)
        (cons (gen-sym-txn '()) 15)))

(define group-size 2)
;(define-symbolic group-size integer?)
;(assert (> group-size 0))

(define txn-group-with-indices
  (take sym-txns-with-indices group-size))

(define mock-global-params
  (global-params 1000 1000 1000 0))

(define (mock-eval-params txn txn-group i)
  (eval-params txn txn-group mock-global-params i))

; this eval model assumes that every txns in the group has the same logic sig
(define (eval-txn-group txns-with-indices global-params)
  (let ([txn-group (map car txns-with-indices)])
    (apply && (map (lambda (x)
                     (let ([i (cdr x)]
                           [txn (car x)])
                       (teal-eval (context (mock-eval-params txn txn-group i) '() split-contract 0 0))))
                   txns-with-indices))))

(define (txn-by-index txn-with-indices index)
  (car (list-ref txn-with-indices index)))

;(define txn-0
;  (txn-content '() 0 0 1000 1000 2000 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0))
;
;(define txn-1
;  (txn-content '() 0 0 1000 1000 2000 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0))
;
;(define split-eval-param-0
;  (mock-eval-params txn-0 (list txn-0 txn-1) 0))
;
;(define split-eval-param-1
;  (mock-eval-params txn-1 (list txn-0 txn-1) 1))
;
;(teal-eval (context split-eval-param-0 '() split-contract 0 0))
;(teal-eval (context split-eval-param-1 '() split-contract 0 0)) 
  
; we show that this program will evaluate to true in two cases
; case 1, split payment

(assert (>= tmpl_rat1 0))
(assert (>= tmpl_rat2 0))
(define case-1
  (let ([txn-0 (txn-by-index txn-group-with-indices 0)]
        [txn-1 (txn-by-index txn-group-with-indices 1)])
    (&& (= group-size 2)
        (= (txn-content-sender txn-0) (txn-content-sender txn-1))
        (= (txn-content-receiver txn-0) tmpl_rcv1)
        (= (txn-content-receiver txn-1) tmpl_rcv2)
        (= (* (txn-content-amount txn-0) tmpl_rat2) (* (txn-content-amount txn-1) tmpl_rat1))
        (>= (txn-content-amount txn-0) tmpl_minpay)
        (= (txn-content-close_remainder_to txn-0) 0)
        (= (txn-content-close_remainder_to txn-1) 0)
        (= (txn-content-type_enum txn-0) 1)
        (= (txn-content-type_enum txn-1) 1)
        (<= (txn-content-fee txn-0) tmpl_fee)
        (<= (txn-content-fee txn-1) tmpl_fee))))
        
(assert case-1)
;(asserts)
;(eval-txn-group txn-group-with-indices mock-global-params)
(solve (assert (not (eval-txn-group txn-group-with-indices mock-global-params))))

;(define txn-0 (txn-by-index txn-group-with-indices 0))

;(mock-eval-params txn-0 (map car txn-group-with-indices) 0)

;(teal-eval (context (mock-eval-params txn-0 (map car txn-group-with-indices) 0) '() split-contract 0 0))
;(solve (assert (not (teal-eval (context (mock-eval-params txn-0 (map car txn-group-with-indices) 0) '() split-contract 0 0)))))
;(clear-asserts!)

; case 2, this case is a bit strange since the original contract didn't specify group size in
; the close case. 
;(assert (> group-size 0))
;(assert (<= group-size 16))
;
;(define (ok-txn-with-indices txn-with-indices)
;  (let ([txn (car txn-with-indices)])
;    (&& (= (txn-content-close_remainder_to txn) tmpl_own)
;        (= (txn-content-amount txn) 0)
;        (= (txn-content-receiver txn) 0)
;        (> (txn-content-first_valid txn) tmpl_timeout))))
; 
;(define case-2
;     (apply && (map ok-txn-with-indices txn-group-with-indices)))
;
;(assert case-2)
;(verify (assert (not (eval-txn-group txn-group-with-indices mock-global-params))))
