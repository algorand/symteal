#lang rosette/safe

(require rosette/lib/match)
(provide (all-defined-out))

;;; Arithmetic

(struct sha256 () #:transparent) ; not supported yet

(struct keccak256 () #:transparent)

(struct sha512_256 () #:transparent) ; not supported yet

(struct ed25519verify () #:transparent)

(struct plus () #:transparent)

(struct minus () #:transparent)

(struct div () #:transparent)

(struct mul () #:transparent)

(struct gt () #:transparent) ; >

(struct lt () #:transparent) ; <

(struct ge () #:transparent) ; >=

(struct le () #:transparent) ; <=

(struct land () #:transparent) ; &&

(struct lor () #:transparent) ; ||

(struct eq () #:transparent) ; ==

(struct neq () #:transparent) ; !=

(struct lnot () #:transparent)

(struct len () #:transparent)

(struct % () #:transparent)

;;; Loading values
(struct int (value) #:transparent) ; pushes uint64 constant to unit constants heap

(struct byte (value) #:transparent) ; pushed bytes into bytes heap

(struct addr (value) #:transparent) ; pushes address bytes to stack

(struct arg (index) #:transparent) ; push LogicSig.Args[index] value to stack

(struct txn (field) #:transparent) ; push a field of current transaction to stack

(struct gtxn (index field) #:transparent) ; push a field of group txn at index to stack

(struct global (index) #:transparent) ; push a field from global context to stack by index

;;; Flow Control

(struct err () #:transparent) ; error, panic immediately.

(struct bnz (offset) #:transparent) ; branch to offset of the value is not zero. offset itself is 3 bytes.

(struct pop () #:transparent) ;discard value from the stack

(struct dup () #:transparent) ;duplicate last value on stack

;;; Error

(struct teal-error (msg) #:transparent) ; this is not part of syntax


(require syntax/parse/define)

(define-simple-macro (number-match v [c e ...] ... [{~datum else} e2 ...])
  (case v
    [(c) e ...]
    ...
    [else e2 ...]))

; decode error code to error message
(define (decode-error error-code)
  (number-match error-code
    [1 "type-error-expected-uint"]
    [2 "type-error-expected-bytes"]
    [3 "error"]
    [4 "+ overflow"]
    [5 "- results negative"]
    [6 "divided by 0"]
    [7 "* overlflow"]
    [8 "compare values in different types"]
    [9 "invalid txn field"]
    [10 "bnz offset out of range"]
    [11 "group index out of range"]
    [12 "invalid gtxn field"]
    [else (assert #f "impossible")]))
