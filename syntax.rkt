#lang rosette 

;;; Arithmetic

(struct sha256 () #:transparent)

(struct keccak256 () #:transparent)

(struct sha512_256 () #:transparent)

(struct ed25519verify () #:transparent)

(struct plus () #:transparent)

(struct minus () #:transparent)

(struct divide () #:transparent)

(struct time () #:transparent)

(struct gt () #:transparent)

(struct lt () #:transparent)

(struct gte () #:transparent)

(struct lte () #:transparent)

(struct land () #:transparent)

(struct lor () #:transparent)

(struct eq () #:transparent)

(struct neq () #:transparent)

(struct ! () #:transparent)

(struct len () #:transparent)

(struct btoi () #:transparent)

(struct % () #:transparent)

(struct bor () #:transparent)

(struct band () #:transparent)

(struct xor () #:transparent)

(struct ~ () #:transparent)

;;; Loading values
(struct intcblock () #:transparent) ; pushes uint64 constant to unit constants heap 

(struct intc () #:transparent) ; intc is followed by a uint, index of the to-be-loaded value in intcblock 

(struct intc_1 () #:transparent)

(struct intc_2 () #:transaprent)

(struct intc_3 () #:transparent)

(struct intc_4 () #:transparent)

(struct bytec () #:transparent)

(struct bytec_0 () #:transparent)

(struct bytec_1 () #:transparent)

(struct bytec_2 () #:transparent)

(struct bytec_3 () #:transparent)

(struct arg (index) #:transparent) ; push LogicSig.Args[index] value to stack

(struct arg_0 #:transparent)

(struct arg_1 #:transparent)

(struct arg_2 #:transparent)

(struct arg_3 #:transparent)

(struct txn (index) #:transparent) ; push a field of current transaction to stack by index

(struct global (index) #:transparent) ; push a field from global context to stack by index

;;; Flow Control

(struct err () #:transparent) ; error, panic immediately.

(struct bnz (offset) #:transparent) ; branch to offset of the value is not zero. offset itself is 3 bytes.

(struct pop #:transparent) ;discard value from the stack

(struct dup #:transparent) ;duplicate last value on stack
