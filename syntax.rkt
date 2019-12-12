#lang rosette/safe 

(provide (all-defined-out))

;;; Arithmetic

(struct sha256 () #:transparent)

(struct keccak256 () #:transparent)

(struct sha512_256 () #:transparent)

(struct ed25519verify () #:transparent)

(struct plus () #:transparent)

(struct minus () #:transparent)

(struct divide () #:transparent)

(struct mul () #:transparent)

(struct gt () #:transparent) ; >

(struct lt () #:transparent) ; <

(struct ge () #:transparent) ; >=

(struct le () #:transparent) ; <=

(struct land () #:transparent) ; &&

(struct lor () #:transparent) ; ||

(struct eq () #:transparent) ; ==

(struct neq () #:transparent)

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
