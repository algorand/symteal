#lang rosette/safe

;;; TEAL interpreter in Rosette

;;; data structures

; txn
(struct txn
  (sender fee first_valid first_valid_time last_valid
   note lease receiver amount close_remainder_to
   vote_pk selection_pk vote_first vote_last vote_key_dilution
   type type_enum xfer_asset asset_amount asset_sender asset_receiver
   asset_close_to group_index tx_id)
  #:transparent)

; gloable parameters
(struct global
  (max_txn_fee min_balance max_txn_life) #:transparent)

; eval parameters
(struct eval_params
  (txn global args) #:transparent)

; define the context, TODO: add type guard to the arguments.
(struct context (eval_params stack program pc next_pc err intc bytec) #:transparent)

; stack element.
(struct stack-elmt (uint bytes) #:transparent)

; op-spec, roughly same as OpSpec in data/transaction/logic/eval.go.
(struct op-spec (name op args returntype) #:transparent)

; reflect the type of an stack element:
; 0 is uint.
; 1 is bytes.
(define (elmt-type e)
  (if (null? (stack-elmt-bytes e)) 1 0)) 

(define uint64-max 18446744073709551615)

(define ops
  (list
   (op-spec "err" op-err null "None")
   (op-spec "sha256" op-sha256 '("Bytes") "Bytes")
   (op-spec "keccak256" op-keccak256 '("Bytes") "Bytes")
   (op-spec "ed25519verify" op-ed25519verify '("Bytes" "Bytes" "Bytes") "Uint64")
   (op-spec "+" op-plus '("Uint64" "Uint64") "Uint64")
   (op-spec "-" op-minus '("Uint64" "Uint64") "Uint64")
   (op-spec "/" op-div '("Uint64" "Uint64") "Uint64")
   (op-spec "*" op-mul '("Uint64" "Uint64") "Uint64")
   (op-spec ">" op-gt '("Uint64" "Uint64") "Uint64")
   (op-spec "<" op-lt '("Uint64" "Uint64") "Uint64")
   (op-spec ">=" op-ge '("Uint64" "Uint64") "Uint64")
   (op-spec "<=" op-le '("Uint64" "Uint64") "Uint64")
   (op-spec "&&" op-and '("Uint64" "Uint64") "Uint64")
   (op-spec "||" op-or '("Uint64" "Uint64") "Uint64")
   (op-spec "==" op-eq '("Any" "Any") "Uint64")
   (op-spec "!=" op-neq '("Any" "Any") "Uint64")
   (op-spec "!" op-not '("Uint64") "Uint64")
   (op-spec "len" op-len '("Bytes") "Uint64")
   ;(op-spec "btoi" op-btoi '("Bytes") "Uint64")
   ;(op-spec "%" op-mod '("Uint64" "Uint64") "Uint64")
   ;(op-spec "|" op-bitor '("Uint64" "Uint64") "Uint64")
   ;(op-sepc "&" op-bitand '("Uint64" "Uint64") "Uint64")
   ;(op-spec "^" op-bitxor '("Uint64" "Uint64") "Uint64")
   ;(op-spec "~" op-bitnot '("Uint64") "Uint64")

   (op-spec "int" op-int '() "Uint64")

   ;(op-spec "intcblock" op-intcblock '() "None")
   ;(op-spec "intc" op-intc '() "Uint64")
   ;(op-spec "bytecblock" op-bytecblock '() "None")
   ;(op-spec "bytec" op-bytec '() "Bytes")
   (op-spec "arg" op-arg '() "Bytes")
   (op-spec "txn" op-txn '() "Any")
   ;(op-spec "global" op-globle '() "Any")
   ;(op-spec "bnz" op-bnz '("Uint64") "None")
   ;(op-spec "pop" op-pop '("Any") "None")
   ;(op-spec "dup" op-dup '() "Any")
   ))

;; helper functions:

; get the uint from the top of the stack.
(define (top-uint cxt)
  (stack-elmt-uint (car (context-stack cxt))))

; get the uint from the second of the stack.
(define (second-uint cxt)
  (stack-elmt-uint (car (cdr (context-stack cxt)))))

; get the bytes from top of the stack.
(define (top-bytes cxt)
  (stack-elmt-bytes (car (context-stack cxt))))

; get the bytes from the second of the stack.
(define (second-bytes cxt)
  (stack-elmt-bytes (car (cdr (context-stack cxt)))))

; add an error message to the context.
(define (add-err cxt msg)
  (context (context-eval_params cxt)
           (context-stack cxt)
           (context-program cxt)
           (context-pc cxt)
           (context-next_pc cxt)
           msg
           (context-intc cxt)
           (context-bytec cxt)))

; update stack with a new stack value
(define (update-stack cxt ns)
  (context (context-eval_params cxt)
           ns
           (context-program cxt)
           (context-pc cxt)
           (context-next_pc cxt)
           (context-err cxt)
           (context-intc cxt)
           (context-bytec cxt)))

; push an int to a stack
(define (push-int-stack cxt i)
  (update-stack cxt (cons (stack-elmt i null) (context-stack cxt))))

; push bytes to a stack
(define (push-bytes-stack cxt b)
  (update-stack cxt (cons (stack-elmt 0 b) (context-stack cxt))))

;; opcode semantics:

; err.
(define (op-err cxt)
  (add-err cxt "error"))

; TODO: definition. 
(define (op-sha256 cxt) cxt)

; TODO: definition.
(define (op-keccak256 cxt) cxt)

; TODO: definition.
(define (op-ed25519verify cxt) cxt)

; +
(define (op-plus cxt)
  (let ([r (+ (second-uint cxt) (top-uint cxt))])
    (if (> r uint64-max)
        (add-err cxt "+ overflow")
        (update-stack cxt (cons (stack-elmt r nil)
                                (cdr (cdr (context-stack cxt))))))))

; -
(define (op-minus cxt)
  (let ([r (- (second-uint cxt) (top-uint cxt))])
    (if (< r 0)
        (add-err cxt "- would result negative")
        (update-stack cxt (cons (stack-elmt r nil)
                                (cdr (cdr (context-stack cxt))))))))

; /
(define (op-div cxt)
  (if (= (top-uint cxt) 0)
      (add-err cxt "/ 0")
      (let ([r (/ (second-uint cxt) (top-uint cxt))])
        (update-stack cxt (cons (stack-elmt r nil)
                                (cdr (cdr (context-stack cxt))))))))

; *
(define (op-mul cxt)
  (let ([r (* (top-uint cxt) (second-uint cxt))])
    (if (> r uint64-max)
        (add-err cxt "* overflow")
        (update-stack cxt (cons (stack-elmt r nil)
                                (cdr (cdr (context-stack cxt))))))))

; compare two numbers
(define (op-comp cxt op)
  (let ([r (if (op (second-uint cxt) (top-uint cxt)) 1 0)])
    (update-stack cxt (cons (stack-elmt r nil)
                            (cdr (cdr (context-stack cxt)))))))

; >
(define (op-gt cxt)
  (op-comp cxt >))

; <
(define (op-lt cxt)
  (op-comp cxt <))

; >=
(define (op-ge cxt)
  (op-comp cxt >=))

; <=
(define (op-le cxt)
  (op-comp cxt <=))

; and
; one caveat is that the two operands should never be negative
(define (op-and cxt)
  (op-comp cxt (lambda (a b) (or (= a 0) (= b 0))))) 

; or
(define (op-or cxt)
  (op-comp cxt (lambda (a b) (and (= a 0) (= b 0)))))        

; compare bytes
(define (bytes-comp cxt op)
  (let ([r (if (op (second-bytes cxt) (top-bytes cxt)) 1 0)])
    (update-stack cxt (cons (stack-elmt r nil)
                            (cdr (cdr (context-stack cxt)))))))

; ==
(define (op-eq cxt)
  (let ([ta (elmt-type (car (context-stack cxt)))]
        [tb (elmt-type (car (cdr (context-stack cxt))))])
    (if (= ta tb)
        (if (= ta 1)
            (bytes-comp cxt bveq)
            (op-comp cxt =))
        (add-err "cannot compare values in different types"))))

; neq
(define (op-neq cxt)
  (let ([ta (elmt-type (car (context-stack cxt)))]
        [tb (elmt-type (car (cdr (context-stack cxt))))])
    (if (= ta tb)
        (if (= ta 1)
            (bytes-comp cxt (lambda (a b) (not (bveq a b))))
            (op-comp cxt (lambda (a b) (not (= a b)))))
        (add-err "cannot compare values in different types"))))

; !
(define (op-not cxt)
  (let ([a (top-uint cxt)])
    (if (= a 0)
        (update-stack cxt (cons (stack-elmt 1 null) (cdr (context-stack cxt)))))))

; txn
(define (op-txn cxt)
  (let ([txn (eval_params-txn (context-eval_params cxt))]
        [idx (list-ref (context-program cxt) (+ (context-pc cxt) 1))])
    (match idx
      [0 (push-bytes-stack (txn-sender txn))]
      [1 (push-int-stack (txn-fee txn))]
      [2 (push-int-stack (txn-first_valid txn))]
      [3 (push-int-stack (txn-last_valid txn))]
      [4 (push-bytes-stack (txn-note txn))]
      [5 (push-bytes-stack (txn-receiver txn))]
      [6 (push-int-stack (txn-amount txn))]
      [7 (push-bytes-stack (txn-close_remainder_to txn))]
      [8 (push-bytes-stack (txn-vote_pk txn))]
      [9 (push-bytes-stack (txn-selection_pk txn))]
      [10 (push-int-stack (txn-vote_first txn))]
      [11 (push-int-stack (txn-vote_last txn))]
      [12 (push-int-stack (txn-vote_key_dilution txn))]
      [_ (add-err cxt (string-append "invalid txn field " (number->string idx)))]
      )))


; len 
(define (op-len cxt)
  (let [bytes (top-bytes cxt)]
    (update-stack cxt (cons (stack-elmt (length bytes) null) (rest (context-stack cxt))))))
  
; byte
; note: byte is not an actual opcode, the assembler will compile it to
; bytcblock, bytec
;(define (op-byte cxt)


; arg
(define (op-arg cxt)
  (let ([args (eval_params-args (context-eval_params cxt))]
        [idx (list-ref (context-program cxt) (+ (context-pc cxt) 1))])
    (update-stack cxt (cons (stack-elmt 0 (eval_params-args (context-eval_params cxt)))
                            (cdr (context-stack cxt))))))

;(define teal-eval-step (program context) nil)

