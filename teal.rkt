#lang rosette/safe

(require rosette/lib/match)
(require "syntax.rkt")

(provide txn-content global eval_params context keccak256-hash teal-eval) 

;;; TEAL interpreter in Rosette

;;; data structures

; txn
(struct txn-content
  (sender fee first_valid first_valid_time last_valid
   note lease receiver amount close_remainder_to
   vote_pk selection_pk vote_first vote_last vote_key_dilution
   type type_enum xfer_asset asset_amount asset_sender asset_receiver
   asset_close_to group_index tx_id)
  #:transparent)

; gloable parameters
(struct global
  (max_txn_fee min_balance max_txn_life zero_address group_size) #:transparent)

; eval parameters
(struct eval_params
  (txn global args) #:transparent)

; define the context, TODO: add type guard to the arguments.
(struct context (eval_params stack program pc err) #:transparent)

; stack element.
; type of stack element
; 0 is uint
; 1 is bytes
(struct stack-elmt (value type) #:transparent)

; maximum uint64
(define uint64-max 18446744073709551615)

;; helper functions:

; get the top element of the stack and make sure it is uint
(define (top-uint cxt)
  (let ([top-elmt (car (context-stack cxt))])
    (if (equal? (stack-elmt-type top-elmt) 0)
        (stack-elmt-value top-elmt)
        (teal-error "type-error-expected-uint"))))

; get the second element of the stack and make sure it is uint
(define (second-uint cxt)
  (let ([second-elmt (car (cdr (context-stack cxt)))])
    (if (equal? (stack-elmt-type second-elmt) 0)
        (stack-elmt-value second-elmt)
        (teal-error "type-error-expected-uint"))))

; get the top element of the stack and make sure it is bytes
(define (top-bytes cxt)
  (let ([top-elmt (car (context-stack cxt))])
    (if (equal? (stack-elmt-type top-elmt) 1)
        (stack-elmt-value top-elmt)
        (teal-error "type-error-expected-uint"))))

; get the second element of the stack and make sure it is bytes
(define (second-bytes cxt)
  (let ([second-elmt (car (cdr (context-stack cxt)))])
    (if (equal? (stack-elmt-type second-elmt) 1)
        (stack-elmt-value second-elmt)
        (teal-error "type-error-expected-uint"))))

; add an error message to the context.
(define (add-err cxt msg)
  (context (context-eval_params cxt)
           (context-stack cxt)
           (context-program cxt)
           (context-pc cxt)
           msg))

; update stack with a new stack value
(define (update-stack cxt ns)
  (context (context-eval_params cxt)
           ns
           (context-program cxt)
           (context-pc cxt)
           (context-err cxt)))

; push an int to a stack
(define (push-int cxt i)
  (update-stack cxt (cons (stack-elmt i 0) (context-stack cxt))))

; push bytes to a stack
(define (push-bytes cxt b)
  (update-stack cxt (cons (stack-elmt b 1) (context-stack cxt))))

;; opcode semantics:

; int.
(define (op-int cxt v)
  (push-int cxt v))

; err.
(define (op-err cxt)
  (add-err cxt "error"))

(define-symbolic keccak256-hash (~> integer? integer?))

; keccak256
(define (op-keccak256 cxt)
  (let ([a (top-bytes cxt)])
    (match a
      [(teal-error msg) (add-err cxt msg)]
      [va (update-stack cxt (cons (stack-elmt (keccak256-hash va) 1)
                                  (cdr (context-stack cxt))))])))

(define (int-op cxt op)
  (let ([a (second-uint cxt)]
        [b (top-uint cxt)])
    (match a
      [(teal-error msg) (add-err cxt msg)]
      [va (match b
            [(teal-error msg) (add-err cxt msg)]
            [vb (op cxt va vb)])])))

; +
(define (op-plus cxt)
  (let ([op (lambda (cxt a b)
              (let ([r (+ a b)])
                (if (> r uint64-max)
                    (add-err cxt "+ overflow")
                    (update-stack cxt (cons (stack-elmt r 0)
                                            (cdr (cdr (context-stack cxt))))))))])
    (int-op cxt op)))

; -
(define (op-minus cxt)
  (let ([op (lambda (cxt a b)
              (if (< (- a b) 0)
                    (add-err cxt "- would result negative")
                    (update-stack cxt (cons (stack-elmt (- a b) 0)
                                            (cdr (cdr (context-stack cxt)))))))])
    (int-op cxt op)))

; /
(define (op-div cxt)
  (let ([op (lambda (cxt a b)
              (if (= b 0)
                  (add-err cxt "divided by 0")      
                  (update-stack cxt (cons (stack-elmt (/ a b) 0)
                                          (cdr (cdr (context-stack cxt)))))))])
    (int-op cxt op)))

; *
(define (op-mul cxt)
  (let ([op (lambda (cxt a b)
              (if (> (* a b) uint64-max)
                  (add-err cxt "* overlflow")
                  (update-stack cxt (cons (stack-elmt (* a b) 0)
                                          (cdr (cdr (context-stack cxt)))))))])
    (int-op cxt op)))

; compare two numbers
(define (int-comp cxt op)
  (let ([a (second-uint cxt)]
        [b (top-uint cxt)])
    (match a
      [(teal-error msg) (add-err cxt msg)]
      [va (match b
            [(teal-error msg) (add-err cxt msg)]
            [vb (let ([r (if (op va vb) 1 0)])
                  (update-stack cxt (cons (stack-elmt r 0)
                                          (cdr (cdr (context-stack cxt))))))])])))

; >
(define (op-gt cxt)
  (int-comp cxt >))

; <
(define (op-lt cxt)
  (int-comp cxt <))

; >=
(define (op-ge cxt)
  (int-comp cxt >=))

; <=
(define (op-le cxt)
  (int-comp cxt <=))

; and
; one caveat is that the two operands should never be negative
(define (op-and cxt)
  (int-comp cxt (lambda (a b) (or (= a 0) (= b 0))))) 

; or
(define (op-or cxt)
  (int-comp cxt (lambda (a b) (and (= a 0) (= b 0)))))        

; compare bytes
(define (bytes-comp cxt op)
  (let ([a (second-bytes cxt)]
        [b (top-bytes cxt)])
    (match a
      [(teal-error msg) (add-err cxt msg)]
      [va (match b
            [(teal-error msg) (add-err cxt msg)]
            [vb (let ([r (if (op va vb) 1 0)])
    (update-stack cxt (cons (stack-elmt r 1)
                            (cdr (cdr (context-stack cxt))))))])])))

; ==
(define (op-eq cxt)
  (let ([ta (stack-elmt-type (car (context-stack cxt)))]
        [tb (stack-elmt-type (car (cdr (context-stack cxt))))])
    (if (= ta tb)
        (if (= ta 1)
            (bytes-comp cxt =)
            (int-comp cxt =))
        (add-err "cannot compare values in different types"))))

; neq
(define (op-neq cxt)
  (let ([ta (stack-elmt-type (car (context-stack cxt)))]
        [tb (stack-elmt-type (car (cdr (context-stack cxt))))])
    (if (= ta tb)
        (if (= ta 1)
            (bytes-comp cxt (lambda (a b) (not (bveq a b))))
            (int-comp cxt (lambda (a b) (not (= a b)))))
        (add-err "cannot compare values in different types"))))

; !
(define (op-not cxt)
  (let ([a (top-uint cxt)])
    (match a
      [(teal-error msg) (add-err cxt msg)]
      [va (if (= a 0)
        (update-stack cxt
                      (cons (stack-elmt 1 0) (cdr (context-stack cxt))))
        (update-stack cxt
                      (cons (stack-elmt 0 0) (cdr (context-stack cxt)))))])))

; txn
(define (op-txn cxt idx)
  (let ([txn (eval_params-txn (context-eval_params cxt))])
    (match idx
      [0 (push-bytes cxt (txn-content-sender txn))]
      [1 (push-int cxt (txn-content-fee txn))]
      [2 (push-int cxt (txn-content-first_valid txn))]
      [3 (push-int cxt (txn-content-first_valid_time txn))]
      [4 (push-int cxt (txn-content-last_valid txn))]
      [5 (push-bytes cxt (txn-content-note txn))]
      [6 (push-bytes cxt (txn-content-lease txn))]
      [7 (push-bytes cxt (txn-content-receiver txn))]
      [8 (push-int cxt (txn-content-amount txn))]
      [9 (push-bytes cxt (txn-content-close_remainder_to txn))]
      [10 (push-bytes cxt (txn-content-vote_pk txn))]
      [11 (push-bytes cxt (txn-content-selection_pk txn))]
      [12 (push-int cxt (txn-content-vote_first txn))]
      [13 (push-int cxt (txn-content-vote_last txn))]
      [14 (push-int cxt (txn-content-vote_key_dilution txn))]
      [15 (push-bytes cxt (txn-content-type txn))]
      [16 (push-int cxt (txn-content-type_enum txn))]
      [17 (push-int cxt (txn-content-xfer_asset txn))]
      [18 (push-int cxt (txn-content-asset_amount txn))]
      [19 (push-bytes cxt (txn-content-asset_sender txn))]
      [20 (push-bytes cxt (txn-content-asset_receiver txn))]
      [21 (push-bytes cxt (txn-content-asset_close_to txn))]
      [22 (push-int cxt (txn-content-group_index txn))]
      [23 (push-bytes cxt (txn-content-tx_id txn))]
      [_ (add-err cxt "invalid txn field")]
      )))

  
; byte
; note: byte is not an actual opcode, the assembler will compile it to
; bytcblock, bytec
(define (op-byte cxt value)
  (push-bytes cxt value))

; addr
(define (op-addr cxt value)
  (push-bytes cxt value))

; arg
(define (op-arg cxt index)
  (let ([args (eval_params-args (context-eval_params cxt))])
    (push-bytes cxt (list-ref (eval_params-args (context-eval_params cxt)) index))))          

; update pc
(define (update-pc cxt new-pc)
  (context (context-eval_params cxt)
           (context-stack cxt)
           (context-program cxt)
           new-pc
           (context-err cxt)))

; increase pc by 1
(define (pc-increment cxt)
  (update-pc cxt (+ (context-pc cxt) 1)))

; bnz
(define (op-bnz cxt offset)
  (let ([a (top-uint cxt)])
    (match a
      [(teal-error msg) (add-err cxt msg)]
      [va
       (if (= a 0)
           (pc-increment (update-stack cxt (cdr (context-stack cxt)))) ;pop if zero
           (let ([new-pc (+ (context-pc cxt) offset)])
             (if (>= new-pc (len (context-program cxt)))
                 (add-err cxt "bnz offset out of range")
                 (update-pc (update-stack cxt (cdr (context-stack cxt))) new-pc)))
           )])))

; op-spec, roughly same as OpSpec in data/transaction/logic/eval.go.
(struct op-spec (name op args returntype) #:transparent)

; this is a list of currently supported ops
; purely information purpose
(define ops
  (list
   (op-spec "err" op-err null "None")
   (op-spec "keccak256" op-keccak256 '("Bytes") "Bytes")
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
   (op-spec "int" op-int '() "Uint64")
   (op-spec "arg" op-arg '() "Bytes")
   (op-spec "txn" op-txn '() "Any")
   (op-spec "byte" op-byte '() "Bytes")
   (op-spec "addr" op-addr '() "Bytes")
   ))  

; teal eval step
; read an instruction, execute this instruction,
; then recursively call itself
(define (eval-step cxt)
  (cond
    ; return cxt if all instructions has been executed
    [(= (context-pc cxt) (len (context-program cxt))) cxt]
    [(not (= (context-err cxt) "")) cxt]
    [else
     (let ([op (list-ref (context-program) (context-pc cxt))])
       (match op
         [(keccak256) (eval-step (pc-increment (op-keccak256 cxt)))]
         [(err) (op-err cxt)] ; return now, no need recursion
         [(plus) (eval-step (pc-increment (op-plus cxt)))]
         [(div) (eval-step (pc-increment (op-div cxt)))]
         [(mul) (eval-step (pc-increment (op-mul cxt)))]
         [(gt) (eval-step (pc-increment (op-gt cxt)))]
         [(lt) (eval-step (pc-increment (op-lt cxt)))]
         [(ge) (eval-step (pc-increment (op-ge cxt)))]
         [(le) (eval-step (pc-increment (op-le cxt)))]
         [(land) (eval-step (pc-increment (op-and cxt)))]
         [(lor) (eval-step (pc-increment (op-or cxt)))]
         [(eq) (eval-step (pc-increment (op-eq cxt)))]
         [(neq) (eval-step (pc-increment (op-neq cxt)))]
         [(not) (eval-step (pc-increment (op-div cxt)))]
         [(int value) (eval-step (pc-increment (op-int cxt value)))]
         [(byte value) (eval-step (pc-increment (op-byte cxt value)))]
         [(addr value) (eval-step (pc-increment (op-addr cxt value)))]
         [(txn field) (eval-step (pc-increment (op-txn cxt field)))]
         [(arg index) (eval-step (pc-increment (op-arg cxt index)))]))]))


; teal eval
(define (teal-eval cxt)
  (let ([result (eval-step cxt)])
    (cond
      [(not (= (context-err result) "")) #f]
      [(> (len (context-stack result)) 1) #f]
      [(= (len (context-stack result)) 0) #f]
      [(= (stack-elmt-type (car (context-stack result))) 1) #f]
      [(> (stack-elmt-value (car (context-stack result))) 0) #t]
      [else #f])))

