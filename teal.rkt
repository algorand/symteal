#lang rosette

;;; TEAL interpreter in Rosette

;; define the context, TODO: add type guard to the arguments.
(struct context (stack program pc next_pc err intc bytec) #:transparent)

;; stack element.
(struct stack-elmt (uint bytes) #:transparent)

;; op-spec, roughly same as OpSpec in data/transaction/logic/eval.go.
(struct op-spec (name op args returntype) #:transparent)

;; reflect the type of an stack element:
;; 0 is uint.
;; 1 is bytes.
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
   (op-spec "btoi" op-btoi '("Bytes") "Uint64")
   (op-spec "%" op-mod '("Uint64" "Uint64") "Uint64")
   (op-spec "|" op-bitor '("Uint64" "Uint64") "Uint64")
   (op-sepc "&" op-bitand '("Uint64" "Uint64") "Uint64")
   (op-spec "^" op-bitxor '("Uint64" "Uint64") "Uint64")
   (op-spec "~" op-bitnot '("Uint64") "Uint64")

   (op-spec "intcblock" op-intcblock '() "None")
   (op-spec "intc" op-intc '() "Uint64")
   (op-spec "bytecblock" op-bytecblock '() "None")
   (op-spec "bytec" op-bytec '() "Bytes")
   (op-spec "arg" op-arg '() "Bytes")
   (op-spec "txn" op-txn '() "Any")
   (op-spec "global" op-globle '() "Any")
   (op-spec "bnz" op-bnz '("Uint64") "None")
   (op-spec "pop" op-pop '("Any") "None")
   (op-spec "dup" op-dup '() "Any")
             ))

; add an error message to the context.
(define (add-err cxt msg)
  (context (context-stack cxt)
           (context-program cxt)
           (context-pc cxt)
           (context-next_pc cxt)
           msg
           (context-intc cxt)
           (context-bytec cxt)))

(define (update-stack cxt ns)
  (context ns
           (context-program cxt)
           (context-pc cxt)
           (context-next_pc cxt)
           (context-err cxt)
           (context-intc cxt)
           (context-bytec cxt)))

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

; eq
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


;(define teal-eval-step (program context) nil)

