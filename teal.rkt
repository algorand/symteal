#lang rosette

;;; TEAL interpreter in Rosette

;; define the context, TODO: add type guard to the arguments
(struct context (stack program pc next_pc intc bytec) #:transparent)

;; op-spec, roughly same as OpSpec in data/transaction/logic/eval.go
(struct op-spec (name op args returntype) #:transparent)

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
   
             ))

;(define teal-eval-step (program context)