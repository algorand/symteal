#lang rosette/safe

(require "ledger.rkt" "config.rkt")

(provide periodic-payment)

(define (periodic-payment tmpl_rcv tmpl_period tmpl_dur tmpl_amt tmpl_lease tmpl_timeout tmpl_fee)
  (list
   (txn TypeEnum)
   (int (bv 1 bv64))
   (eq)
   (txn Fee)
   (int tmpl_fee)
   (le)
   (land) ; is Payment and ok Fee
   (txn CloseRemainderTo)
   (global ZeroAddress)
   (eq)
   (txn Receiver)
   (addr tmpl_rcv)
   (eq)
   (land)
   (txn Amount)
   (int tmpl_amt)
   (eq)
   (land)
   (txn FirstValid)
   (int tmpl_period)
   (mod)
   (int (bv 0 bv64))
   (eq)
   (land)
   (txn LastValid)
   (int tmpl_dur)
   (txn FirstValid)
   (plus)
   (eq)
   (land)
   (txn Lease)
   (byte tmpl_lease)
   (eq)
   (land) ; on period and correct duration and good lease and no close and good Receiver and good amount
   (txn CloseRemainderTo)
   (addr tmpl_rcv)
   (eq)
   (txn Receiver)
   (global ZeroAddress)
   (eq)
   (land)
   (txn FirstValid)
   (int tmpl_timeout)
   (ge)
   (land)
   (txn Amount)
   (int (bv 0 bv64))
   (eq)
   (land) ; good close to and after timeout and 0 Amount
   (lor) ; normal payment or close
   (land) ; (normal payment or close) and preamble checks
   ))


