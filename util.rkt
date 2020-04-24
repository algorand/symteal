#lang racket

(require "ledger.rkt" "teal.rkt" "config.rkt")

(provide (all-defined-out))

; recursively convert member of struct from bv to nat
(define (bv-struct->nat s)
  (match s
    [(ledger-state accounts leases) (ledger-state (map bv-struct->nat accounts)
                                                  (map bv-struct->nat leases))]
    [(account-state balance assets program) (account-state (bv->nat balance)
                                                           (map bv->nat assets)
                                                           program)]
    [(lease sender value last-valid) (lease (bv->nat sender)
                                            (bv->nat value)
                                            (bv->nat last-valid))]
    [(txn-content args sender fee first_valid first_valid_time last_valid
                  note lease receiver amount close_remainder_to
                  vote_pk selection_pk vote_first vote_last vote_key_dilution
                  type type_enum xfer_asset asset_amount asset_sender asset_receiver
                  asset_close_to tx_id)
     (txn-content args (bv->nat sender) (bv->nat fee) (bv->nat first_valid) (bv->nat first_valid_time)
                  (bv->nat last_valid) (bv->nat note) (bv->nat lease) (bv->nat receiver)
                  (bv->nat amount) (bv->nat close_remainder_to) (bv->nat vote_pk) (bv->nat selection_pk)
                  (bv->nat vote_first) (bv->nat vote_last) (bv->nat vote_key_dilution) (bv->nat type)
                  (bv->nat type_enum) (bv->nat xfer_asset) (bv->nat asset_amount) (bv->nat asset_sender)
                  (bv->nat asset_receiver) (bv->nat asset_close_to) (bv->nat tx_id))]))
