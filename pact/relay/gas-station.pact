(namespace (read-msg 'ns))

(module gas-station GOVERNANCE
  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'relay-ns-admin)))

  (implements gas-payer-v1)
  (use coin)
  (use util.guards1)

  (defschema gas
    balance:decimal
    guard:guard)

  (defconst GAS_STATION "relay-free-gas")

  (defcap GAS_PAYER:bool
    (
    user:string
    limit:integer
    price:decimal
    )
    (enforce (= "exec" (at "tx-type" (read-msg))) "Inside an exec")
    (enforce (= 1 (length (at "exec-code" (read-msg)))) "Tx of only one pact function")
    (enforce (= "(relay." (take 7 (at 0 (at "exec-code" (read-msg))))) "only relay namespace")
    (enforce (= 0.00000001 (at "gas-price" (chain-data))) "Must use minimum gas limit")
    (compose-capability (ALLOW_GAS))
  )

  (defcap ALLOW_GAS () true)

  (defun init ()
    (coin.create-account GAS_STATION
      (guard-any
        [
          (create-gas-payer-guard)
          (keyset-ref-guard 'relay-ns-admin)
        ]))
  )

  (defun create-gas-payer-guard:guard ()
    (create-user-guard (gas-payer-guard))
  )

  (defun gas-payer-guard ()
    (require-capability (GAS))
    (require-capability (ALLOW_GAS))
  )
)


(if (read-msg 'upgrade)
  ["upgrade"]
  [(init)]
)

(coin.details "relay-free-gas")
