(namespace 'kswap)
(module gas-station GOVERNANCE
  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'swap-ns-admin)))

  (implements gas-payer-v1)
  (use coin)

  (defschema gas
    balance:decimal
    guard:guard)

  (deftable ledger:{gas})

  (defcap GAS_PAYER:bool
    ( user:string
      limit:integer
      price:decimal
    )
    (enforce (= "exec" (at "tx-type" (read-msg))) "Inside an exec")
    (enforce (= 1 (length (at "exec-code" (read-msg)))) "Tx of only one pact function")
    (enforce (= "(kswap." (take 7 (at 0 (at "exec-code" (read-msg))))) "only kswap namespace")
    (enforce (= 0.000000000001 (at "gas-price" (chain-data))) "Must use minimum gas limit")
    (compose-capability (ALLOW_GAS))
  )

  (defcap ALLOW_GAS () true)

  (defun create-gas-payer-guard:guard ()
    (create-user-guard (gas-payer-guard))
  )

  (defun gas-payer-guard ()
    (require-capability (GAS))
    (require-capability (ALLOW_GAS))
  )
)

; (coin.transfer-create "swap-ns-admin" "kswap-free-gas" (kswap.gas-station.create-gas-payer-guard) 5.0)
