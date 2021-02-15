(namespace (read-msg 'ns))
(module xyz GOVERNANCE

  (implements fungible-v2)
  (use fungible-util)

  (defschema entry
    balance:decimal
    guard:guard)

  (deftable ledger:{entry})

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'swap-ns-admin)))

  (defcap DEBIT (sender:string)
    (enforce-guard (at 'guard (read ledger sender))))

  (defcap CREDIT (receiver:string) true)

  (defcap TRANSFER:bool
    ( sender:string
      receiver:string
      amount:decimal
    )
    @managed amount TRANSFER-mgr
    (enforce-valid-transfer sender receiver (precision) amount)
    (compose-capability (DEBIT sender))
    (compose-capability (CREDIT receiver))
  )

  (defun TRANSFER-mgr:decimal
    ( managed:decimal
      requested:decimal
    )

    (let ((newbal (- managed requested)))
      (enforce (>= newbal 0.0)
        (format "TRANSFER exceeded for balance {}" [managed]))
      newbal)
  )

  (defconst MINIMUM_PRECISION 14)

  (defun enforce-unit:bool (amount:decimal)
    (enforce-precision (precision) amount))

  (defun create-account:string
    ( account:string
      guard:guard
    )
    (enforce-valid-account account)
    (insert ledger account
      { "balance" : 0.0
      , "guard"   : guard
      })
    )

  (defun get-balance:decimal (account:string)
    (at 'balance (read ledger account))
  )

  (defun details:object{fungible-v2.account-details}
    ( account:string )
    (with-read ledger account
      { "balance" := bal
      , "guard" := g }
      { "account" : account
      , "balance" : bal
      , "guard": g })
    )

  (defun rotate:string (account:string new-guard:guard)
    (with-read ledger account
      { "guard" := old-guard }

      (enforce-guard old-guard)

      (update ledger account
        { "guard" : new-guard }))
    )

  (defun fund:string (account:string amount:decimal)
    (with-capability (GOVERNANCE)
      (with-capability (CREDIT account)
        (credit account
          (at 'guard (read ledger account))
          amount))))

  (defun precision:integer ()
    MINIMUM_PRECISION)

  (defun transfer:string (sender:string receiver:string amount:decimal)

    (enforce (!= sender receiver)
      "sender cannot be the receiver of a transfer")
    (enforce-valid-transfer sender receiver (precision) amount)

    (with-capability (TRANSFER sender receiver amount)
      (debit sender amount)
      (with-read ledger receiver
        { "guard" := g }
        (credit receiver g amount))
      )
    )

  (defun transfer-create:string
    ( sender:string
      receiver:string
      receiver-guard:guard
      amount:decimal )

    (enforce (!= sender receiver)
      "sender cannot be the receiver of a transfer")
    (enforce-valid-transfer sender receiver (precision) amount)

    (with-capability (TRANSFER sender receiver amount)
      (debit sender amount)
      (credit receiver receiver-guard amount))
    )

  (defun debit:string (account:string amount:decimal)

    (require-capability (DEBIT account))
    (with-read ledger account
      { "balance" := balance }

      (enforce (<= amount balance) "Insufficient funds")

      (update ledger account
        { "balance" : (- balance amount) }
        ))
    )


  (defun credit:string (account:string guard:guard amount:decimal)

    (require-capability (CREDIT account))
    (with-default-read ledger account
      { "balance" : 0.0, "guard" : guard }
      { "balance" := balance, "guard" := retg }
      ; we don't want to overwrite an existing guard with the user-supplied one
      (enforce (= retg guard)
        "account guards do not match")

      (write ledger account
        { "balance" : (+ balance amount)
        , "guard"   : retg
        })
      ))


  (defpact transfer-crosschain:string
    ( sender:string
      receiver:string
      receiver-guard:guard
      target-chain:string
      amount:decimal )
    (step (enforce false "cross chain not supported"))
    )

)


(if (read-msg 'upgrade)
  ["upgrade"]
  [(create-table ledger)]
)
