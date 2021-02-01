(namespace (read-msg 'ns))
(module kpenny GOVERNANCE

  (implements fungible-v2)
  (use swap.fungible-util)
  (use util.guards)

  (defschema conversion
    account:string
    amount-kda:decimal
    amount-kpenny:decimal)

  (deftable conversions:{conversion})

  (defschema entry
    balance:decimal
    guard:guard)

  (deftable ledger:{entry})

  (defcap GOVERNANCE ()
    (enforce-guard
      (keyset-ref-guard 'swap-ns-admin)))

  (defcap DEBIT (sender:string)
    (enforce-guard (at 'guard (read ledger sender))))

  (defcap CREDIT (receiver:string) true)

  (defcap FUND () true)

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

  (defconst MINIMUM_PRECISION:integer 14)

  (defconst CONVERSION_RATE:decimal 1000000.0)

  (defconst KPENNY_BANK:string 'kpenny-bank)

  (defconst SWAP_DEADLINE (time "2021-03-15T00:00:00Z"))

  (defconst FINAL_DEADLINE (time "2021-03-30T00:00:00Z"))

  (defun kpenny-bank-guard () (create-module-guard 'kpenny-admin))

  (defun init ()
    (coin.create-account KPENNY_BANK (kpenny-bank-guard))
  )

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


  (defun fund:string (account:string amount:decimal guard:guard)
    (with-capability (CREDIT account)
      (require-capability (FUND))
        (credit account guard amount))
  )

  (defun convert:string (account:string amount-kda:decimal)
    (enforce-guard (before-date SWAP_DEADLINE))
    (coin.transfer account KPENNY_BANK amount-kda)
    (let
      ( (tx-id (hash {"account": account, "amount": amount-kda, "salt": (at "block-time" (chain-data))}))
        (amount-kpenny (* amount-kda CONVERSION_RATE))
        (g (at 'guard (coin.details account)))
      )
      (insert conversions (format "{}-{}" [account, tx-id])
        { "account"        : account
        , "amount-kda"     : amount-kda
        , "amount-kpenny"  : amount-kpenny
        })
      (with-capability (FUND)
        (fund account amount-kpenny g))
    )
  )

  (defun redeem:string (account:string)
    (enforce-guard (at-after-date SWAP_DEADLINE))
    (with-read ledger account
      { "balance" := amount-kpenny,
        "guard" := g
      }
      (with-capability (DEBIT account)
        (debit account amount-kpenny))
      (let (
        (amount-kda (/ amount-kpenny CONVERSION_RATE)))
        (coin.transfer KPENNY_BANK account amount-kda)
        (format
          "{} KPenny redeemed at {} KDA for {}"
          [amount-kpenny amount-kda account])))
  )

  (defun precision:integer ()
    MINIMUM_PRECISION)

  (defun transfer:string (sender:string receiver:string amount:decimal)
    (enforce-guard (before-date FINAL_DEADLINE))
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

  (defun read-conversions (account:string)
    (select conversions (where 'account (= account)))
  )

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
  [ (create-table conversions)
    (create-table ledger)
    (init)
  ]
)
