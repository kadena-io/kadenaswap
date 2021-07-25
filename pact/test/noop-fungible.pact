(module noop-fungible G
  (defcap G () true)
  (implements fungible-v2)
  (defconst GUARD:guard (read-keyset "guard"))
  (defcap TRANSFER:bool
    ( sender:string
      receiver:string
      amount:decimal
    )
    @managed amount TRANSFER-mgr
    true
  )

  (defun TRANSFER-mgr:decimal
    ( managed:decimal
      requested:decimal
    )
    0)

  (defun enforce-unit:bool (amount:decimal) true)

  (defun create-account:string
    ( account:string
      guard:guard
    )
    "doc"
    ""
  )

  (defun get-balance:decimal (account:string)
    1.0
  )

  (defun details:object{fungible-v2.account-details}
    ( account:string )
      { "account" : account
      , "balance" : 1.0
      , "guard": GUARD }
    )

  (defun rotate:string (account:string new-guard:guard)
    "doc"
    "")

  (defun precision:integer ()
    12)

  (defun transfer:string (sender:string receiver:string amount:decimal)
    "doc"
    "")

  (defun transfer-create:string
    ( sender:string
      receiver:string
      receiver-guard:guard
      amount:decimal )
    "doc"
    "")

  (defpact transfer-crosschain:string
    ( sender:string
      receiver:string
      receiver-guard:guard
      target-chain:string
      amount:decimal )
    (step "")
    )

)
