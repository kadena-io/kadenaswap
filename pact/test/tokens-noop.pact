(namespace (read-msg 'ns))

(module tokens G
  (defcap G () true)
  (defconst GUARD:guard (read-keyset "guard"))
  (defun init-issuer (guard:guard) "" "")

  (defun key ( token:string account:string )
    account
  )

  (defun total-supply:decimal (token:string)
    1.0
  )

  (defcap TRANSFER:bool
    ( token:string
      sender:string
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
    0.0)


  (defcap MINT (token:string account:string amount:decimal)
    @managed
    true
  )

  (defcap BURN (token:string account:string amount:decimal)
    @managed
    true
  )

  (defun enforce-unit:bool (token:string amount:decimal)
    true)

  (defun truncate:decimal (token:string amount:decimal)
    amount
  )


  (defun create-account:string
    ( token:string
      account:string
      guard:guard
    )
    "" ""
    )

  (defun get-balance:decimal (token:string account:string)
    1.0
    )

  (defun details
    ( token:string account:string )

    { 'account: account
    , 'balance: 1.0
    , 'guard: GUARD }

    )

  (defun rotate:string (token:string account:string new-guard:guard)
    "" ""
    )


  (defun precision:integer (token:string)
    14)

  (defun transfer:string
    ( token:string
      sender:string
      receiver:string
      amount:decimal
    )
    "" ""
    )

  (defun transfer-create:string
    ( token:string
      sender:string
      receiver:string
      receiver-guard:guard
      amount:decimal
    )
    "" ""
    )

  (defun mint:string
    ( token:string
      account:string
      guard:guard
      amount:decimal
    )
    "" ""
  )

  (defun burn:string
    ( token:string
      account:string
      amount:decimal
    )
    "" ""
  )


  (defun update-supply (token:string amount:decimal)
    "" ""
  )

  (defpact transfer-crosschain:string
    ( token:string
      sender:string
      receiver:string
      receiver-guard:guard
      target-chain:string
      amount:decimal )
    (step (format "{}" [(enforce false "cross chain not supported")]))
    )

  (defun get-tokens () [""])

)
