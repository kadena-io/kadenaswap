(enforce-pact-version "3.7")

(namespace 'swap)

(module tokens GOVERNANCE

  (defschema entry
    token:string
    account:string
    balance:decimal
    guard:guard
    )

  (deftable ledger:{entry})

  (defschema issuer
    guard:guard
  )

  (deftable issuers:{issuer})

  (defschema supply
    supply:decimal
    )

  (deftable supplies:{supply})

  (defconst ISSUER_KEY "I")

  ; (defcap GOVERNANCE () (enforce false "autonomous"))
  (defcap GOVERNANCE () (enforce (keyset-ref-guard 'swap-ns-admin)))

  (defcap DEBIT (token:string sender:string)
    (enforce-guard
      (at 'guard
        (read ledger (key token sender)))))

  (defcap CREDIT (token:string receiver:string) true)


  (defcap ISSUE ()
    (enforce-guard (at 'guard (read issuers ISSUER_KEY)))
  )

  (defcap MINT (token:string account:string amount:decimal)
    @managed ;; one-shot for a given amount
    (compose-capability (ISSUE))
  )

  (defcap BURN (token:string account:string amount:decimal)
    @managed ;; one-shot for a given amount
    (compose-capability (ISSUE))
  )

  (defun init-issuer (guard:guard)
    (insert issuers ISSUER_KEY {'guard: guard})
  )

  (defun key ( token:string account:string )
    (format "{}:{}" [token account])
  )

  (defun total-supply:decimal (token:string)
    (with-default-read supplies token
      { 'supply : 0.0 }
      { 'supply := s }
      s)
  )

  (defcap TRANSFER:bool
    ( token:string
      sender:string
      receiver:string
      amount:decimal
    )
    @managed amount TRANSFER-mgr
    (enforce-unit token amount)
    (enforce (> amount 0.0) "Positive amount")
    (compose-capability (DEBIT token sender))
    (compose-capability (CREDIT token receiver))
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

  (defconst MINIMUM_PRECISION 12)

  (defun enforce-unit:bool (token:string amount:decimal)
    (enforce
      (= (floor amount (precision token))
         amount)
      "precision violation")
  )

  (defun truncate:decimal (token:string amount:decimal)
    (floor amount (precision token))
  )


  (defun create-account:string
    ( token:string
      account:string
      guard:guard
    )
    (insert ledger (key token account)
      { "balance" : 0.0
      , "guard"   : guard
      , "token" : token
      , "account" : account
      })
    )

  (defun get-balance:decimal (token:string account:string)
    (at 'balance (read ledger (key token account)))
    )

  (defun details
    ( token:string account:string )
    (read ledger (key token account))
    )

  (defun rotate:string (token:string account:string new-guard:guard)
    (with-read ledger (key token account)
      { "guard" := old-guard }

      (enforce-guard old-guard)

      (update ledger (key token account)
        { "guard" : new-guard }))
    )


  (defun precision:integer (token:string)
    MINIMUM_PRECISION)

  (defun transfer:string
    ( token:string
      sender:string
      receiver:string
      amount:decimal
    )

    (enforce (!= sender receiver)
      "sender cannot be the receiver of a transfer")

    (with-capability (TRANSFER token sender receiver amount)
      (debit token sender amount)
      (with-read ledger (key token receiver)
        { "guard" := g }
        (credit token receiver g amount))
      )
    )

  (defun transfer-create:string
    ( token:string
      sender:string
      receiver:string
      receiver-guard:guard
      amount:decimal
    )

    (enforce (!= sender receiver)
      "sender cannot be the receiver of a transfer")

    (with-capability (TRANSFER token sender receiver amount)
      (debit token sender amount)
      (credit token receiver receiver-guard amount))
    )

  (defun mint:string
    ( token:string
      account:string
      guard:guard
      amount:decimal
    )
    (with-capability (MINT token account amount)
      (with-capability (CREDIT token account)
        (credit token account guard amount)))
  )

  (defun burn:string
    ( token:string
      account:string
      amount:decimal
    )
    (with-capability (BURN token account amount)
      (with-capability (DEBIT token account)
        (debit token account amount)))
  )

  (defun debit:string
    ( token:string
      account:string
      amount:decimal
    )

    (require-capability (DEBIT token account))

    (enforce-unit token amount)

    (with-read ledger (key token account)
      { "balance" := balance }

      (enforce (<= amount balance) "Insufficient funds")

      (update ledger (key token account)
        { "balance" : (- balance amount) }
        ))
    (update-supply token (- amount))
  )


  (defun credit:string
    ( token:string
      account:string
      guard:guard
      amount:decimal
    )

    (require-capability (CREDIT token account))

    (enforce-unit token amount)

    (with-default-read ledger (key token account)
      { "balance" : 0.0, "guard" : guard }
      { "balance" := balance, "guard" := retg }
      (enforce (= retg guard)
        "account guards do not match")

      (write ledger (key token account)
        { "balance" : (+ balance amount)
        , "guard"   : retg
        , "token"   : token
        , "account" : account
        })

      (update-supply token amount)
      ))

  (defun update-supply (token:string amount:decimal)
    (with-default-read supplies token
      { 'supply: 0.0 }
      { 'supply := s }
      (write supplies token {'supply: (+ s amount)}))
  )

  (defpact transfer-crosschain:string
    ( token:string
      sender:string
      receiver:string
      receiver-guard:guard
      target-chain:string
      amount:decimal )
    (step (enforce false "cross chain not supported"))
    )

  (defun get-tokens ()
    "Get all token identifiers"
    (keys supplies))

)

(create-table ledger)
(create-table issuers)
(create-table supplies)
