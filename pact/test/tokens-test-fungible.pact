(module tokens-test-fungible G
  " Module for mocking an individual Kadenaswap liquidity token \
  \ as a standalone 'fungible-v2' for coverage by the fungible \
  \ test. Note that when Pact gains implicit arguments for \
  \ modrefs, this will not be needed anymore."
  (defcap G () true)

  (defconst TOKEN:string "token")

  (implements fungible-v2)

  (defschema entry guard:guard version:integer)
  (deftable entries:{entry})

  (defschema caps pending:[object])
  (deftable capstate:{caps})
  (defconst S "SINGLETON")

  (defcap TRANSFER:bool
    ( sender:string
      receiver:string
      amount:decimal
    )
    @managed amount TRANSFER-mgr
    (compose-capability (USER sender (enforce-entry sender)))
    (with-default-read capstate S
      { 'pending: [] }
      { 'pending := caps }
      (write capstate S
        { 'pending: (+ caps
          [ { 'sender: sender, 'receiver: receiver
            , 'amount: amount } ]) })
    )
  )

  (defun install-pending ()
    (with-default-read capstate S
      { 'pending: [] }
      { 'pending := caps }
      (write capstate S { 'pending: [] })
      (map (install) caps)
    )
  )
  (defun install (cap:object)
    (install-capability
      (swap.tokens.TRANSFER TOKEN
        (at 'sender cap)
        (at 'receiver cap)
        (at 'amount cap)
      ))
  )

  (defcap USER (account:string version:integer) true)

  (defun enforce-entry:integer (account:string)
    (let ((e (read entries account)))
      (enforce-guard (at 'guard e))
      (at 'version e)
      )
  )

  (defun TRANSFER-mgr:decimal
    ( managed:decimal
      requested:decimal
    )
    requested
  )

  (defun transfer:string
    ( sender:string
      receiver:string
      amount:decimal
    )
    (with-capability (TRANSFER sender receiver amount)
      (install-pending)
      (swap.tokens.transfer TOKEN sender receiver amount)
    )

  )

  (defun transfer-create:string
    ( sender:string
      receiver:string
      receiver-guard:guard
      amount:decimal
    )
    (with-capability (TRANSFER sender receiver amount)
      (install-pending)
      (swap.tokens.transfer-create
        TOKEN sender receiver receiver-guard amount)
    )
  )

  (defpact transfer-crosschain:string
    ( sender:string
      receiver:string
      receiver-guard:guard
      target-chain:string
      amount:decimal
    )
    (step (enforce false "unsupported"))
  )

  (defun get-balance:decimal
    ( account:string )
    (swap.tokens.get-balance TOKEN account)
  )

  (defun details:object{fungible-v2.account-details}
    ( account: string )
    (remove "token"
      (+ { 'guard: (at 'guard (read entries account))}
        (swap.tokens.details TOKEN account)))
  )

  (defun precision:integer ()
    (swap.tokens.precision TOKEN)
  )

  (defun enforce-unit:bool
    ( amount:decimal )
    (swap.tokens.enforce-unit TOKEN amount)
  )

  (defun delegate-guard (account:string version:integer)
    (create-user-guard (delegate-user-guard account version))
  )

  (defun delegate-user-guard (account:string version:integer)
    (require-capability (USER account version))
  )


  (defun create-account:string
    ( account:string
      guard:guard
    )
    (insert entries account { 'guard: guard, 'version: 0 })
    (swap.tokens.create-account TOKEN account (delegate-guard account 0))
  )

  (defun rotate:string
    ( account:string
      new-guard:guard
    )
    (let ((v (enforce-entry account)))
      (write entries account
        { 'guard: new-guard
        , 'version: (+ 1 v)
      })
      (swap.tokens.rotate TOKEN account (delegate-guard account 0))
    ))

  (defun test-fund
    ( account:string guard:guard amount:decimal )
    (create-account account guard)
    (swap.tokens.credit TOKEN account (delegate-guard account 0) amount)
    true
  )

)
(create-table entries)
(create-table capstate)
