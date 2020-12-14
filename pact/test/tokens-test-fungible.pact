(module tokens-test-fungible G
  " Module for mocking an individual Kadenaswap liquidity token \
  \ as a standalone 'fungible-v2' for coverage by the fungible \
  \ test. Note that when Pact gains implicit arguments for \
  \ modrefs, this will not be needed anymore."
  (defcap G () true)

  (defconst TOKEN:string "token")

  (implements fungible-v2)

  (defschema entry guard:guard version:integer)
  (deftable entries:{entry}
    "Track GUARD and maintain VERSION for delegate guard")

  (defschema caps pending:[object])
  (deftable capstate:{caps}
    "Singleton storage for accumulating delegate TRANSFER cap install")
  (defconst S "SINGLETON")

  (defcap TRANSFER:bool
    ( sender:string
      receiver:string
      amount:decimal
    )
    @managed amount TRANSFER-mgr
    (enforce-entry sender)
    ;; managed caps are called on install, track
    ;; for delegate install in transfer function
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
    "Install pending delegate TRANSFER caps"
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

  (defcap DELEGATE (account:string version:integer)
    "Delegate of user guard to underlying token custody."
    true)

  (defun enforce-entry:integer (account:string)
    "Enforce tracked guard and return current delegate version."
    (let ((e (read entries account)))
      (enforce-guard (at 'guard e))
      (at 'version e)
      )
  )

  (defun TRANSFER-mgr:decimal
    ( managed:decimal
      requested:decimal
    )
    "All-pass manager so underlying can be tested."
    requested
  )

  (defun transfer:string
    ( sender:string
      receiver:string
      amount:decimal
    )
    (with-capability (TRANSFER sender receiver amount)
      (install-pending)
      (with-capability
        (DELEGATE sender (at 'version (read entries sender)))
        (swap.tokens.transfer TOKEN sender receiver amount))
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
      (with-default-read entries receiver
        { 'guard: receiver-guard, 'version: 0 }
        { 'guard := g, 'version:= v }
        (let
          ( (del-guard
              (if (= receiver-guard g)
                ;; correct case, write and return current delegate
                (let ((d (delegate-guard receiver v)))
                  (write entries receiver { 'guard: g, 'version: v })
                  d)
                ;; incorrect case, pass invalid delegate to ensure enforcement
                (delegate-guard receiver (+ 1 v)))) )
          (with-capability (DELEGATE sender (at 'version (read entries sender)))
            (swap.tokens.transfer-create
              TOKEN sender receiver del-guard amount))))
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
    (create-user-guard (require-delegate account version))
  )

  (defun require-delegate (account:string version:integer)
    (require-capability (DELEGATE account version))
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
    " Enforces tracked user guard, increments delegate version \
    \ while calling underlying with old delegate in scope to \
    \ enforce old guard."
    (let*
      ( (v (enforce-entry account))
        (v1 (+ 1 v))
      )
      (write entries account
        { 'guard: new-guard
        , 'version: v1
      })
      (with-capability (DELEGATE account v)
        (swap.tokens.rotate TOKEN account
          (delegate-guard account v1)))
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
