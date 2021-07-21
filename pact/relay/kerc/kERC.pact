(namespace (read-msg 'ns))
(module kERC GOVERNANCE

  "Wrapping test fungible token."

  (implements fungible-v2)
  (use kswap.fungible-util)

  (defschema entry
    balance:decimal
    guard:guard)

  (deftable ledger:{entry})

  (defschema wrap-data
    account:string ;; account in this ledger
    wraps:[string] ;; list of "wrap ids" that uniquely identify a wrap tx
  )

  (deftable wraps:{wrap-data}) ;; keyed by ethereum address without 0x pfx

  (defcap GOVERNANCE ()
    (enforce-guard
      (keyset-ref-guard 'swap-ns-admin)))

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

  (defconst MINIMUM_PRECISION 18)

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

  (defconst ERC_ADDRESS
    (read-msg "erc-address")
    "Address of wrapped ERC-20 contract")

  (defconst ERC20_TRANSFER
    "0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"
    "Topic 0 value for ERC20 transfers"
    )

  (defconst LOCKER_ACCOUNT
    (read-msg "locker-account") ;; no 0x prefix
    "Address of kERC relay locker account")

  (defconst TYPE_TOPIC 0 "Topic index of event type")
  (defconst FROM_TOPIC 1 "Topic index of ERC-20 TRANSFER 'from' argument")
  (defconst TO_TOPIC 2 "Topic index of ERC-20 TRANSFER 'to' argument")

  (defconst ADDRESS_PREFIX 26 "Chars to drop at beginning of eth addy topic")

  (defconst MINIMUM_DEPTH (read-integer "minimum-depth")
    "Minimum valid block depth for Ethereum proof.")

  (defcap WRAP (from:string account:string wrapid:string amount:decimal)
    @event true)

  (defcap MINT (account:string amount:decimal)
    true)

  (defcap REGISTER (wrap-account:string account:string)
    @managed
    (enforce-guard (at 'guard (read ledger account)))
  )

  (defun register (wrap-account:string account:string)
    (with-capability (REGISTER wrap-account account)
      (insert wraps wrap-account { 'account: account, 'wraps: [] }))
  )

  (defun wrap ()
    " Wrap/mint tokens corresponding to a lockup on another platform \
    \ as supported by the relay. Expects an SPV proof of the relevant \
    \ ERC-20 TRANSFER event in msg data at `proof`. Enforces minimum depth."
    (let*
      ( (result (verify-spv 'ETH (read-msg 'proof )))
        (header (at 'header result))
        (receipt (at 'receipt result))
      )
      (enforce (>= (at 'depth result) MINIMUM_DEPTH) "insufficient depth")
      (relay.relay.validate
        { 'hash: (at 'hash header)
        , 'number: (at 'number header)
        , 'receipts-root: (at 'receipts-root header)
        })
      (let*
        ( (ev (at 0 (at 'logs receipt)))
          (wrapid (at 'root result)) ;; TODO is this a good index?
          (amount (/ (str-to-int 16 (drop 2 (at 'data ev)))
                     (+ 0.0 (^ 10 MINIMUM_PRECISION)))) ;; precision should match eth
          (topics (at 'topics ev))
          (from (drop ADDRESS_PREFIX (at FROM_TOPIC topics)))
          (to (drop ADDRESS_PREFIX (at TO_TOPIC topics)))
          (wrapdata (read wraps from))
          (prev-wraps (at 'wraps wrapdata))
          (account (at 'account wrapdata))
        )
        (enforce (= ERC20_TRANSFER (at TYPE_TOPIC topics)) "not ERC-20 TRANSFER")
        (enforce (= ERC_ADDRESS (at 'address ev)) "not from ERC contract")
        (enforce (not (contains wrapid prev-wraps)) "duplicate wrap")
        (enforce (= LOCKER_ACCOUNT to) "bad TO value")
        (emit-event (WRAP from account wrapid amount))
        (update wraps from { 'wraps: (+ prev-wraps [wrapid])})
        (with-capability (MINT account amount)
          (mint account amount))))
  )

  (defun mint (account:string amount:decimal)
    (require-capability (MINT account amount))
    (with-capability (CREDIT account)
      (credit account (at 'guard (read ledger account)) amount)
      (emit-event (TRANSFER "" account amount))
    )
  )

)

(if (read-msg 'upgrade)
  ["upgrade"]
  [(create-table ledger)
   (create-table wraps)
  ]
)
