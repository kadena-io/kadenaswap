
(namespace (read-msg 'ns))
(module relay GOVERNANCE

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'relay-admin-keyset)))

  (defschema header
    ;; difficulty:string     ;; "0xbfabcdbd93dda"
    hash:string           ;; "0xb3b20624f8f0f86eb50dd04688409e5cea4bd02d700bf6e79e9384d47d6a5a35"
    ;; mix-hash:string       ;; "0x3d1fdd16f15aeab72e7db1013b9f034ee33641d92f71c0736beab4e67d34c7a7"
    ;; nonce:string          ;; "0x4db7a1c01d8a8072"
    number:integer        ;; 6008149
    parent-hash:string    ;; "0x61a8ad530a8a43e3583f8ec163f773ad370329b2375d66433eb82f005e1d6202"
    receipts-root:string  ;; "0x5eced534b3d84d3d732ddbc714f5fd51d98a941b28182b6efe6df3a0fe90004b"
    timestamp:integer     ;; 1532236873
    ;; transactions-root:string ;; "0xf98631e290e88f58a46b7032f025969039aa9b5696498efc76baf436fa69b262"
  )

  (defschema signer
    guard:guard
    stake:decimal
  )

  (deftable signers:{signer})

  (defschema entry
    header:object{header}
    signers:[string]
    weight:decimal)

  (deftable entries:{entry})

  (defschema staked
    token:module{fungible-v2}
    account:string
    total:decimal
    reward-pool:decimal
    reward:decimal
  )

  (deftable stakes:{staked})

  (defcap REWARD ()
    @event true)

  (defcap ADD (hash number signer-count signer)
    @event true)

  (defcap STAKE (signer:string amount:decimal)
    @event true)

  (defcap STAKED (token:string total:decimal)
    @event true)

  (defun init-staking
    ( token:module{fungible-v2}
      account:string
      reward:decimal
    )
    (with-capability (GOVERNANCE)
      (insert stakes (token-key token)
        { 'token:token
        , 'account:account
        , 'total:0.0
        , 'reward-pool:0.0
        , 'reward: reward
        })))

  (defun fund-reward
    ( token:module{fungible-v2}
      from:string
      amount:decimal
    )
    (enforce (>= amount 0.0) "zero amount")
    (let ((key (token-key token)))
      (with-read stakes key
        {'reward-pool:= pooled, 'account:= account }
        (update stakes key {'reward-pool: (+ pooled amount)})
        (token::transfer from account amount)))
  )

  (defun update-reward
    ( token:module{fungible-v2}
      reward:decimal
    )
    (with-capability (GOVERNANCE)
      (enforce (>= reward 0.0) "zero reward")
      (update stakes (token-key token) {'reward: reward }))
  )


  (defun entry-key (header:{header})
    (format "{}:{}" [(at 'number header) (at 'hash header)])
  )

  (defun validate-header:bool (cand:object{header} min-weight:decimal)
    (with-read entries (entry-key header)
      { 'header:= header, 'weight:= weight }
      (enforce (>= weight min-weight) "Minimum weight not met")
      (enforce (= cand header) "Invalid candidate header")
    )
  )

  (defun add (signer:string header:object{header})
    (with-read signers signer
      { 'guard:= guard, 'stake:= stake }
      (enforce-guard guard)
      (let ((key (entry-key header)))
        (with-default-read entries key
          { 'header: header, 'signers: [], 'weight: 0.0 }
          { 'header:= stored, 'signers:= ss, 'weight:= weight }
          (enforce (= header stored) "Mismatched headers")
          (if (contains signer ss) "Already added"
            (with-capability
              (ADD (at 'hash header) (at 'number header)
                   (+ 1 (length ss)) signer)
              (write entries key {
                'header: stored,
                'signers: (+ [signer] ss),
                'weight: (compute-weight weight signer ss) })
              (with-capability (REWARD)
                (reward signer)))))))
  )

  (defun compute-weight:decimal (weight:decimal signer:string ss:[string])
    1.0
  )

  (defun reward (signer:string)
    (require-capability (REWARD))
    ""
  )

  (defun stake
    ( token:module{fungible-v2}
      signer:string
      guard:guard
      amount:decimal
    )
    (let ((key (token-key token)))
      (with-read stakes key
        {'account:= account, 'total:= total }
        (enforce (>= amount 0.0) "zero amount")
        (token::transfer signer account amount)
        (let ((updated-total (+ total amount)))
          (with-capability (STAKED key updated-total)
            (update stakes key {'total: updated-total})))))
    (with-default-read signers signer
      {'guard: guard, 'stake: 0.0 }
      {'guard:= stored-guard, 'stake:= stake }
      (enforce (= guard stored-guard) "guard mismatch")
      (with-capability (STAKE signer amount)
        (write signers signer
          { 'guard: guard, 'stake: (+ stake amount)})))
  )

  (defun token-key (token:module{fungible-v2})
    (format "{}" [token]))

)

(if (read-msg 'upgrade)
  ["upgrade"]
  [ (create-table signers)
    (create-table entries)
    (create-table stakes)
    (init-staking coin (read-msg 'relay-coin-account) 1.0)
  ]
)
