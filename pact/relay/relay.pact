
(namespace (read-msg 'ns))
(module relay GOVERNANCE

  (use util.guards)

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'relay-admin-keyset)))

  (defconst DAY:integer (* 24 (* 60 60)))

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

  ;; stake is single-token
  (defschema signer
    guard:guard
    stake:decimal
    pool:string
    start:time
    lockup:integer
  )

  (deftable signers:{signer})

  (defschema entry
    header:object{header}
    signers:[string]
    weight:decimal)

  (deftable entries:{entry})

  ;; invariant: balance = stakes+rewards
  (defschema pool
    token:module{fungible-v2}
    account:string
    stakes:decimal
    rewards:decimal
    reward:decimal
    min-lockup:integer
  )

  (deftable pools:{pool})

  (defcap REWARD ()
    @event true)

  (defcap ADD ( hash:string number:integer signer-count:integer
                signer:string weight:decimal)
    @event true)

  (defcap STAKE (signer:string amount:decimal lockup:integer)
    @event true)

  (defcap STAKED (token:string total:decimal)
    @event true)

  (defun init-pool
    ( token:module{fungible-v2}
      account:string
      reward:decimal
      min-lockup:integer
    )
    (with-capability (GOVERNANCE)
      (insert pools (token-key token)
        { 'token:token
        , 'account:account
        , 'stakes:0.0
        , 'rewards:0.0
        , 'reward: reward
        , 'min-lockup: min-lockup
        })))

  (defun fund-reward
    ( token:module{fungible-v2}
      from:string
      amount:decimal
    )
    (enforce (>= amount 0.0) "zero amount")
    (let ((key (token-key token)))
      (with-read pools key
        {'rewards:= rewards, 'account:= account }
        (update pools key {'rewards: (+ rewards amount)})
        (token::transfer from account amount)))
  )

  (defun update-reward
    ( token:module{fungible-v2}
      reward:decimal
    )
    (with-capability (GOVERNANCE)
      (enforce (>= reward 0.0) "zero reward")
      (update pools (token-key token) {'reward: reward }))
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
      { 'guard:= guard, 'pool:= pool
      , 'stake:= stake, 'start:= start, 'lockup:= lockup }
      (enforce-guard guard)
      (let ((key (entry-key header)))
        (with-default-read entries key
          { 'header: header, 'signers: [], 'weight: 0.0 }
          { 'header:= stored, 'signers:= ss, 'weight:= weight }
          (enforce (= header stored) "Mismatched headers")
          (enforce (not (contains signer ss)) "Already added")
          (let ((new-weight
                  (compute-weight pool weight stake start lockup)))
            (with-capability
              (ADD (at 'hash header) (at 'number header)
                   (+ 1 (length ss)) signer new-weight)
              (write entries key {
                'header: stored,
                'signers: (+ [signer] ss),
                'weight: new-weight }))
              (with-capability (REWARD)
                (reward signer))))))
  )

  (defun compute-weight:decimal
    ( pool:string
      current-weight:decimal
      stake:decimal
      start:time
      lockup:integer
    )
    (with-read pools pool
      {'stakes:= stakes, 'reward:= reward }
      (enforce (> stakes 0.0) "No stakes")
      (let*
        ( (total (+ stakes reward))
          (remaining (- lockup (diff-days start (chain-time))))
          (scaled-stake
            (* stake (/ (- lockup remaining) lockup)))
          (weight (/ scaled-stake total))
        )
        weight))
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
      lockup:integer
    )
    (let ((key (token-key token))
          (now (chain-time)))
      (with-read pool key
        {'account:= account, 'staked:= staked, 'min-lockup:= min-lockup }
        (enforce (>= amount 0.0) "negative amount")
        (token::transfer signer account amount)
        (let ((updated-staked (+ staked amount)))
          (with-capability (STAKED key updated-staked)
            (update pools key {'staked: updated-staked})))
        (with-default-read signers signer
          { 'guard: guard, 'stake: 0.0,
            'start: now, 'lockup: lockup }
          { 'guard:= stored-guard, 'stake:= stake,
            'start:= start, 'lockup:= plockup }
          (enforce (= guard stored-guard) "guard mismatch")
          (enforce (>= lockup min-lockup) "insufficient lockup")
          (let* ((in-lockup (< (diff-days now start) plockup))
                 (new-start (if in-lockup start now)))
            (if in-lockup (enforce (>= lockup plockup) "invalid lockup reduction")
              true)
            (with-capability (STAKE signer amount lockup)
              (write signers signer
                { 'guard: guard, 'stake: (+ stake amount), 'pool: key
                , 'start: new-start, 'lockup: lockup
                }))))))
  )

  (defun diff-days:integer (a:time b:time)
    (/ (diff-time a b) DAY))

  (defun token-key (token:module{fungible-v2})
    (format "{}" [token]))

)

(if (read-msg 'upgrade)
  ["upgrade"]
  [ (create-table signers)
    (create-table entries)
    (create-table pools)
    (init-pool coin (read-msg 'relay-coin-account) 1.0 60)
  ]
)
