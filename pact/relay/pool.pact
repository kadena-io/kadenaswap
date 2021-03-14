(namespace (read-msg 'ns))

(module pool GOVERNANCE

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'relay-ns-admin))
  )

  (defconst DAY:integer (* 24 (* 60 60)))

  (use util.guards)

  (defschema pool-schema
    token:module{fungible-v2}
    account:string
    bonded:decimal
    reserve:decimal
    lockup:integer
    bond:decimal
    active:[string]
    activity:integer
    endorsers:integer
    denouncers:integer
    confirm:decimal
    rate:decimal
    fee:decimal
    guard:guard
  )
  (deftable pools:{pool-schema})

  (defun get-pool:object{pool-schema} (id:string)
    (read pools id))

  (defschema bond-schema
    pool:string
    guard:guard
    balance:decimal
    date:time
    lockup:integer
    activity:integer
  )

  (deftable bonds:{bond-schema})

  (defun get-bond:object{bond-schema} (id:string)
    (read bonds id))

  (defcap POOL_ADMIN ()
    (compose-capability (GOVERNANCE)))

  (defcap WITHDRAW (bond:string)
    @managed
    (enforce-guard (at 'guard (get-bond bond)))
  )

  (defcap BOND ( pool:string bond:string bonded:decimal lockup:integer )
    @event true
  )

  (defcap UPDATE ( pool:string bonded:decimal reserve:decimal )
    @event true)

  (defcap FEE ( pool:string bond:string amount:decimal)
    @event true)

  (defun pool-guard () (create-module-guard "pool-bank"))

  (defun init-pool
    ( pool:string
      token:module{fungible-v2}
      account:string
      lockup:integer
      bond:decimal
      activity:integer
      endorsers:integer
      denouncers:integer
      confirm:decimal
      rate:decimal
      fee:decimal
      guard:guard
    )
    (with-capability (POOL_ADMIN)
      (token::create-account account (pool-guard))
      (insert pools pool
        { 'token: token
        , 'account: account
        , 'bonded: 0.0
        , 'reserve: 0.0
        , 'lockup: lockup
        , 'bond: bond
        , 'active: []
        , 'activity: activity
        , 'endorsers:endorsers
        , 'denouncers:denouncers
        , 'confirm:confirm
        , 'rate: rate
        , 'fee: fee
        , 'guard: guard
        })))

  (defun update-pool
    ( pool:string
      lockup:integer
      bond:decimal
      activity:integer
      endorsers:integer
      denouncers:integer
      confirm:decimal
      rate:decimal
      fee:decimal
    )
    (with-capability (POOL_ADMIN)
      (update pools pool
        { 'lockup: lockup
        , 'bond: bond
        , 'activity: activity
        , 'endorsers:endorsers
        , 'denouncers:denouncers
        , 'confirm:confirm
        , 'rate: rate
        , 'fee: fee
        })))

  (defun fund-reserve
    ( pool:string
      account:string
      amount:decimal
    )
    (with-read pools pool
      { 'token:= token:module{fungible-v2}
      , 'reserve:= reserve
      , 'bonded:=bonded
      , 'account:= pool-account }
      (token::transfer account pool-account amount)
      (let ((new-reserve (+ reserve amount)))
        (with-capability (UPDATE pool bonded new-reserve) 1)
        (update pools pool { 'reserve: new-reserve })))
  )

  (defun withdraw-reserve
    ( pool:string
      account:string
      amount:decimal )
    (with-capability (POOL_ADMIN)
      (with-read pools pool
        { 'token:=token:module{fungible-v2}
        , 'reserve:=reserve
        , 'bonded:=bonded
        , 'account:=pool-account
        }
        (install-capability (token::TRANSFER pool-account account amount))
        (token::transfer pool-account account amount)
        (let ((new-reserve (- reserve amount)))
          (with-capability (UPDATE pool bonded new-reserve) 1)
          (update pools pool { 'reserve:new-reserve}))))
  )

  (defun new-bond:string
    ( pool:string
      account:string
      guard:guard
    )
    (with-read pools pool
      { 'token:= token:module{fungible-v2}
      , 'account:= pool-account
      , 'bonded:= bonded
      , 'reserve:=reserve
      , 'lockup:= lockup
      , 'bond:= bond-amount
      , 'rate:=rate
      , 'active:= active }
      (let*
        ( (date (chain-time))
          (bond (format "{}:{}" [account (format-time "%F" date)]))
        )
        (token::transfer account pool-account bond-amount)
        (with-capability (BOND pool account bond-amount lockup) 1)
        (insert bonds bond
          { 'pool: pool
          , 'guard: guard
          , 'balance: bond-amount
          , 'date: date
          , 'lockup: lockup
          , 'activity: 0
          })
        (let ((new-bonded (+ bonded bond-amount)))
          (enforce (> reserve (* 2.0 (* (* rate lockup) new-bonded)))
            "Insufficient reserve")
          (with-capability (UPDATE pool new-bonded reserve) 1)
          (update pools pool
            { 'bonded: new-bonded
            , 'active: (+ active [bond])
            })
          bond)))
  )


  (defun diff-days:integer (a:time b:time)
    (/ (floor (diff-time a b)) DAY))

  (defun withdraw
    ( bond:string
      account:string
    )
    (with-capability (WITHDRAW bond)
      (with-read bonds bond
        { 'pool:= pool
        , 'guard:= guard
        , 'date:= date
        , 'lockup:= lockup
        , 'balance:= balance
        , 'activity:= activity
        }
        (with-read pools pool
          { 'token:= token:module{fungible-v2}
          , 'account:= pool-account
          , 'bonded:= bonded
          , 'reserve:= reserve
          , 'active:= active
          , 'activity:= min-activity
          , 'rate:= rate
          }
          (let* ( (elapsed (diff-days (chain-time) date))
                  (servicing (if (< activity min-activity) 0.0
                                 (* balance (* rate elapsed))))
                  (total (+ balance servicing))
                  (new-bonded (- bonded balance))
                  (new-reserve (- reserve servicing))
                )
            (enforce (> elapsed lockup) "Lockup in force")
            (install-capability (token::TRANSFER pool-account account total))
            (token::transfer pool-account account total)
            (with-capability (UPDATE pool new-bonded new-reserve) 1)
            (update pools pool
              { 'bonded: new-bonded
              , 'reserve: new-reserve
              , 'active: (- active [bond])
              })))))
  )

  (defun pay-fee
    ( bond:string )
    (with-read bonds bond
      { 'pool:= pool
      , 'balance:= balance
      , 'activity:= activity
      }
      (with-read pools pool
        { 'token:= token:module{fungible-v2}
        , 'bonded:= bonded
        , 'reserve:= reserve
        , 'guard:= guard
        , 'fee:=amount
        }
        (enforce-guard guard)
        (with-capability (FEE pool bond amount) 1)
        (update bonds bond
          { 'balance: (+ balance amount)
          , 'activity: (+ activity 1)
          })
        (update pools pool
          { 'bonded: (+ bonded amount)
          , 'reserve: (- reserve amount)
          })))
  )




  (defun pick-active (pool:string endorse:bool bonder:string)
    " Pick random selection of COUNT active bonders, without BONDER, from POOL."
    (with-read pools pool
      { 'active:=active, 'endorsers:= endorsers, 'denouncers:= denouncers }
      (let ( (count (if endorse endorsers denouncers))
             (h (hash [(at 'prev-block-hash (chain-data)) (tx-hash)]))
           )
        (enforce
          (>= (length active) count)
          "Not enough active bonders")
        (at 'picks
          (fold (pick)
            { 'hash: h
            , 'cands: (filter (!= bonder) active)
            , 'picks: []
            }
            (make-list count 0)))))
  )


  (defschema picks
    "Structure for holding random picks"
    hash:string
    cands:[string]
    picks:[string])

  (defun pick:object{picks} (o:object{picks} x_)
    " Accumulator to pick a random candidate using hash value, \
    \ and re-hash hash value."
    (let* ((h0 (at 'hash o))
           (cs (at 'cands o))
           (count (length cs))
           (p (mod (str-to-int 64 h0) count)))
      { 'hash: (hash h0)
      , 'cands: (+ (take p cs)
                   (take (- (+ p 1) count) cs))
      , 'picks: (+ [(at p cs)] (at 'picks o)) }))



)

(if (read-msg 'upgrade)
  ["upgrade"]
  [ (create-table pools)
    (create-table bonds)
  ]
)
