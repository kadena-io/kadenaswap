(namespace (read-msg 'ns))

(module pool GOVERNANCE

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'relay-admin-keyset))
  )

  (defconst DAY:integer (* 24 (* 60 60)))

  (use util.guards)

  (defschema pool-schema
    token:module{fungible-v2}
    account:string
    bonded:decimal
    reward:decimal
    lockup:integer
    bond:decimal
    active:[string]
    activity:integer
    rate:decimal
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

  (defcap POOL-ADMIN ()
    (compose-capability (GOVERNANCE)))

  (defun pool-guard () (create-module-guard "pool-bank"))

  (defun init-pool
    ( pool-id:string
      token:module{fungible-v2}
      account:string
      lockup:integer
      bond:decimal
      activity:integer
      rate:decimal
      guard:guard
    )
    (with-capability (POOL-ADMIN)
      (token::create-account account (pool-guard))
      (insert pools pool-id
        { 'token: token
        , 'account: account
        , 'bonded: 0.0
        , 'reward: 0.0
        , 'lockup: lockup
        , 'bond: bond
        , 'active: []
        , 'activity: activity
        , 'rate: rate
        , 'guard: guard
        })))

  (defun update-pool
    ( pool-id:string
      lockup:integer
      bond:decimal
      activity:integer
      rate:decimal
    )
    (with-capability (POOL-ADMIN)
      (update pools pool-id
        { 'lockup: lockup
        , 'bond: bond
        , 'activity: activity
        , 'rate: rate
        })))

  (defun fund-reward
    ( pool-id:string
      account:string
      amount:decimal
    )
    (with-read pools pool-id
      { 'token:= token:module{fungible-v2}
      , 'reward:= reward
      , 'account:= pool-account }
      (token::transfer account pool-account amount)
      (update pools pool-id { 'reward: (+ reward amount) }))
  )

  (defun new-bond:string
    ( pool-id:string
      account:string
      guard:guard
    )
    (with-read pools pool-id
      { 'token:= token:module{fungible-v2}
      , 'account:= pool-account
      , 'bonded:= bonded
      , 'lockup:= lockup
      , 'bond:= bond
      , 'active:= active }
      (let*
        ( (date (chain-time))
          (bond-id (format "{}:{}" [account date]))
        )
        (token::transfer account pool-account bond)
        (insert bonds bond-id
          { 'pool: pool-id
          , 'guard: guard
          , 'balance: bond
          , 'date: date
          , 'lockup: lockup
          })
        (update pools pool-id
          { 'bonded: (+ bonded bond)
          , 'active: (+ active [bond-id])
          })
        bond-id))
  )


  (defun diff-days:integer (a:time b:time)
    (/ (diff-time a b) DAY))

  (defun withdraw
    ( pool-id:string
      bond-id:string
      account:string
    )
    (with-read bonds bond-id
      { 'pool:= pool-id
      , 'guard:= guard
      , 'date:= date
      , 'lockup:= lockup
      , 'balance:= balance
      , 'activity:= activity
      }
      (enforce-guard guard)
      (with-read pools pool-id
        { 'token:= token:module{fungible-v2}
        , 'account:= pool-account
        , 'bonded:= bonded
        , 'reward:= reward
        , 'active:= active
        , 'activity:= min-activity
        , 'rate:= rate
        }
        (let* ( (elapsed (diff-days (chain-time) date))
                (interest (if (< activity min-activity) 0.0
                            (* balance (* rate elapsed))))
                (return (+ balance interest))
              )
          (enforce (> elapsed lockup) "Lockup in force")
          (install-capability (token::TRANSFER pool-account account return))
          (token::transfer pool-account account return)
          (update pools pool-id
            { 'bonded: (- bonded balance)
            , 'reward: (- reward interest)
            , 'active: (- active [bond-id])
            }))))
  )

  (defun reward
    ( bond-id:string
      amount:decimal )
    (with-read bonds bond-id
      { 'pool:= pool-id
      , 'balance:= balance
      , 'activity:= activity
      }
      (with-read pools pool-id
        { 'token:= token:module{fungible-v2}
        , 'bonded:= bonded
        , 'reward:= reward
        , 'guard:= guard
        }
        (enforce-guard guard)
        (update bonds bond-id
          { 'balance: (+ balance amount)
          , 'activity: (+ activity 1)
          })
        (update pools pool-id
          { 'bonded: (+ bonded amount)
          , 'reward: (- reward amount)
          })))
  )

)

(if (read-msg 'upgrade)
  ["upgrade"]
  [ (create-table pools)
    (create-table bonds)
  ]
)
