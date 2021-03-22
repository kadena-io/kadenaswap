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
    unlock:integer
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
    account:string
    balance:decimal
    date:time
    lockup:integer
    rate:decimal
    activity:integer
    renewed:integer
    terminated:bool
  )

  (deftable bonds:{bond-schema})

  (defun get-bond:object{bond-schema} (id:string)
    (read bonds id))

  (defun get-active-bond:object{bond-schema} (id:string)
    (let ((bond (get-bond id)))
      (enforce (is-active bond) "Inactive bond")
      bond))

  (defun is-active:bool (bond:object{bond-schema})
    (and (not (at 'terminated bond))
      (< (elapsed-days (at 'date bond)) (at 'lockup bond)))
  )

  (defcap POOL_ADMIN ()
    (compose-capability (GOVERNANCE)))

  (defcap BONDER (bond:string)
    @managed
    (enforce-guard (at 'guard (get-bond bond)))
  )

  (defcap BOND ( pool:string bond:string bonded:decimal lockup:integer renew:integer )
    @event true
  )

  (defcap UNBOND ( pool:string bond:string bonded:decimal )
    @event true
  )

  (defcap FEE ( pool:string bond:string fee:decimal )
    @event true
  )

  (defcap UPDATE ( pool:string bonded:decimal reserve:decimal )
    @event true)

  (defcap ACTIVITY ( pool:string bond:string activity:integer)
    @event true)

  (defun pool-guard () (create-module-guard "pool-bank"))

  (defun init-pool
    ( pool:string
      token:module{fungible-v2}
      account:string
      lockup:integer
      unlock:integer
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
        , 'unlock: unlock
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
      unlock:integer
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
        , 'unlock: unlock
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
      , 'unlock:= unlock
      , 'bond:= bond-amount
      , 'rate:=rate
      , 'active:= active }
      (let*
        ( (date (chain-time))
          (bond (format "{}:{}" [account (format-time "%F" date)]))
          (new-bonded (+ bonded bond-amount))
        )
        (enforce (> reserve (* 2.0 (* (* rate 365) new-bonded)))
          "Insufficient reserve")
        (token::transfer account pool-account bond-amount)
        (with-capability (BOND pool account bond-amount lockup 0) 1)
        (insert bonds bond
          { 'pool: pool
          , 'guard: guard
          , 'account: account
          , 'balance: bond-amount
          , 'date: date
          , 'lockup: lockup
          , 'rate: rate
          , 'activity: 0
          , 'renewed: 0
          , 'terminated: false
          })
        (with-capability (UPDATE pool new-bonded reserve) 1)
        (update pools pool
          { 'bonded: new-bonded
          , 'active: (+ active [bond])
          })
        bond))
  )


  (defun elapsed-days:integer (t:time)
    (/ (floor (diff-time (chain-time) t)) DAY))

  (defun unbond
    ( bond:string )
    (with-capability (BONDER bond)
      (with-read bonds bond
        { 'pool:= pool
        , 'guard:= guard
        , 'account:= account
        , 'date:= date
        , 'lockup:= lockup
        , 'balance:= balance
        , 'activity:= activity
        , 'rate:= rate
        , 'terminated:= terminated
        }
        (enforce (not terminated) "Terminated")
        (with-read pools pool
          { 'token:= token:module{fungible-v2}
          , 'account:= pool-account
          , 'bonded:= bonded
          , 'reserve:= reserve
          , 'active:= active
          , 'activity:= min-activity
          , 'unlock:= unlock
          , 'fee:= fee
          }
          (let* ( (elapsed (elapsed-days date))
                  (risk-fee (if (< activity min-activity) 0.0
                                 (* balance (* rate elapsed))))
                  (activity-fee (* activity fee))
                  (fees (+ risk-fee activity-fee))
                  (total (+ balance fees))
                  (new-bonded (- bonded balance))
                  (new-reserve (- reserve fees))
                )
            (enforce (> elapsed (+ lockup unlock)) "Lockup or unlock in force")
            (install-capability (token::TRANSFER pool-account account total))
            (token::transfer pool-account account total)
            (with-capability (FEE pool bond fees) 1)
            (with-capability (UNBOND pool bond balance) 1)
            (update bonds bond { 'terminated: true })
            (with-capability (UPDATE pool new-bonded new-reserve) 1)
            (update pools pool
              { 'bonded: new-bonded
              , 'reserve: new-reserve
              , 'active: (filter (!= bond) active)
              })))))
  )


  (defun renew
    ( bond:string )
    (with-capability (BONDER bond)
      (with-read bonds bond
        { 'pool:= pool
        , 'guard:= guard
        , 'account:= account
        , 'date:= date
        , 'lockup:= lockup
        , 'balance:= balance
        , 'activity:= activity
        , 'rate:= rate
        , 'terminated:= terminated
        , 'renewed:= renewed
        }
        (enforce (not terminated) "Terminated")
        (with-read pools pool
          { 'token:= token:module{fungible-v2}
          , 'account:= pool-account
          , 'bonded:= bonded
          , 'reserve:= reserve
          , 'active:= active
          , 'activity:= min-activity
          , 'unlock:= unlock
          , 'fee:= fee
          }
          (let* ( (elapsed (elapsed-days date))
                  (risk-fee (if (< activity min-activity) 0.0
                                 (* balance (* rate elapsed))))
                  (activity-fee (* activity fee))
                  (fees (+ risk-fee activity-fee))
                  (new-reserve (- reserve fees))
                  (new-renewed (+ 1 renewed))
                )
            (enforce (>= elapsed lockup) "Bond still active")
            (enforce (< elapsed (+ lockup unlock)) "Unlock period expired")
            (install-capability (token::TRANSFER pool-account account fees))
            (token::transfer pool-account account fees)
            (with-capability (FEE pool bond fees) 1)
            (with-capability (BOND pool account balance lockup new-renewed) 1)
            (update bonds bond
              { 'date: (chain-time)
              , 'activity: 0
              , 'renewed: new-renewed
              })
            (with-capability (UPDATE pool bonded new-reserve) 1)
            (update pools pool
              { 'reserve: new-reserve
              , 'active: (if (contains bond active) active (+ [bond] active))
              })))))
  )


  (defun record-activity
    ( bond:string )
    (with-read bonds bond
      { 'activity:= activity, 'pool:=pool }
      (with-read pools pool
        { 'guard:= guard }
        (enforce-guard guard)
        (let ((new-activity (+ 1 activity)))
          (with-capability (ACTIVITY pool bond new-activity)
            (update bonds bond
              { 'activity: new-activity })))))
  )




  (defun pick-active (pool:string endorse:bool bonder:string)
    " Pick random selection of active bonders, without BONDER, from POOL. \
    \ Count is if ENDORSE endorsers otherwise denouncers from pool config."
    (with-read pools pool
      { 'active:=active, 'endorsers:= endorsers, 'denouncers:= denouncers }
      (let ( (count (if endorse endorsers denouncers))
             (h (hash [(at 'prev-block-hash (chain-data)) (tx-hash)]))
           )
        (enforce
          (>= (length active) count)
          "Not enough active bonders")
        (bind (fold (pick count)
            { 'hash: h
            , 'cands: (filter (!= bonder) active)
            , 'picks: []
            , 'picked: 0
            , 'inactives: []
            }
            active)
          { 'picks:=picks, 'inactives:= inactives }
          (if (= inactives []) ""
            (update pools pool
              { 'active: (filter (in-list inactives) active) }))
          picks)))
  )

  (defun in-list:bool (l:list i)
    (contains i l))


  (defschema picks
    "Structure for holding random picks"
    hash:string
    cands:[string]
    picks:[string]
    picked:integer
    inactives:[string])

  (defun pick:object{picks} (count:integer o:object{picks} x_)
    " Accumulator to pick COUNT random active candidates using hash value, \
    \ and re-hash hash value."
    (if (= (at 'picked o) count) o
      (let* ( (cands (at 'cands o))
              (cand-count (length cands))
            )
        (enforce (> cand-count 0) "Not enough active bonders")
        (let* ( (h0 (at 'hash o))
                (i (mod (str-to-int 64 h0) cand-count))
                (p (at i cands))
                (active (is-active (get-bond p)))
              )
          { 'hash: (hash h0)
          , 'cands: (+ (take i cands)
                       (take (- (+ i 1) cand-count) cands))
          , 'picks: (+ (if active [p] []) (at 'picks o))
          , 'picked: (+ (if active 1 0) (at 'picked o))
          , 'inactives: (+ (if active [] [p]) (at 'inactives o))
          })))
    )



)

(if (read-msg 'upgrade)
  ["upgrade"]
  [ (create-table pools)
    (create-table bonds)
  ]
)
