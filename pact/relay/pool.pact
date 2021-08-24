(namespace (read-msg 'ns))

(module pool GOVERNANCE

  @model [

    ;; prop-pool-write-guard
    (property
     (forall (pool:string)
      (when (row-written pools pool)
       (row-enforced pools 'guard pool)))
     { 'except:
      [ fund-reserve     ;; UNCHECKED
        withdraw-reserve ;; prop-admin-guard
        renew            ;; prop-bond-write-guard
        new-bond         ;; UNCHECKED
        unbond           ;; prop-bond-write-guard
        update-pool      ;; prop-admin-guard
        init-pool        ;; prop-admin-guard
        update-actives   ;; UNCHECKED
      ] } )

    ;; prop-bond-write-guard
    (property
     (forall (bond:string)
      (when (row-written bonds bond)
       (row-enforced bonds 'guard bond)))
     { 'except:
      [ new-bond         ;; UNCHECKED
        slash            ;; prop-pool-guard
        record-activity  ;; prop-pool-guard
        rotate           ;; UNCHECKED
      ] } )

    ;; prop-admin-guard
    (property
     (authorized-by 'relay-ns-admin)
     { 'only:
      [ init-pool
        update-pool
        withdraw-reserve
      ] } )

    ;; prop-pool-guard
    (property
     (forall (pool:string)
      (when (row-read pools pool)
       (row-enforced pools 'guard pool)))
     { 'only:
      [ slash
        record-activity
      ] } )

  ]

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'relay-ns-admin))
  )
  ;; bless v1-mainnet
  (bless "7Nkl4CxzJJFgjTsJrxoas-4qDuWys_GvkEs9rXfeNTc")

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

  (defun pool-keys () (keys pools))

  (defun bond-keys () (keys bonds))

  (defun get-keyed-pool (pool:string)
    { 'key: pool, 'pool: (get-pool pool) })

  (defun get-keyed-bond (bond:string)
    { 'key: bond, 'bond: (get-bond bond)}
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

  (defcap SLASH ( pool:string bond:string slashed:decimal )
    @event true
  )

  (defcap UPDATE ( pool:string bonded:decimal reserve:decimal )
    @event true)

  (defcap ACTIVITY ( pool:string bond:string activity:integer)
    @event true)

  (defcap ROTATE (bond:string)
    "Rotation of bond credentials requires auth from originating account."
    @managed
    (with-read bonds bond
      { 'pool := pool, 'account := account }
      (with-read pools pool
        { 'token := token:module{fungible-v2} }
          (enforce-guard (at 'guard (token::details account)))))
  )

  (defun pool-guard:guard () (create-module-guard "pool-bank"))

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
    @model [
      ;; TODO would like to make statements about transfer operation success
      (property (= (column-delta pools 'reserve) amount)) ;; reserve bumped
      (property (= (column-delta pools 'bonded) 0.0))     ;; bonded constant
      ]
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
        , 'rate:=rate
        }
        (enforce (> reserve amount) "withdraw-reserve: insufficient reserve")
        (let ((committed (* (* rate 365) bonded)))
          (enforce (>= committed amount)
            "withdraw-reserve: violation of committed reserve"))
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
    @model [
     (property (= (column-delta pools 'reserve) 0.0)) ;; reserve constant
     (property
      (= (column-delta pools 'bonded) ;; bonded bumped by pool bond size
         (at 'bond (read pools pool 'before))))
    ]
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
          (let* ( (fees
                    (compute-fees lockup activity min-activity
                      balance rate fee))
                  (elapsed (elapsed-days date))
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
          (let* ( (fees
                    (compute-fees lockup activity min-activity
                      balance rate fee))
                  (elapsed (elapsed-days date))
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

  (defun compute-fees:decimal
    ( lockup:integer
      activity:integer
      min-activity:integer
      balance:decimal
      rate:decimal
      fee:decimal )
    (let*
      ( (risk-fee (if (< activity min-activity) 0.0
                     (* balance (* rate lockup))))
        (activity-fee (* activity fee))
      )
      (+ risk-fee activity-fee))
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

  (defun update-actives (pool:string)
    @model [
     (property (= (column-delta pools 'bonded) 0.0)) ;; bonded unchanged
     (property (= (column-delta pools 'reserve) 0.0)) ;; reserve unchanged
    ]
    (with-read pools pool { 'active:= active }
      (update pools pool
        { 'active:
          (filter
            (compose (get-bond) (is-active))
            active) }))
  )

  (defun rotate
    ( bond:string
      guard:guard
    )
    @model [
     ;; TODO need way to verify that ROTATE checks token guard.
     (property (= (column-delta pools 'bonded) 0.0)) ;; bonded unchanged
     (property (= (column-delta pools 'reserve) 0.0)) ;; reserve unchanged
    ]
    @doc "Rotate bond to new GUARD."
    (with-capability (ROTATE bond)
      (update bonds bond
        { 'guard: guard }))
  )


  (defun slash (bond:string)
    (with-read bonds bond
      { 'activity:= activity
      , 'pool:=pool
      , 'balance:=balance
      }
      (with-read pools pool
        { 'guard:= guard
        , 'token:= token:module{fungible-v2}
        , 'account:= pool-account
        , 'bonded:= bonded
        , 'reserve:= reserve
        }
        (enforce-guard guard)
        (let*
          ( (slashed (* balance 0.5))
            (new-balance (- balance slashed))
            (new-bonded (- bonded slashed))
            (new-reserve (+ reserve slashed))
          )
          (emit-event (SLASH pool bond slashed))
          (update bonds bond { 'balance: new-balance })
          (emit-event (UPDATE pool new-bonded new-reserve))
          (update pools pool
            { 'reserve: new-reserve
            , 'bonded: new-bonded }))))
  )

  (defun pick-active:[string]
    ( pool:string
      endorse:bool
      bonders:[string]
     )
    " Pick random selection of active bonders, without BONDERS, from POOL. \
    \ Count is if ENDORSE endorsers otherwise denouncers from pool config."
    (update-actives pool)
    (with-read pools pool
      { 'active:=active
      , 'endorsers:= endorsers
      , 'denouncers:= denouncers
      , 'guard:= guard }
      (enforce-guard guard)
      (let ( (count (if endorse endorsers denouncers))
             (h (hash [(at 'prev-block-hash (chain-data)) (tx-hash)]))
             (cands (filter (compose (in-list bonders) (not)) active))
             (init-picks:[string] [])
           )
        (enforce
          (>= (length cands) count)
          (format "pick-active: not enough active bonders {}, need {}"
            [(length cands) count]))
        (sort (at 'picks
          (fold (pick count)
            { 'hash: h
            , 'cands: cands
            , 'picks: init-picks
            , 'picked: 0
            }
            active)))))
  )

  (defun in-list:bool (l:list i:string)
    (contains i l))


  (defschema picks
    "Structure for holding random picks"
    hash:string
    cands:[string]
    picks:[string]
    picked:integer)

  (defun pick:object{picks} (count:integer o:object{picks} x_:string)
    " Accumulator to pick COUNT random active candidates using hash value, \
    \ and re-hash hash value."
    (if (= (at 'picked o) count) o
      (let* ( (cands (at 'cands o))
              (cand-count (length cands))
            )
        (enforce (> cand-count 0) "pick: insufficent active bonders")
        (let* ( (h0 (at 'hash o))
                (i (mod (str-to-int 64 h0) cand-count))
                (p (at i cands))
              )
          { 'hash: (hash h0)
          , 'cands: (+ (take i cands)
                       (take (- (+ i 1) cand-count) cands))
          , 'picks: (+ [p] (at 'picks o))
          , 'picked: (+ 1 (at 'picked o))
          })))
    )



)

(if (read-msg 'upgrade)
  ["upgrade"]
  [ (create-table pools)
    (create-table bonds)
  ]
)
