
(namespace (read-msg 'ns))
(module relay GOVERNANCE

  (use util.guards)

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'relay-ns-admin)))

  (defschema header
    hash:string
    number:integer
    receipts-root:string
  )

  (defconst POOL "kda-relay-pool")

  (defschema height-schema
    proposed:string
    accepted:string
    inactive:[string])

  (deftable heights:{height-schema})

  (defconst BLOCK_PROPOSED 0)
  (defconst BLOCK_ACCEPTED 1)
  (defconst BLOCK_WITHDRAWN -1)
  (defconst BLOCK_DENOUNCED -2)

  (defconst PICK_ENDORSE true)
  (defconst PICK_DENOUNCE false)

  (defschema proposal-schema
    header:object{header}
    pool:string
    status:integer
    proposer:string
    endorsers:[string]
    endorsed:[string]
    denouncer:string
    denouncers:[string]
    denounced:[string]
  )

  (deftable proposals:{proposal-schema})

  (defun get-proposal (phash:string) (read proposals phash))

  (defcap PROPOSE
    ( height:integer
      hash:string
      proposer:string
      endorsers:[string]
    )
    @event
    true
  )

  (defcap DENOUNCE
    ( height:integer
      hash:string
      denouncer:string
      denouncers:[string]
    )
    @event
    true
  )

  (defcap ENDORSE
    ( hash:string
      endorser:string
      accepted:bool
    )
    @event
    true
  )

  (defcap ENDORSE-DENOUNCE
    ( hash:string
      endorser:string
      denounced:bool
    )
    @event
    true
  )

  (defcap BONDER ( bond:string )
    (enforce-guard (at 'guard (pool.get-bond bond)))
  )

  (defun entry-key (header:object{header})
    (format "{}:{}" [(at 'number header) (at 'hash header)])
  )

  (defun get-height (header:object{header})
    (int-to-str 10 (at 'number header)))

  (defun propose
    ( header:object{header} proposer:string )
    (let ( (height (get-height header))
           (hash (at 'hash header))
           (pool (at 'pool (pool.get-active-bond proposer)))
           (e:[string] [])
         )
      (with-default-read heights height
        { 'proposed: "", 'accepted: "", 'inactive: e}
        { 'proposed:= proposed, 'accepted:=accepted, 'inactive:=inactive }
        (enforce (and (!= hash accepted)
                      (not (contains hash inactive)))
                    "Duplicate proposal")
        (enforce (= proposed "") "Already active proposal")
        (let ((endorsers (pool.pick-active pool PICK_ENDORSE [proposer])))
          (with-capability
            (PROPOSE (at 'number header) hash proposer endorsers) 1)
          (with-capability (BONDER proposer)
            (insert proposals hash
              { 'header:header
              , 'pool:pool
              , 'status:BLOCK_PROPOSED
              , 'proposer:proposer
              , 'endorsers: endorsers
              , 'endorsed:[]
              , 'denouncer:""
              , 'denouncers:[]
              , 'denounced:[]
              })
            (write heights height
              { 'proposed: hash, 'accepted: accepted, 'inactive: inactive })
        ))))
  )

  (defun endorse
    ( header:object{header} endorser:string )
    (let ((hash (at 'hash header))
          (height (get-height header)))
      (with-read heights height
        { 'proposed:=proposed, 'accepted:= accepted }
        (enforce (= hash proposed) "Block not proposed at height")
        (with-read proposals hash
          { 'header:=stored, 'pool:=pool-id, 'status:=status
          , 'endorsers:=endorsers, 'endorsed:=endorsed }
          (enforce (= header stored) "Header mismatch")
          (enforce (= status BLOCK_PROPOSED) "Invalid status")
          (enforce (contains endorser endorsers) "Invalid endorser")
          (enforce (not (contains endorser endorsed)) "Duplicate endorse")
          (with-capability (BONDER endorser)
            (let*
              ( (pool (pool.get-pool pool-id))
                (count (+ 1 (length endorsed)))
                (needed (ceiling (* (at 'confirm pool) (length endorsers))))
                (is-accepted (>= count needed))
              )
              (with-capability (ENDORSE hash endorser is-accepted) 1)
              (update proposals hash
                { 'endorsed: (+ [endorser] endorsed)
                , 'status: (if is-accepted BLOCK_ACCEPTED BLOCK_PROPOSED)
                })
              (if is-accepted
                (let ((msg "Hash already accepted at height"))
                  (enforce (!= hash accepted) msg)
                  (update heights height { 'proposed:"", 'accepted:hash}))
                "skip")
              (pool.record-activity endorser))))))
  )

  (defun validate:bool ( header:object{header} )
    (let* ((hash (at 'hash header))
           (height (get-height header))
           (accepted (at 'accepted (read heights height))))
      (enforce (= hash accepted) "Not accepted")
      (with-read proposals hash
        { 'header:=stored }
        (enforce (= header stored) "Header mismatch")))
    true)


  (defun denounce
    ( header:object{header} denouncer:string )
    (let ( (height (get-height header))
           (hash (at 'hash header))
           (pool (at 'pool (pool.get-active-bond denouncer)))
         )
      (with-read heights height
        { 'accepted:=accepted }
        (enforce (= hash accepted) "Block not accepted at height")
        (with-read proposals hash
          { 'header:=stored
          , 'status:=status
          , 'proposer:=proposer
          , 'endorsers:=endorsers
          , 'denouncer:=old-denouncer }
          (enforce (= header stored) "Header mismatch")
          (enforce (= status BLOCK_ACCEPTED) "Invalid status")
          (enforce (= "" old-denouncer) "Already denounced")
          (let*
            ( (skip (+ [denouncer proposer] endorsers))
              (denouncers (pool.pick-active pool PICK_DENOUNCE skip))
            )
            (emit-event
              (DENOUNCE (at 'number header) hash denouncer denouncers))
            (with-capability (BONDER denouncer)
              (update proposals hash
                { 'header:header
                , 'denouncer: denouncer
                , 'denouncers: denouncers
                }))))))
  )


  (defun endorse-denounce
    ( header:object{header} endorser:string )
    (let ((hash (at 'hash header))
          (height (get-height header)))
      (with-read heights height
        { 'accepted:= accepted, 'inactive:= inactive }
        (enforce (= hash accepted) "Block not accepted at height")
        (with-read proposals hash
          { 'header:=stored, 'pool:=pool-id, 'status:=status
          , 'proposer:=proposer, 'endorsers:=endorsers
          , 'endorsed:=endorsed
          , 'denouncers:=denouncers, 'denounced:=denounced }
          (enforce (= header stored) "Header mismatch")
          (enforce (= status BLOCK_ACCEPTED) "Invalid status")
          (enforce (contains endorser denouncers) "Invalid endorser")
          (enforce (not (contains endorser denounced)) "Duplicate denounce")
          (with-capability (BONDER endorser)
            (let*
              ( (pool (pool.get-pool pool-id))
                (count (+ 1 (length denounced)))
                (needed (ceiling (* (at 'confirm pool) (length denouncers))))
                (is-denounced (>= count needed))
              )
              (emit-event (ENDORSE-DENOUNCE hash endorser is-denounced))
              (update proposals hash
                { 'denounced: (+ [endorser] denounced)
                , 'status: (if is-denounced BLOCK_DENOUNCED BLOCK_ACCEPTED)
                })
              (if is-denounced
                (let ((new-inactive (+ [hash] inactive)))
                  (update heights height
                    { 'accepted: ""
                    , 'inactive: new-inactive})
                  (pool.slash proposer)
                  (map (pool.slash) endorsed))
                ["skip"])
              (pool.record-activity endorser))))))
  )

  (defun pool-module-guard () (create-module-guard "pool"))

)

(if (read-msg 'upgrade)
  ["upgrade"]
  [ (create-table heights)
    (create-table proposals)
    (pool.init-pool
      POOL
      coin
      (read-msg 'relay-coin-account)
      (read-integer 'lockup)
      (read-integer 'unlock)
      (read-decimal 'bond)
      (read-integer 'activity)
      (read-integer 'endorsers)
      (read-integer 'denouncers)
      (read-decimal 'confirm)
      (read-decimal 'rate)
      (read-decimal 'fee)
      (pool-module-guard)
    )
  ]
)
