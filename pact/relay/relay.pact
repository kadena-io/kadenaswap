
(namespace (read-msg 'ns))
(module relay GOVERNANCE

  (use util.guards)

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'relay-ns-admin)))

  (defschema header
    ;; difficulty:string
    hash:string
    ;; mix-hash:string
    ;; nonce:string
    number:integer
    ;; parent-hash:string
    receipts-root:string
    ;; timestamp:integer
    ;; transactions-root:string
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

  (defcap ENDORSE
    ( hash:string
      endorser:string
      accepted:bool
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
         )
      (with-default-read heights height
        { 'proposed: "", 'accepted: "", 'inactive: []}
        { 'proposed:= proposed, 'accepted:=accepted, 'inactive:=inactive }
        (enforce (and (!= hash accepted)
                      (not (contains hash inactive)))
                    "Duplicate proposal")
        (enforce (= proposed "") "Already active proposal")
        (let ((endorsers (pool.pick-active pool PICK_ENDORSE proposer)))
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
                [ (enforce (= "" accepted) "Height already accepted")
                  (update heights height { 'proposed:"", 'accepted:hash}) ]
                [])
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
