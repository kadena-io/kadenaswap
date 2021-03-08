
(namespace (read-msg 'ns))
(module relay GOVERNANCE

  (use util.guards)

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'relay-admin-keyset)))

  (defschema header
    ;; difficulty:string
    hash:string
    ;; mix-hash:string
    ;; nonce:string
    number:integer
    parent-hash:string
    receipts-root:string
    timestamp:integer
    ;; transactions-root:string
  )

  (defconst POOL "kda-relay-pool")

  (defschema entry
    header:object{header}
    bonds:[string]
    endorsers:[string]
  )

  (deftable entries:{entry})

  (defun entry-key (header:{header})
    (format "{}:{}" [(at 'number header) (at 'hash header)])
  )

  (defun pool-module-guard () (create-module-guard "pool"))

)

(if (read-msg 'upgrade)
  ["upgrade"]
  [ (create-table entries)
    (pool.init-pool
      POOL
      coin
      (read-msg 'relay-coin-account)
      (read-integer 'lockup)
      (read-decimal 'bond)
      (read-integer 'activity)
      (read-decimal 'rate)
      (pool-module-guard)
    )
  ]
)
