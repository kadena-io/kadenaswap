
(namespace (read-msg 'ns))
(module relay GOVERNANCE

  (use util.guards)

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
