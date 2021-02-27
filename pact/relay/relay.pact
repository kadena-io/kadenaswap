
(namespace (read-msg 'ns))
(module relay 'relay-admin-keyset

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

  (defcap REWARD ()
    @event true)

  (defcap ADD (hash number signer-count signer)
    @event true)

  (defun entry-key (header:{eth-header})
    (format "{}:{}" [(at 'number header) (at 'hash header)])
  )

  (defun validate-header:bool (cand:object{eth-header} min-weight:decimal)
    (with-read entries (entry-key header)
      { 'header:= header, 'weight:= weight }
      (enforce (>= weight min-weight) "Minimum weight not met")
      (enforce (= cand header) "Invalid candidate header")
    )
  )

  (defun add (signer:string header:object{header})
    (with-read signers signer
      { 'guard: guard, 'stake: stake }
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

  (defun stake (signer:string amount:decimal)
    1
  )

)
