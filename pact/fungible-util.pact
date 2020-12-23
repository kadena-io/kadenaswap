
(namespace (read-msg 'ns))

(module fungible-util GOVERNANCE

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'swap-ns-admin)))

  (defun enforce-valid-amount
    ( precision:integer
      amount:decimal
    )
    (enforce (> amount 0.0) "Positive non-zero amount")
    (enforce-precision precision amount)
  )

  (defun enforce-valid-account (account:string)
    (enforce (> (length account) 2) "minimum account length")
  )

  (defun enforce-precision
    ( precision:integer
      amount:decimal
    )
    (enforce
      (= (floor amount precision) amount)
      "precision violation")
  )

  (defun enforce-valid-transfer
    ( sender:string
      receiver:string
      precision:integer
      amount:decimal)
    (enforce (!= sender receiver)
      "sender cannot be the receiver of a transfer")
    (enforce-valid-amount precision amount)
    (enforce-valid-account sender)
    (enforce-valid-account receiver)
  )




)
