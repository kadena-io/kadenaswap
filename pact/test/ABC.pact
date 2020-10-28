(namespace 'test)
(module abc G
  (defcap G () (enforce false "autonomous"))
  (implements fungible-v2)
  (defcap TRANSFER:bool
    (sender:string receiver:string amount:decimal)
    @managed amount TRANSFER-mgr
    true)
  (defun TRANSFER-mgr:decimal
    (managed:decimal requested:decimal) 1.0)
  (defun transfer:string
    (sender:string receiver:string amount:decimal) "" "")
  (defun transfer-create:string
    (sender:string receiver:string receiver-guard:guard amount:decimal) "" "")
  (defpact transfer-crosschain:string
    (sender:string receiver:string receiver-guard:guard target-chain:string amount:decimal)
    (step ""))
  (defun rotate:string
    (account:string new-guard:guard) "" "")
  (defun get-balance:decimal
    (account:string) 0.0)
  (defun details:object{fungible-v2.account-details}
    (account:string) {})
  (defun precision:integer () 12.0)
  (defun create-account:string (account:string guard:guard) "" "")
  (defun enforce-unit:bool (amount:decimal) true)
  )
