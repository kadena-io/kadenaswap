(enforce-pact-version "3.7")

(namespace (read-msg 'bridge-ns))

;; bridge.toETH
(module toETH GOVERNANCE

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'bridge-ns-admin )))

  (defcap MODIFY_AVAILABLE_TOKENS ()
    (enforce-guard (keyset-ref-guard 'bridge-ns-admin )))

  ; -----------------------------------------------------
  ; Validations
  (defun validate-eth-acct (eth-acct:string)
    ; TODO: check if this is possible/warranted
    true)

  ; -----------------------------------------------------
  ; Tables

  ; This contract will have a contract-account in every integrated token contract
  ; that it can transfer to/from
  (defschema available-token
    @doc "The table of KDA-side token fungibles that
    this contract can interact with (eg wETH, xDAI)"
    ; this is used for dynamic loading/dispatch on the fungible interface
    contract-name:string
    ; I'm not sure if these details (eg the ETH contract addy, the eth contract
    ; decimals, etc) should go in here of if it should be elsewhere
    eth-details:object)

  (deftable available-tokens:{available-token})

  (defschema registered-acct
    @doc "Contains the mapping of ETH->KDA accounts"
    kda-acct:string)
  (deftable registry-table:{registered-acct})

  ; -----------------------------------------------------
  ; Bridge Contract
  (defun register (kda-acct:string eth-acct:string)
    @doc "registers a mapping of ETH to KDA accounts"
    (validate-eth-acct eth-acct)
    ; get-balance fails if kda-acct doesn't exist already
    ; do we want this behavior? There's no reason a bridge
    ; acct needs to be in the coin table
    (coin.get-balance kda-acct)
    (insert registery-table eth-acct
      {"kda-acct":kda-acct}))


)
