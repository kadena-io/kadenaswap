(enforce-pact-version "3.7")

(namespace (read-msg 'bridge-ns))

;; bridge.toETH
(module toETH GOVERNANCE

  (defconst MODULE_NAME "bridge.toETH")

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'bridge-ns-admin )))

  (defcap MODIFY_AVAILABLE_TOKENS ()
    (enforce-guard (keyset-ref-guard 'bridge-ns-admin )))

  (defcap CONTRACT_ACCT ()
    @doc "For creating contract accounts in other token contracts"
    @managed
    true)

  ; -----------------------------------------------------
  ; Validations
  (defun validate-eth-acct (eth-acct:string)
    ; TODO: check if this is possible/warranted
    true)

  ; -----------------------------------------------------
  ; Tables

  ; This contract will have a contract-account in every integrated token contract
  ; that it can transfer to/from.
  (defschema available-token
    @doc "The table of KDA-side token fungibles that \
         \this contract can interact with (eg wETH, xDAI)"
    ; this is used for dynamic loading/dispatch on the fungible interface
    kda-contract:module{fungible-v2}
    ; module guard's account name
    module-guard-name:string
    ; I'm not sure if these details (eg the ETH contract addy, the eth contract
    ; decimals, etc) should go in here of if it should be elsewhere
    eth-details:object)

  (deftable available-tokens:{available-token})

  (defschema registered-acct
    @doc "Contains the mapping of ETH->KDA accounts"
    kda-acct:string)
  (deftable registry-table:{registered-acct})

  ; -----------------------------------------------------
  ; Bridgable Tokens Section
  (defun make-token-available:string
    (eth-token:string module-guard-name:string kda-contract:module{mintable-v1} erc20-details:object)
    @doc "add erc20 token as available for bridging"
    (require-capability (MODIFY_AVAILABLE_TOKENS))

  ;; TODO -- figure out this schema/interaction
    (let ((minter-details (kda-contract::is-minter-whitelisted MODULE_NAME)))
      (insert available-tokens eth-token
        {"kda-contract"      : kda-contract
        ,"module-guard-name" : module-guard-name
        ,"eth-details"       : erc20-details})

    (format "ETH token `{}` is available for bridging" [eth-token])))

  ; -----------------------------------------------------
  ; User Account Management Section
  (defun create-registration:string (kda-acct:string eth-acct:string)
    @doc "registers a mapping of ETH to KDA accounts. \
         \Ownership of KDA account is not enforced."
    (validate-eth-acct eth-acct)
    ; get-balance fails if kda-acct doesn't exist already
    ; do we want this behavior? There's no reason a bridge
    ; acct needs to be in the coin table
    (coin.get-balance kda-acct)
    (insert registry-table eth-acct
      {"kda-acct":kda-acct})
    (format "KDA Account `{}` registered to receive \
            \wrapped tokens from ETH `{}`"
      [kda-acct eth-acct]))

  (defun update-registration:string (kda-acct:string eth-acct:string)
    @doc "modify an existing registration. \
         \Also use in case someone squats on a mapping"
    (validate-eth-acct eth-acct)
    (require-capability (coin.DEBIT kda-acct))
    (update registry-table eth-acct
      {"kda-acct":kda-acct})
    (format "KDA Account `{}` registered to receive \
            \wrapped tokens from ETH `{}`"
      [kda-acct eth-acct]))

  ; -----------------------------------------------------
  ; Bridge Section





)
