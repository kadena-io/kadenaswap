(enforce-pact-version "3.7")

(interface mintable-v1
  "Standard for mint/burn interface with limits. \
  \A bridge module can be whitelisted to perform limited minting/burning"

  ; -----------------------------------------------------
  ; Schema

  (defschema minter
    @doc "Schema for a whitelist table of contracts that can mint \
         \ and their associated limits"
    @model [ (invariant (>= mintable 0))
             (invariant (<= mintable max-mintable)) ]
    acct:string
    mintable:decimal
    max-mintable:decimal
    guard:guard)

  ;------
  ; Capabilities
  (defcap MINT:bool ()
    @doc "Autonomously managed capability for minting tokens"
    @managed)

  (defcap BURN:bool ()
    @doc "Autonomously managed capability for burning tokens"
    @managed)

  (defcap MODIFY_MINTER:bool (guard:guard)
    @doc "The implementing module defines this")

  ; ---
  ; Functionality

  (defun whitelist-minter:string
    (minter-name:string
     minter-acct:string
     max-mintable:string
     guard:guard))

  (defun modify-minter:string
    (name:string
     acct:string
     max-mintable:string
     guard:guard)
     @doc "set max-mintable to 0 to disable a minter")

  (defun is-minter-whitelisted:object{minter}
    (minter-name:string)
    @doc "allows a minter to check if they are whitelisted")

  (defun mint:string
    ( minter-name:string
      receiver:string
      amount:decimal)
    @doc "mint tokens from to user account and update the minter's limit"
    @model [ (property (> amount 0.0))
             (property (!= minter-name ""))
             (property (!= receiver ""))
             (property (!= minter-name receiver))
           ])

  (defun burn:string
    ( minter-name:string
      burning-acct:string
      amount:decimal)
    @doc "burn tokens from a user account and update the minter's limit"
    @model [ (property (> amount 0.0))
             (property (!= minter-name ""))
             (property (!= burning-acct ""))
             (property (!= minter-name burning-acct))
           ])
)
