(enforce-pact-version "3.7")

(namespace 'swap)

(module exchange GOVERNANCE

  (defcap GOVERNANCE () (enforce false "autonomous"))

  (defcap CREATE_PAIR
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2} )
    @managed
    ;; dupes checked in 'get-pair-create'
    true)

  (defcap ISSUANCE () true)

  (defschema pair
    tokenA:module{fungible-v2}
    tokenB:module{fungible-v2}
    account:string
    guard:guard
    reserveA:decimal
    reserveB:decimal
    )

  (deftable pairs:{pair})

  (defconst MINIMUM_LIQUIDITY 0.1)

  (defun init ()
    (tokens.init-issuer (create-module-guard "issuance"))
  )

  (defun get-pair:object{pair}
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    (read pairs (get-pair-key tokenA tokenB)))

  (defun pair-exists:bool
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    (with-default-read pairs
      (get-pair-key tokenA tokenB)
      { 'account: "" }
      { 'account := a }
      (> (length a) 0))
  )

  (defun ensure-deadline (deadline:time)
    (enforce (>= deadline (at 'block-time (chain-data)))
      "expired")
  )

  (defun add-liquidity
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
      amountADesired:decimal
      amountBDesired:decimal
      amountAMin:decimal
      amountBMin:decimal
      account-id:string
      account-guard:guard
      deadline:time
    )
    (ensure-deadline deadline)
    (let*
      ( (p (get-pair tokenA tokenB))
        (canon (is-canonical tokenA tokenB))
        (rA (at 'reserveA p))
        (rB (at 'reserveB p))
        (reserveA (if canon rA rB))
        (reserveB (if canon rB rA))
        (amounts
          (if (and (= reserveA 0.0) (= reserveB 0.0))
            [amountADesired amountBDesired]
            (let ((amountBOptimal (quote amountADesired reserveA reserveB)))
              (if (<= amountBOptimal amountBDesired)
                (let ((x (enforce (>= amountBOptimal amountBMin)
                           "add-liquidity: insufficient B amount")))
                  [amountADesired amountBOptimal])
                (let ((amountAOptimal (quote amountBDesired reserveB reserveA)))
                  (enforce (<= amountAOptimal amountADesired)
                    "add-liquidity: optimal A less than desired")
                  (enforce (>= amountAOptimal amountAMin)
                    "add-liquidity: insufficient A amount")
                  [amountAOptimal amountBDesired])))))
        (amountA (at 0 amounts))
        (amountB (at 1 amounts))
        (pair-account (at 'account p))
      )
      ;; transfer
      (tokenA::transfer account-id pair-account amountA)
      (tokenB::transfer account-id pair-account amountB)
      ;; mint
      (let*
        ( (token0:module{fungible-v2} (at 'tokenA p))
          (token1:module{fungible-v2} (at 'tokenB p))
          (balance0 (token0::get-balance pair-account))
          (balance1 (token1::get-balance pair-account))
          (reserve0 (at 'reserveA p))
          (reserve1 (at 'reserveB p))
          (amount0 (- balance0 reserve0))
          (amount1 (- balance1 reserve1))
          (key (get-pair-key tokenA tokenB))
          (totalSupply (tokens.total-supply key))
          (liquidity
            (if (= 0.0 totalSupply)
              (with-capability (ISSUANCE)
                (mint key pair-account (at 'guard p) MINIMUM_LIQUIDITY)
                (- (sqrt (* amount0 amount1)) MINIMUM_LIQUIDITY))
              (let ((l0 (/ (* amount0 totalSupply) reserve0))
                    (l1 (/ (* amount1 totalSupply) reserve1))
                   )
                ;; need min, max
                (if (<= l0 l1) l0 l1))))
        )
        (enforce (> liquidity 0.0) "mint: insufficient liquidity minted")
        (with-capability (ISSUANCE)
          (mint key account-id account-guard liquidity))
        (update pairs key
          { 'reserveA: balance0
          , 'reserveB: balance1
          })
      )
    )
  )

  (defun mint (token:string to:string guard:guard amount:decimal)
    (require-capability (ISSUANCE))
    (let ((a (tokens.round-unit token amount)))
      (install-capability (tokens.MINT token to a))
      (tokens.mint token to guard a)
    )
  )

  (defun quote
    ( amountA:decimal
      reserveA:decimal
      reserveB:decimal
    )
    (enforce (> amountA 0.0) "quote: insufficient amount")
    (enforce (and (> reserveA 0.0) (> reserveB 0.0)) "quote: insufficient liquidity")
    (/ (* amountA reserveB) reserveA)
  )


  (defun remove-liquidity
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
      liquidity:decimal
      amountAMin:decimal
      amountBMin:decimal
      to:string
      deadline:time )

    (ensure-deadline deadline)

    (let* ( (p (get-pair tokenA tokenB))
            (pair-account (at 'account p))
            (pair-key (get-pair-key tokenA tokenB))
          )
      (tokens.transfer pair-key to pair-account liquidity)
      (let*
        ( (token0:module{fungible-v2} (at 'tokenA p))
          (token1:module{fungible-v2} (at 'tokenB p))
          (balance0 (token0::get-balance pair-account))
          (balance1 (token1::get-balance pair-account))
          (liquidity_ (tokens.get-balance pair-key pair-account))
          (total-supply (tokens.total-supply pair-key))
          (amount0 (/ (* liquidity_ balance0) total-supply))
          (amount1 (/ (* liquidity_ balance1) total-supply))
        )
        (enforce (and (> amount0 0.0) (> amount1 0.0))
          "remove-liquidity: insufficient liquidity burned")
        (with-capability (ISSUANCE)
          (burn pair-key pair-account liquidity))
        ;;TODO fix defcap dynamic bug
        ;(install-capability (token0::TRANSFER pair-account to amount0))
        (token0::transfer pair-account to amount0)
        ;(install-capability (token1::TRANSFER pair-account to amount1))
        (token1::transfer pair-account to amount1)
        (update pairs pair-key
          { 'reserveA: (token0::get-balance pair-account)
          , 'reserveB: (token1::get-balance pair-account)
          })
      )
    )
  )

  (defun burn (token:string to:string amount:decimal)
    (require-capability (ISSUANCE))
    (let ((a (tokens.round-unit token amount)))
      (install-capability (tokens.BURN token to a))
      (tokens.burn token to a)
    )
  )

  (defun create-pair:object{pair}
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
      hint:string
      )
    (let* ((key (get-pair-key tokenA tokenB))
           (a (create-pair-account key hint))
           (g (create-module-guard key))
           (p { 'tokenA: tokenA
              , 'tokenB: tokenB
              , 'account: a
              , 'guard: g
              , 'reserveA: 0.0
              , 'reserveB: 0.0
              })
           )
      (install-capability (CREATE_PAIR tokenA tokenB))
      (with-capability (CREATE_PAIR tokenA tokenB)
        (insert pairs key p)
        (tokenA::create-account a g)
        (tokenB::create-account a g)
        p)))

  (defun get-pair-key
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    " Create canonical key for pair. \
    \ TODO pair upgrades could break canonicity, \
    \ might need a `modref-name` primitive, or \
    \ modrefs should not output interfaces in string rep."
    (format "{}:{}" (canonicalize tokenA tokenB))
  )

  (defun canonicalize:[module{fungible-v2}]
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    (if (is-canonical tokenA tokenB) [tokenA tokenB] [tokenB tokenA])
  )

  (defun is-canonical
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    (< (format "{}" [tokenA]) (format "{}" [tokenB]))
  )

  (defun create-pair-account:string
    ( key:string hint:string)
    (hash (+ hint (+ key (format "{}" [(at 'block-time (chain-data))]))))
  )
)

(create-table pairs)

(init)
