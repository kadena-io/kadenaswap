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

  (defschema leg
    token:module{fungible-v2}
    reserve:decimal
    )

  (defschema pair
    leg0:object{leg}
    leg1:object{leg}
    account:string
    guard:guard
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

  (defun enforce-deadline (deadline:time)
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
    (enforce-deadline deadline)
    (let*
      ( (p (get-pair tokenA tokenB))
        (reserveA (reserve-for p tokenA))
        (reserveB (reserve-for p tokenB))
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
        ( (token0:module{fungible-v2} (at 'token (at 'leg0 p)))
          (token1:module{fungible-v2} (at 'token (at 'leg1 p)))
          (balance0 (token0::get-balance pair-account))
          (balance1 (token1::get-balance pair-account))
          (reserve0 (at 'reserve (at 'leg0 p)))
          (reserve1 (at 'reserve (at 'leg1 p)))
          (amount0 (- balance0 reserve0))
          (amount1 (- balance1 reserve1))
          (key (get-pair-key tokenA tokenB))
          (totalSupply (tokens.total-supply key))
          (liquidity
            (if (or (= 0.0 reserve0) (= 0.0 reserve1))
              (let ((l (sqrt (* amount0 amount1))))
                (if (= totalSupply 0.0)
                  (with-capability (ISSUANCE)
                    (mint key pair-account (at 'guard p) MINIMUM_LIQUIDITY)
                    (- l MINIMUM_LIQUIDITY))
                  l))
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
          { 'leg0: { 'token: token0
                   , 'reserve: balance0 }
          , 'leg1: { 'token: token1
                   , 'reserve: balance1 }
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

    (enforce-deadline deadline)

    (let* ( (p (get-pair tokenA tokenB))
            (pair-account (at 'account p))
            (pair-key (get-pair-key tokenA tokenB))
          )
      (tokens.transfer pair-key to pair-account liquidity)
      (let*
        ( (token0:module{fungible-v2} (at 'token (at 'leg0 p)))
          (token1:module{fungible-v2} (at 'token (at 'leg1 p)))
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
          { 'leg0: { 'token: token0
                   , 'reserve: (token0::get-balance pair-account) }
          , 'leg1: { 'token: token1
                   , 'reserve: (token1::get-balance pair-account) }
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

  (defschema alloc
    token:module{fungible-v2}
    qty:decimal
    idx:integer
    pair:object{pair}
    path:[module{fungible-v2}]
  )

  (defun swap-exact-in
    ( amountIn:decimal
      amountOutMin:decimal
      path:[module{fungible-v2}]
      to:string
      guard:guard
      deadline:time
    )
    (enforce-deadline deadline)
    (let*
      ( (p1 (get-pair (at 0 path) (at 1 path)))
        (allocs
          (fold (compute-out)
            [ { 'token: (at 0 path)
              , 'qty: amountIn
              , 'idx: 0
              , 'pair: p1
              , 'path: path
              }]
            (drop 1 path)))
      )
      (enforce (>= (at 'qty (at 0 allocs)) amountOutMin)
        "swap-exact-in: insufficient output amount")
      (swap to guard (reverse allocs))
    )
  )

  (defconst FEE 0.003)

  (defun compute-out
    ( allocs:[object{alloc}]
      token-out:module{fungible-v2}
    )
    (let*
      ( (head:object{alloc} (at 0 allocs))
        (token-in:module{fungible-v2} (at 'token head))
        (amountIn:decimal (at 'qty head))
        (p (get-pair token-in token-out))
        (reserveIn (reserve-for p token-in))
        (reserveOut (reserve-for p token-out))
        (amountInWithFee (* (- 1.0 FEE) amountIn))
        (numerator (* amountInWithFee reserveOut))
        (denominator (+ reserveIn amountInWithFee))
      )
      (+ [ { 'token: token-out
           , 'qty: (round-unit token-out (/ numerator denominator))
           , 'idx: (+ 1 (at 'idx head))
           , 'pair: p
           , 'path: (drop 1 (at 'path head))
           } ]
         allocs)
    )
  )

  (defun swap
    ( to:string
      guard:guard
      allocs:[object{alloc}]
    )
    (let*
      ( (head:object{alloc} (at 0 allocs))
        (head-token:module{fungible-v2} (at 'token head))
      )
      (head-token::transfer to (at 'account (at 'pair head)) (at 'qty head))
      (map (swap-leg (- (length allocs) 1) to guard) (drop 1 allocs))
    )
  )

  (defun swap-leg
    ( last:integer
      to:string
      guard:guard
      alloc:object{alloc}
    )
    (let*
      ( (amount-out (at 'qty alloc))
        (p (at 'pair alloc))
        (account (at 'account p))
        (token:module{fungible-v2} (at 'token alloc))
        (reserve-out (reserve-for p token))
        (path (at 'path alloc))
        (is-last (= last (at 'idx alloc)))
        (recipient
          (if is-last to
            (at 'account (get-pair (at 0 path) (at 1 path)))))
        (recip-guard
          (if is-last guard
            (at 'guard (get-pair (at 0 path) (at 1 path)))))
      )
      (enforce (> amount-out 0.0) "swap-leg: insufficient output")
      (enforce (< amount-out reserve-out) "swap-leg: insufficient liquidity")
      (enforce (!= recipient account) "swap-leg: invalid TO")
      ;; TODO install modref caps
      (token::transfer-create account recipient recip-guard (at 'qty alloc))
      (let*
        ( (leg0 (at 'leg0 p))
          (leg1 (at 'leg1 p))
          (token0:module{fungible-v2} (at 'token leg0))
          (token1:module{fungible-v2} (at 'token leg1))
          (balance0 (token0::get-balance account))
          (balance1 (token1::get-balance account))
          (reserve0 (at 'reserve leg0))
          (reserve1 (at 'reserve leg1))
          (amount0Out (if (is-leg0 p token) amount-out 0.0))
          (amount1Out (if (is-leg0 p token) 0.0 amount-out))
          (amount0In (if (> balance0 (- reserve0 amount0Out))
                        (- balance0 (- reserve0 amount0Out))
                        0.0))
          (amount1In (if (> balance1 (- reserve1 amount1Out))
                        (- balance1 (- reserve1 amount1Out))
                        0.0))
          (balance0adjusted (- balance0 (* amount0In FEE)))
          (balance1adjusted (- balance1 (* amount1In FEE)))
        )
        (enforce (or (> amount0In 0.0) (> amount1In 0.0))
          "swap-leg: insufficient input amount")
        (enforce (>= (* balance0adjusted balance1adjusted)
                     (* reserve0 reserve1))
          "swap-leg: K")
        (update pairs (get-pair-key token0 token1)
          { 'leg0: { 'token: token0
                   , 'reserve: balance0 }
          , 'leg1: { 'token: token1
                   , 'reserve: balance1 }
          })
      )
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
           (p { 'leg0: { 'token: tokenA, 'reserve: 0.0 }
              , 'leg1: { 'token: tokenB, 'reserve: 0.0 }
              , 'account: a
              , 'guard: g
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

  (defun is-leg0:bool
    ( p:object{pair}
      token:module{fungible-v2}
    )
    (let ((token0 (at 'token (at 'leg0 p))))
      (= (format "{}" [token])
         (format "{}" [token0]))) ;; TODO modref equality
  )

  (defun leg-for:object{leg}
    ( p:object{pair}
      token:module{fungible-v2}
    )
    (if (is-leg0 p token)
      (at 'leg0 p)
      (at 'leg1 p))
  )

  (defun reserve-for:decimal
    ( p:object{pair}
      token:module{fungible-v2}
    )
    (at 'reserve (leg-for p token))
  )

  (defun create-pair-account:string
    ( key:string hint:string)
    (hash (+ hint (+ key (format "{}" [(at 'block-time (chain-data))]))))
  )

  (defun round-unit (token:module{fungible-v2} amount:decimal)
    (floor amount (token::precision))
  )
)

(create-table pairs)

(init)
