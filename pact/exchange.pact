(enforce-pact-version "3.7")

(namespace 'swap)

(module exchange GOVERNANCE

  (defcap GOVERNANCE () true)

  (defcap CREATE_PAIR
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2} )
    @managed
    ;; dupes checked in 'get-pair-create'
    true)

  (defschema pair
    tokenA:module{fungible-v2}
    tokenB:module{fungible-v2}
    supply:decimal
    account:string
    guard:guard
    reserveA:decimal
    reserveB:decimal
    )

  (deftable pairs:{pair})

  (defconst MINIMUM_LIQUIDITY 0.1)

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
                    "add-liquidity: optimal A greater than desired")
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
        ( (balance0 (tokenA::get-balance pair-account))
          (balance1 (tokenB::get-balance pair-account))
          (reserve0 (at 'reserveA p))
          (reserve1 (at 'reserveB p))
          (amount0 (- balance0 reserve0))
          (amount1 (- balance1 reserve1))
          (totalSupply (at 'supply p))
          (liquidity
            (if (= 0.0 totalSupply)
              (let ((liq (- (sqrt (* amount0 amount1)) MINIMUM_LIQUIDITY)))
                (mint pair-account (at 'guard p) MINIMUM_LIQUIDITY)
                liq)
              (let ((l0 (/ (* amount0 totalSupply) reserve0))
                    (l1 (/ (* amount1 totalSupply) reserve1))
                   )
                ;; need min, max
                (if (<= l0 l1) l0 l1))))
        )
        (enforce (> liquidity 0.0) "mint: insufficient liquidity minted")
        (mint account-id account-guard liquidity)
        (update pairs (get-pair-key tokenA tokenB)
          { 'reserveA: balance0
          , 'reserveB: balance1
          })
      )
    )
  )


  (defun mint (to:string guard:guard amount:decimal)
    1
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
              , 'supply: 0.0
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
