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
    balance:decimal
    account:string
    guard:guard
    reserveA:decimal
    reserveB:decimal
    )

  (deftable pairs:{pair})

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
    (let*
      ( (p (get-pair-create tokenA tokenB))
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
      )
      (tokenA::transfer-create account-id (at 'account p) (at 'guard p) amountA)
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


  (defun get-pair-create:object{pair}
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
      )
    (let ((key (get-pair-key tokenA tokenB)))
      (with-default-read pairs key
        { 'balance: -1.0 }
        { 'balance := balance }
        (if (= balance -1.0)
          (let ((new-pair
                { 'tokenA: tokenA
                , 'tokenB: tokenB
                , 'balance: 0.0
                , 'account: (create-pair-account key)
                , 'guard: (create-module-guard key)
                , 'reserveA: 0.0
                , 'reserveB: 0.0
                }))
            (install-capability (CREATE_PAIR tokenA tokenB))
            (with-capability (CREATE_PAIR tokenA tokenB)
              (insert pairs key new-pair)
              new-pair))
          (read pairs key))))
  )

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
    ( key:string )
    (hash (+ key (format "{}" [(at 'block-time (chain-data))])))
  )
)

(create-table pairs)
