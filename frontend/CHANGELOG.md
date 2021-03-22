# `kadenaswap frontend` Changelog

## (2021-03-22)
- Add price impact field in swap (#121)
- Improve performance in fetching pool stats, pool liquidity and add loader (#78)
- Add preview of signed transaction sent from wallets (Zelcore, Chainweaver) (#111)
- Fix number related issues in removing liquidity (#98)
- Fix error in swap-exact-out transactions (#104)
- Set bounds to slippage and deadlines (#115)

## (2021-01-26)
- Add feature to show user balance of all tokens in the token selector (#45)
- Improve precision by fetching each token's minimum precision instead of using default precision value 12 (#50)
- Improve number formatting by using envData to send transactional data
- Add clickable style to wallet button in header (#61)

## (2021-01-12)
- Gas stations operational!
     - Gas station contract written, tested, and deployed (gas payer account: `kswap-free-gas`)
     - Modify frontend pact calls to include gas station account as gas payer
          - working for wallet
          - working for private key signing methods
     - Show confirmation with 'FREE GAS' screen and informational popup

## (2021-01-08)
- Number related UI fix / improvements
     - Show gas units in user-readable value
     - Show balance without truncated decimal places in liquidity list
     - Add restriction for decimal digits in swap, add/remove liquidity

## (2021-01-07)
- Signing method UI and UX improvements (34, 31)
     - move off default JS alert/prompts
     - better UI for entering password and feedback
     - better modals for using wallet and respective errors

## (2021-01-03)
- slippage NaN error (#26)
- improved token selection UX
- wallet reset button functionality (#43)
     - forget all past wallet info when pressed
- can no longer send multiple transaction on confirm screen (#42)
     - button now disabled if connection is laggy
- precision errors when over 12 decimal places
     - temporary fix before decimal system overhaul
- clean console log (#32))


## (2020-12-30)

- token balance updates
- gas price set to minimum
- token selection bugs
   - faulty search for names
   - no search reset after close
   - show selected token
   - don't allow same token to be chosen for coutnervalue
   - updated swap and pool
- general responsiveness of inputs (swap + pool)
