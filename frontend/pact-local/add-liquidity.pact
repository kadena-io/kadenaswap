(swap.exchange.create-pair coin abc "")
(swap.exchange.add-liquidity
  coin
  abc
  10.0
  40.0
  10.0
  40.0
  "user1"
  "user1"
  (read-keyset 'user-ks)
  (at 'block-time (chain-data))))
