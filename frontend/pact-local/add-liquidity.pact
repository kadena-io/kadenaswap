(swap.exchange.create-pair coin abc "")
(swap.exchange.add-liquidity
  coin
  abc
  10000.0
  40000.0
  10000.0
  40000.0
  "user1"
  "user1"
  (read-keyset 'user-ks)
  (at 'block-time (chain-data))))
