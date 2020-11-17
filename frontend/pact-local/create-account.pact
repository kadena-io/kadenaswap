(coin.create-account "user1" (read-keyset 'user-ks))
(coin.coinbase "user1" (read-keyset 'user-ks)   500.0)

(abc.create-account "user1" (read-keyset 'user-ks))
(abc.fund  "user1" 500.0)

(xyz.create-account "user1" (read-keyset 'user-ks))
(xyz.fund  "user1" 500.0)
