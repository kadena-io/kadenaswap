(coin.create-account "user1" (read-keyset 'user-ks))
(coin.coinbase "user1" (read-keyset 'user-ks)   5000000.0)

(abc.create-account "user1" (read-keyset 'user-ks))
(abc.fund  "user1" 5000000.0)

(xyz.create-account "user1" (read-keyset 'user-ks))
(xyz.fund  "user1" 5000000.0)
