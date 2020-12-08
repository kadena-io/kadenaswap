(define-keyset 'swap-ns-user)
(define-keyset 'swap-ns-admin)
(ns.write-registry 'swap (keyset-ref-guard 'swap-ns-admin) true)
(define-namespace
  "swap"
  (keyset-ref-guard 'swap-ns-user)
  (keyset-ref-guard 'swap-ns-admin)
)
