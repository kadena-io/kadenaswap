(define-keyset 'swap-ns-user)
(define-keyset 'swap-ns-admin)
(ns.write-registry (read-msg 'ns) (keyset-ref-guard 'swap-ns-admin) true)
(define-namespace
  (read-msg 'ns)
  (keyset-ref-guard 'swap-ns-user)
  (keyset-ref-guard 'swap-ns-admin)
)
