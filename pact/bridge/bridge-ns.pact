(define-keyset 'bridge-ns-user)
(define-keyset 'bridge-ns-admin)
(ns.write-registry (read-msg 'bridge-ns) (keyset-ref-guard 'bridge-ns-admin) true)
(define-namespace
  (read-msg 'bridge-ns)
  (keyset-ref-guard 'bridge-ns-user)
  (keyset-ref-guard 'bridge-ns-admin)
)
