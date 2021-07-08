(define-keyset 'dao-ns-user)
(define-keyset 'dao-ns-admin)
(ns.write-registry (read-msg 'dao-ns) (keyset-ref-guard 'dao-ns-admin) true)
(define-namespace
  (read-msg 'dao-ns)
  (keyset-ref-guard 'dao-ns-user)
  (keyset-ref-guard 'dao-ns-admin)
)
