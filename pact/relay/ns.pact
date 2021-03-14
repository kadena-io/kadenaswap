(define-keyset 'relay-ns-user)
(define-keyset 'relay-ns-admin)
(ns.write-registry (read-msg 'ns) (keyset-ref-guard 'relay-ns-admin) true)
(define-namespace
  (read-msg 'ns)
  (keyset-ref-guard 'relay-ns-user)
  (keyset-ref-guard 'relay-ns-admin)
)
