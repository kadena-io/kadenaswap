(enforce-pact-version "3.7")

(namespace (read-msg 'dao-ns))

;; dao.toETH
(module dao-v1 GOVERNANCE
  @doc "This is the start of the DAO, not the end. It intentionally doesn't incentivize participation. \
       \The goal is to get KDA's guardians and ambassadors together, and make something better."

  (defconst MODULE_NAME "dao-v1")
  (defconst MODULE_ACCT_NAME "dao-v1") ; we'll change this
  (defcap INTERNAL () true)

  ; Approved upgrades need to be at least this old to occur.
  ; This gives the ambassadors time to respond
  (defconst APPROVAL_COOLDOWN (days 1))

  ; Approvals are on the clock
  (defconst APPROVAL_TIMEOUT (days 3))

  ; Min time between deactivations
  (defconst DEACTIVATE_COOLDOWN (days 1))

  ; Max time a freeze lasts for
  (defconst FREEZE_TIMEOUT (days 7))

  ; Every guardian needs to escrow this much to
  (defconst GUARDIAN_KDA_REQUIRED 500000)

  (defschema proposed-upgrade
    proposed-upgrade-hash:string
    proposed-upgrade-time:string)

  (defschema dao-state
    guardian-count:integer
    ambassador-count:integer
    dao-frozen-until-date:time
    last-ambassador-deactivation:time
    proposed-upgrade:object{proposed-upgrade}
    )

  (defun adjust-count:bool (key:string adjustment:integer)
    (require-capability INTERNAL)
    (enforce (<= (abs adjustment) 1) "Adjustment is 1 at a time")
    (let* ((prev-state (read state DAO_STATE_KEY))
           (prev-cnt (at key prev-state))
           (new-state (+ {key:(+ prev-cnt adjustment)} prev-state)))
         (write state DAO_STATE_KEY new-state)))

  (defconst DAO_STATE_KEY "state")

  (deftable state:table{dao-state})

  (defun now:time () (at "block-time" (chain-data)))

  (defun is-dao-frozen:bool ()
    ; check the time of tx vs state.frozenuntil... but I think this can be done better via a guard
    (with-read state DAO_STATE_KEY {"dao-frozen-until":=frz-time}
        (enforce (> (now) frz-time)) "DAO is Frozen"))

  ; ----
  ; Guardians are the dao-v1's actors, the entites that will actually do things like run the bridge and upgrade the dao
  (defschema guadian
      guard:guard
      committed-kda:integer
      approved-hash:string
      approved-date:time)

  (deftable guardians:object{guardian})

  (defun register-guardian:bool (acct:string guard:guard)
    (coin.transfer acct MODULE_ACCT_NAME GUARDIAN_KDA_REQUIRED)
    (insert guardians acct
      {"guard":guard
      ,"committed-kda":GUARDIAN_KDA_REQUIRED
      ,"approved-hash":""
      ,"approved-date":(now)})
    (adjust-count "guardian-count" 1))

  (defcap GUARDIAN (acct:string)
    (with-read guardians acct
      {"guard":=guard}
      (enforce-guard guard)))

  ; ----
  ; Ambassadors can lock things up (table flip)
  (defschema ambassador
      guard:guard
      active:bool
      voted-to-freeze:time)
  (deftable ambassadors:table{ambassador})

  (defun register-ambassador:bool (acct:string guard:guard)
    (require-capability (GUARDIAN guardian))
    (insert ambassadors acct
            {"guard":guard
            ,"active":true
            ,"voted-to-freeze":(time "2000-01-01T12:00:00Z")})
    (adjust-count "ambassador-cnt" 1))
  (defun deactivate-ambassador:bool (guardian:string ambassador:string)
    (require-capability (GUARDIAN guardian))
    (let ((dao-state (read state DAO_STATE_KEY)))
      (enforce (> (add-time (now) DEACTIVATE_COOLDOWN) (at 'last-ambassador-deactivation dao-state)) "Deactivate Cooldown Failure")
      (update state DAO_STATE_KEY (+ {"last-ambassador-deactivation":(now)} dao-state))
      (update ambassadors ambassador {"active":false}))
    (adjust-count "ambassador-cnt" (- 1)))
  (defun reactivate-ambassador:bool (guardian:string ambassador:string)
    (require-capability (GUARDIAN guardian))
    (update ambassadors ambassador {"active":true})
    (adjust-count "ambassador-cnt" 1))

  (defcap AMBASSADOR (acct:string)
    (with-read ambassadors acct
      {"guard":=guard, "active":=active}
      (enforce-guard guard)
      (enforce active "Ambassador acct is disabled")))

  ; ----
  ; Freeze the DAO
  (defun vote-to-freeze:bool (ambassador:string)
    (require-capability (AMBASSADOR ambassador))
    (update ambassadors ambassador {"voted-to-freeze":(now)}))

  (defun freeze:bool (ambassador:string)
    (require-capability (AMBASSADOR ambassador))
    (let* ((live-ambs (select ambassadors ["voted-to-freeze"] (where 'active (= true))))
           (ambs-cnt (length live-ambs))
           (ambs-voted-to-freeze (length (filter (>= (add-time (now) (- APPROVAL_COOLDOWN))) live-ambs))))
      (enforce (> (* 2 ambs-voted-to-freeze) ambs-cnt) "Majority vote failed")
      (is-dao-frozen) ; so it can't get called twice
      ; is this correct?
      (write state DAO_STATE_KEY {"dao-frozen-until-date": (add-time (now) FREEZE_TIMEOUT)})))


  ; ----
  ; Upgrade teh DAO
  (defcap GOVERNANCE ()
    (is-dao-frozen); if it's frozen, bail
    (check-hash-approval) ; TODO
    )

)
