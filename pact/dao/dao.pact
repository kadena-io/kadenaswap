(enforce-pact-version "3.7")

(namespace (read-msg 'dao-ns))

(module dao-v1 GOVERNANCE
  @doc
  "This is the start of the KDA DAO, not the end. \
  \It's not even something that should set a precedence. \
  \It intentionally doesn't incentivize participation. \
  \It doesn't even have a function to de-stake. \
  \Instead a community approved(ish) contract upgrade is needed to add functionality. \
  \The goal is to get KDA's guardians and ambassadors together, and make something better. \
  \Guardians are the stakers, with governance rights to approve and apply upgrades. \
  \Proposed upgrades have an APPROVAL_COOLDOWN days cooldown before they can be voted on and applied. \
  \Proposals have APPROVAL_TIMEOUT days to be applied, and need to be re-submitted thereafter. \
  \During this period, ambassadors can exercise their oversite: \
  \a majority vote by the ambassadors can freeze the DAO for FREEZE_TIMEOUT days. \
  \Guardians can add/deactivate/reactivate ambassadors. \
  \A DEACTIVATE_COOLDOWN avoids a red wedding. "

  (use util.guards)
  (defun btwn-incl:bool (start:time end:time ts:time)
    (enforce (>= end start) (format "bounding start {} must come before end {}" [start end]))
    (and (>= ts start) (<= ts end)))

  (defconst DAO_MODULE_NAME "dao-v1")
  (defconst DAO_ACCT_NAME "dao-v1") ; we'll change this
  (defun dao-acct-balance () (coin.get-balance DAO_ACCT_NAME))
  (defcap INTERNAL ()
    "mark some functions as internal only"
    true)

  ; Approved upgrades need to be at least this old to occur.
  ; This gives the ambassadors time to respond
  (defconst APPROVAL_COOLDOWN (days 1))

  ; Approvals are on the clock
  (defconst APPROVAL_TIMEOUT (days 3))

  ; Min time between deactivations
  (defconst DEACTIVATE_COOLDOWN (days 1))

  ; Max time a freeze can exist for
  (defconst FREEZE_TIMEOUT (days 7))

  ; Every guardian needs to escrow this much to
  (defconst GUARDIAN_KDA_REQUIRED 500000.0)

  ; ----
  ; DAO State
  (defconst DAO_STATE_KEY "state")
  (defschema dao-state
    guardian-count:integer
    ambassador-count:integer
    dao-frozen-until:time
    last-ambassador-deactivation:time
    proposed-upgrade-hash:string
    proposed-upgrade-time:time)
  (deftable state:{dao-state})
  (defun init-state:string ()
    (let ((default-time (add-time (chain-time) (days -7))))
      ; insert fails if key exists, so this only runs once
      (insert state DAO_STATE_KEY
          {'guardian-count:0
          ,'ambassador-count:0
          ,'dao-frozen-until:default-time
          ,'last-ambassador-deactivation:default-time
          ,'proposed-upgrade-hash:""
          ,'proposed-upgrade-time:default-time})))
  (defun view-state:object{dao-state} ()
    (read state DAO_STATE_KEY))

  (defun adjust-ambassador-count:bool (adjustment:integer)
    (require-capability (INTERNAL))
    (enforce (= (abs adjustment) 1) "Adjustment is 1 at a time")
    (let* ((prev-state (read state DAO_STATE_KEY))
           (prev-cnt (at 'ambassador-count prev-state))
           (x (+ {'ambassador-count:(+ prev-cnt adjustment)} prev-state)))
      (write state DAO_STATE_KEY x))
    true)
  (defun adjust-guardian-count:bool (adjustment:integer)
    (require-capability (INTERNAL))
    (enforce (= (abs adjustment) 1) "Adjustment is 1 at a time")
    (with-read state DAO_STATE_KEY {'guardian-count:=cnt}
      (update state DAO_STATE_KEY
        {'guardian-count: (+ adjustment cnt)}))
    true)

  (defun is-dao-frozen:bool ()
    ; check the time of tx vs state.frozenuntil... but I think this can be done better via a guard
    (with-read state DAO_STATE_KEY {"dao-frozen-until":=frz-time}
        (enforce (> (chain-time) frz-time) "DAO is Frozen"))
    false)

  ; ----
  ; Guardians are the dao-v1's actors, the entites that will actually do things like run the bridge and upgrade the dao
  (defschema guardian
      k:string
      guard:guard
      committed-kda:decimal
      approved-hash:string
      approved-date:time)
  (deftable guardians:{guardian})
  (defcap GUARDIAN (acct:string)
    (is-dao-frozen)
    (with-read guardians acct
      {"guard":=guard}
      (enforce-guard guard)))
  (defun view-guardians ()
      (map (read guardians) (keys guardians)))

  (defun register-guardian:bool (acct:string guard:guard)
    (with-capability (INTERNAL)
      (coin.transfer acct DAO_ACCT_NAME GUARDIAN_KDA_REQUIRED)
      (insert guardians acct
        { "k":acct
        , "guard":guard
        ,"committed-kda":GUARDIAN_KDA_REQUIRED
        ,"approved-hash":""
        ,"approved-date":(chain-time)})
      (adjust-guardian-count 1)))

  ; For now, registration is a one-way street
  (defun unregister-guardian (acct:string)
    (enforce false
      (format "{} needs to be upgraded to enable withdrawls" [DAO_MODULE_NAME]))
    (is-dao-frozen))

  (defun propose-dao-upgrade (acct:string hsh:string)
    (with-capability (GUARDIAN acct)
      ; should we limit the frequency of proposals?
      ; for now, we just trust guardians to not be griefers
      (update state DAO_STATE_KEY
        {"proposed-upgrade-hash":hsh
        ,"proposed-upgrade-time":(chain-time)}))
     (guardian-approve-hash acct hsh)
     true)

  (defun guardian-approve-hash:bool (acct:string hsh:string)
    (with-capability (GUARDIAN acct)
      (with-read state DAO_STATE_KEY {'proposed-upgrade-hash:=prp-hsh}
        (enforce (= prp-hsh hsh)
          (format "Upgrade hash mismatch: {} vs {}" [prp-hsh hsh])))
      (update guardians acct
        {"approved-hash":hsh
        ,"approved-date":(chain-time)}))
    true)

  ; ----
  ; Ambassadors can lock things up (table flip)
  (defschema ambassador
      k:string
      guard:guard
      active:bool
      voted-to-freeze:time)
  (deftable ambassadors:{ambassador})
  (defcap AMBASSADOR (acct:string)
    (with-read ambassadors acct
      {"guard":=guard, "active":=active}
      (enforce-guard guard)
      (enforce active "Ambassador acct is disabled")))
  (defun view-ambassadors ()
      (map (read ambassadors) (keys ambassadors)))

  (defun register-ambassador:bool (guardian:string acct:string guard:guard)
    (with-capability (GUARDIAN guardian)
      (insert ambassadors acct
              {"k":acct
              ,"guard":guard
              ,"active":true
              ,"voted-to-freeze":(time "2000-01-01T12:00:00Z")})
      (with-capability (INTERNAL)
        (adjust-ambassador-count 1)))
      true)
  (defun deactivate-ambassador:bool (guardian:string ambassador:string)
    (with-capability (GUARDIAN guardian)
      (let ((lst-deactivate (at 'last-ambassador-deactivation (read state DAO_STATE_KEY))))
        (enforce (> (chain-time) (add-time lst-deactivate DEACTIVATE_COOLDOWN)) "Deactivate Cooldown Failure")
        (update state DAO_STATE_KEY {"last-ambassador-deactivation":(chain-time)})
        (update ambassadors ambassador {"active":false}))
      (with-capability (INTERNAL)
        (adjust-ambassador-count -1)))
      true)
  (defun reactivate-ambassador:bool (guardian:string ambassador:string)
    (with-capability (GUARDIAN guardian)
      (update ambassadors ambassador {"active":true})
      (with-capability (INTERNAL)
        (adjust-ambassador-count 1)))
      true)

  ; ----
  ; Freeze the DAO
  (defun vote-to-freeze:bool (ambassador:string)
    (with-capability (AMBASSADOR ambassador)
      (update ambassadors ambassador {"voted-to-freeze":(chain-time)}))
    true)

  (defun freeze:string (ambassador:string)
    (with-capability (AMBASSADOR ambassador)
    (let* ((live-ambs
              (map (at "voted-to-freeze")
              (select ambassadors ["voted-to-freeze"] (where 'active (= true)))))
           (ambs-cnt (length live-ambs))
           (ambs-voted-to-freeze (length (filter (btwn-incl (add-time (chain-time) (- APPROVAL_COOLDOWN)) (chain-time)) live-ambs)))
           (dao-frz-ts (add-time (chain-time) FREEZE_TIMEOUT)))
      (enforce (> (* 2 ambs-voted-to-freeze) ambs-cnt)
        (format "Majority vote failed: {} of {}" [ambs-voted-to-freeze ambs-cnt]))
      (is-dao-frozen) ; so it can't get called twice
      ; is this correct?
      (update state DAO_STATE_KEY
        {"dao-frozen-until": dao-frz-ts})
      (format "DAO now frozen until {}" [dao-frz-ts]))))


  ; ----
  ; Upgrade the DAO
  (defcap GOVERNANCE ()
    (is-dao-frozen); if it's frozen, bail
    (check-hash-approval (tx-hash)))

  (defun check-hash-approval:bool (hsh:string)
    (with-read state DAO_STATE_KEY
      {'proposed-upgrade-time:=prp-time
      ,'guardian-count:=grd-cnt}
      (enforce (>= (chain-time) (add-time prp-time APPROVAL_COOLDOWN))
        (format "Proposal still in cooldown" [(add-time prp-time APPROVAL_COOLDOWN)]))
      (enforce (< (chain-time) (add-time prp-time APPROVAL_TIMEOUT))
        (format "Proposal has timed out" [(add-time prp-time APPROVAL_COOLDOWN)]))
      (let*
         ((approvals (map (at 'approved-date )
           (select guardians (where 'approved-hash (= hsh)))))
         (valid-aprv-start (add-time (chain-time) (- APPROVAL_TIMEOUT)))
         (valid-approvals (length (filter (<= valid-aprv-start) approvals))))
        (enforce (> (* 2 valid-approvals) grd-cnt)
          (format "Upgrade not approved, {} of {} for {}"
            [valid-approvals grd-cnt hsh])))))

    ; ----
    ; Initialize the DAO
    (defun init:string ()
      (init-state)
      (coin.create-account
        DAO_ACCT_NAME
        (create-module-guard DAO_ACCT_NAME)))

)

(create-table state)
(create-table ambassadors)
(create-table guardians)
(dao.dao-v1.init)
