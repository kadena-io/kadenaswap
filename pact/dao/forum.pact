(enforce-pact-version "4.0")

(namespace (read-msg 'dao-ns))

(module forum GOVERNANCE

  (defconst FORUM_MODULE_NAME "forum")
  (defcap FORUM-INTERNAL ()
    "mark some functions as internal only"
    true)

  (defcap GOVERNANCE ()
    (with-read state STATE_KEY {'mjolnir-guard:=mjolnir}
      (enforce (enforce-guard mjolnir) "You have failed to lift mjolnir, you are not worthy")))

  (defun create-forum-gov-guard:guard ()
  @doc "primarily for namespace usage"
    (create-module-guard FORUM_MODULE_NAME))

  (use util.guards)
  (use dao.init [is-guardian is-ambassador])

  ; ----
  ; Forum State
  (defconst STATE_KEY "state")
  (defschema state-obj
    next-forum-uuid:integer
    mjolnir-guard:guard)
  (deftable state:{state-obj})
  (deftable forum-state)
  (defun view-state ()
    (read state STATE_KEY))
  (defun init-state:bool (mjolnir-guard:guard)
    (insert state STATE_KEY
        {'next-forum-uuid:0
        ,'mjolnir-guard:mjolnir-guard})
    true)
  (defcap MJOLNIR ()
    (with-read state STATE_KEY {'mjolnir-guard:=mjolnir}
      (enforce (enforce-guard mjolnir)
        "You have failed to lift mjolnir, you are not worthy")))

  (defun rotate-mjolnir (guard:guard)
    (with-capability (MJOLNIR)
      (update state STATE_KEY {"mjolnir-guard":guard})))

  (defun forum-uuid:string ()
    (require-capability (FORUM-INTERNAL))
    (with-read state STATE_KEY {'next-forum-uuid:=i}
      (update state STATE_KEY {'next-forum-uuid: (+ 1 i)})
      (format "{}" [i])))

  ; ----
  ; members
  (defschema member-obj
    name:string
    guard:guard
    moderator:bool
    disabled:bool)
  (deftable members:{member-obj})
  (defun view-members ()
    (map (read members) (keys members)))
  (defcap MEMBER:bool (member:string)
    (with-read members member {"guard":=g ,"disabled":=disabled}
      (enforce (not disabled) (format "Member '{}' is disabled" [member]))
      (enforce (enforce-guard g) "Keyset Failure"))
    true)
  (defcap MODERATOR (member:string)
    (with-read members member
      {"guard":=g ,"disabled":=disabled, "moderator":= moderator}
      (enforce (not disabled) (format "Member '{}' is disabled" [member]))
      (enforce moderator (format "Member '{}' is not a moderator" [member]))
      (enforce (enforce-guard g) "Keyset Failure"))
    true)

  (defun become-moderator (guardian:string moderator-guard:guard)
    (is-guardian guardian)
    (insert members guardian
      {'name:guardian
      ,'guard:moderator-guard
      ,'moderator:true
      ,'disabled:false}))

  (defun become-member (ambassdor:string member-guard:guard)
    (is-ambassador ambassdor)
    (insert members ambassdor
      {'name:ambassdor
      ,'guard:member-guard
      ,'moderator:false
      ,'disabled:false}))

  (defun disable-member (moderator:string member:string)
    (log-mod-action moderator (format "Disabled: {}" [member]))
    (with-read members member
      {"disabled":=disabled, "moderator":=is-mod}
      (enforce (not is-mod) "Moderators cannot moderate other mods")
      (update members member {"disabled":true})))

  (defun enable-member (moderator:string member:string)
    (log-mod-action moderator (format "Enabled: {}" [member]))
    (with-read members member
      {"disabled":=disabled, "moderator":=is-mod}
      (enforce (not is-mod) "Moderators cannot moderate other mods")
      (update members member {"disabled":false})))

  (defun rotate-member (member:string guard:guard)
    (with-capability (MEMBER member)
      (update members member {"guard":guard})))

  (defun rotate-moderator (moderator:string guard:guard)
    (with-capability (MODERATOR moderator)
      (update members moderator {"guard":guard})))

  (defun disable-moderator (moderator:string)
    (log-mjolnir-action (format "Disabled: {}" [moderator]))
    (update members moderator {"disabled":true}))

  (defun enable-moderator (moderator:string)
    (log-mjolnir-action (format "enabled: {}" [moderator]))
    (update members moderator {"disabled":false}))

  (defun mjolnir-write-member (member:string member-guard:guard is-moderator:bool)
    "use this to directly mutate members"
    (log-mjolnir-action (format "Creating new member: {}" [member]))
    (write members member
      {'name:member
      ,'guard:member-guard
      ,'moderator:is-moderator
      ,'disabled:false}))

  ; ----
  ; suggestion functionality

  (defconst MAXIMUM_TOPIC_HEADLINE_LENGTH 256)
  (defconst MAXIMUM_TOPIC_BODY_LENGTH 5000)
  (defconst MAXIMUM_COMMENT_BODY_LENGTH 2500)

  (defun validate-markdown (subj:string txt:string max:integer)
    (let ((txt-length (length txt)))
      (enforce
        (>= txt-length 3)
        (format
          "{} does not conform to the min length of '{}': {}..."
          [subj, 3, (take 30 txt)]))
      (enforce
        (<= txt-length max)
        (format
          "{} does not conform to the max length of '{}': {}..."
          [subj, max, (take 30 txt)]))))

  (defschema modlog
    author:string
    timestamp:time
    action:string)
  (deftable modlogs:{modlog})
  (defun view-modlogs ()
    (map (read modlogs) (keys modlogs)))
  (defun log-mod-action (author:string action:string)
    (with-capability (MODERATOR author)
      (with-capability (FORUM-INTERNAL)
        (let ((index (forum-uuid)))
          (insert modlogs index
            {'author: author
            ,'timestamp:(chain-time)
            ,'action: action})))))
  (defun log-mjolnir-action (action:string)
    (with-capability (MJOLNIR)
      (with-capability (FORUM-INTERNAL)
        (let ((index (forum-uuid)))
          (insert modlogs index
            {'author: "mjolnir"
            ,'timestamp:(chain-time)
            ,'action: action})))))

  (defschema comment
    index:string
    parent-index:string
    topic-index:string
    author:string
    timestamp:time
    modified:bool
    deleted:bool
    locked:bool
    body:string
    upvotes:[string]
    downvotes:[string]
    child-indexs:[string])
  (deftable comments:{comment})
  (defun view-comments ()
    (map (read comments) (keys comments)))
  (defun view-comment (comment-index:string)
    (with-read comments comment-index
        { 'index := index
        , 'parent-index := parent-index
        , 'author := author
        , 'timestamp := timestamp
        , 'modified := modified
        , 'deleted := deleted
        , 'locked := locked
        , 'body := body
        , 'upvotes := upvotes
        , 'downvotes := downvotes
        , 'child-indexs := child-indexs }
      { 'index : index
      , 'parent-index : parent-index
      , 'author : author
      , 'timestamp : timestamp
      , 'modified : modified
      , 'deleted : deleted
      , 'locked : locked
      , 'body : body
      , 'upvotes : upvotes
      , 'downvotes : downvotes
      , 'child-indexs : child-indexs}))

  (defschema topic
    index:string
    headline:string
    author:string
    timestamp:time
    modified:bool
    body:string
    deleted:bool
    locked:bool
    upvotes:[string]
    downvotes:[string]
    comment-indexs:[string]
    all-comments:[string])
  (deftable topics:{topic})
  (defun view-topic (topic-index)
    (with-read topics topic-index
        { 'index := index
        , 'headline := headline
        , 'author := author
        , 'timestamp := timestamp
        , 'modified := modified
        , 'body := body
        , 'deleted := deleted
        , 'locked := locked
        , 'upvotes := upvotes
        , 'downvotes := downvotes
        , 'comment-indexs := comment-indexs
        , 'all-comments := all-comments}
      { 'index : index
      , 'headline : headline
      , 'author : author
      , 'timestamp : timestamp
      , 'modified : modified
      , 'body : body
      , 'deleted : deleted
      , 'locked : locked
      , 'upvotes : upvotes
      , 'downvotes : downvotes
      , 'comment-indexs : comment-indexs
      , 'all-comments : all-comments }))
  (defun undeleted-topic-keys:[string] ()
    (sort
      (map (at 'index)
      (select topics [ 'index ] (where 'deleted (= false))))))
  (defun view-topics ()
    (map (view-topic) (undeleted-topic-keys)))
  (defun view-deleted-topics ()
    (select topics [ 'index 'headline ] (where 'deleted (= true))))
  (defun view-topic-raw (topic-index:string)
    (read topics topic-index))

  (defun post-topic:string (headline:string author:string body:string)
    (with-capability (MEMBER author)
      (validate-markdown "topic headline" headline MAXIMUM_TOPIC_HEADLINE_LENGTH)
      (validate-markdown "topic body" body MAXIMUM_TOPIC_BODY_LENGTH)
      (with-capability (FORUM-INTERNAL)
        (let ((index (forum-uuid)))
          (insert topics index
             { 'index : index
             , 'headline : headline
             , 'author : author
             , 'timestamp : (chain-time)
             , 'modified : false
             , 'body : body
             , 'deleted : false
             , 'locked : false
             , 'upvotes : []
             , 'downvotes : []
             , 'comment-indexs: []
             , 'all-comments: []})
          index))))

  (defun modify-topic:bool (index:string headline:string body:string)
    (with-read topics index
        { 'author := author
        , 'deleted := deleted
        , 'locked := locked }
      (enforce (and (not deleted) (not locked)) "Deleted/Locked posts can't be modified")
      (validate-markdown "topic headline" headline MAXIMUM_TOPIC_HEADLINE_LENGTH)
      (validate-markdown "topic body" body MAXIMUM_TOPIC_BODY_LENGTH)
      (with-capability (MEMBER author)
        (update topics index
           { 'headline : headline
           , 'timestamp : (chain-time)
           , 'modified : true
           , 'body : body })))
    true)

  (defun delete-topic:bool (moderator:string topic-index:string)
    (log-mod-action moderator (format "Deleting topic {}" [topic-index]))
    (update topics topic-index {'deleted:true})
    true)

  (defun undelete-topic:bool (moderator:string topic-index:string)
    (log-mod-action moderator (format "Undeleting topic {}" [topic-index]))
    (update topics topic-index {'deleted:false})
    true)

  (defun lock-comment (comment-index:string)
    (require-capability (FORUM-INTERNAL))
    (update comments comment-index {'locked:true})
    true)
  (defun unlock-comment (comment-index:string)
    (require-capability (FORUM-INTERNAL))
    (update comments comment-index {'locked:false})
    true)

  (defun lock-topic:bool (moderator:string topic-index:string)
    (log-mod-action moderator (format "Locking topic {}" [topic-index]))
      (update topics topic-index {'locked : true})
      (with-capability (FORUM-INTERNAL)
        (with-read topics topic-index {"all-comments":=all-comments}
          (map (lock-comment) all-comments)))
    true)

  (defun unlock-topic:bool (moderator:string topic-index:string)
    (log-mod-action moderator (format "Unlocking topic {}" [topic-index]))
      (with-read topics topic-index {'comment-indexs := comment-indexs}
        (update topics topic-index {'locked : false})
        (with-capability (FORUM-INTERNAL)
          (map (unlock-comment) comment-indexs)))
    true)

  (defun post-topic-comment:string (author:string body:string topic-index:string)
    (with-capability (MEMBER author)
      (validate-markdown "comment body" body MAXIMUM_COMMENT_BODY_LENGTH)
      (with-capability (FORUM-INTERNAL)
        (let ((index (forum-uuid)))
          (with-read topics topic-index
              {'comment-indexs := comment-indexs
              ,'all-comments := all-comments}
            (update topics topic-index
              {'comment-indexs: (+ comment-indexs [index])
              ,'all-comments: (+ all-comments [index])}))
          (insert comments index
             { 'index : index
             , 'parent-index : topic-index
             , 'topic-index : topic-index
             , 'author : author
             , 'timestamp : (chain-time)
             , 'modified : false
             , 'locked : false
             , 'body : body
             , 'deleted : false
             , 'upvotes : []
             , 'downvotes : []
             , 'child-indexs : [] })
          index))))

  (defun post-comment-comment:string (author:string body:string parent-index:string)
    (with-capability (MEMBER author)
      (validate-markdown "comment body" body MAXIMUM_COMMENT_BODY_LENGTH)
      (with-capability (FORUM-INTERNAL)
        (let ((index (forum-uuid)))
          (with-read comments parent-index
              {'child-indexs := child-indexs
              ,"topic-index" := topic-index}
            (with-read topics topic-index {'all-comments := all-comments}
            (update topics topic-index
              {'all-comments: (+ all-comments [index])})
            (update comments parent-index
              {'child-indexs: (+ child-indexs [index])})
            (insert comments index
               { 'index : index
               , 'parent-index : parent-index
               , 'topic-index : topic-index
               , 'author : author
               , 'timestamp : (chain-time)
               , 'modified : false
               , 'locked : false
               , 'body : body
               , 'deleted : false
               , 'upvotes : []
               , 'downvotes : []
               , 'child-indexs : [] })))
            index))))

  (defun modify-comment:bool (index:string body:string)
    (with-read comments index
        { 'author := author
        , 'deleted := deleted
        , 'locked := locked }
      (validate-markdown "comment body" body MAXIMUM_COMMENT_BODY_LENGTH)
      (enforce (and (not deleted) (not locked)) "Deleted/Locked comments can't be modified")
      (with-capability (MEMBER author)
        (update comments index
           { 'timestamp : (chain-time)
           , 'modified : true
           , 'body : body })))
    true)

  (defun delete-topic-comment:bool (moderator:string comment-index:string)
    (with-read comments comment-index {'parent-index := topic-index}
      (log-mod-action moderator (format "Deleted comment {} from topic {}" [comment-index topic-index]))
      (update comments comment-index {'deleted : true})
      (with-read topics topic-index
          {'all-comments := all-comments, 'comment-indexs := comment-indexs}
        (update topics topic-index
          {'comment-indexs : (filter (!= comment-index) comment-indexs)
          ,'all-comments : (filter (!= comment-index) all-comments)}
          )))
    true)

  (defun delete-comment-comment:bool (moderator:string comment-index:string)
    (with-read comments comment-index {'parent-index := parent-index}
      (log-mod-action moderator (format "Deleted comment {} from comment {}" [comment-index parent-index]))
      (update comments comment-index {'deleted : true})
      (with-read comments parent-index
          {'topic-index := topic-index, 'child-indexs := child-indexs}
        (with-read topics topic-index {'all-comments := all-comments}
          (update topics topic-index
            {'all-comments : (filter (!= comment-index) all-comments)})
          (update comments parent-index
            {'child-indexs : (filter (!= comment-index) child-indexs)}))))
    true)

  (defun undelete-topic-comment:bool (moderator:string comment-index:string)
    (with-read comments comment-index {'parent-index := topic-index}
      (log-mod-action moderator (format "UnDeleted comment {} from topic {}" [comment-index topic-index]))
      (update comments comment-index {'deleted : false})
      (with-read topics topic-index
          {'all-comments := all-comments, 'comment-indexs := comment-indexs}
        (update topics topic-index
          {'comment-indexs : (+ comment-indexs [comment-index])
          ,'all-comments : (+ all-comments [comment-index])}
          )))
    true)

  (defun undelete-comment-comment:bool (moderator:string comment-index:string)
    (with-read comments comment-index {'parent-index := parent-index}
      (log-mod-action moderator (format "Deleted comment {} from comment {}" [comment-index parent-index]))
      (update comments comment-index {'deleted : false})
      (with-read comments parent-index
          {'topic-index := topic-index, 'child-indexs := child-indexs}
        (with-read topics topic-index {'all-comments := all-comments}
          (update topics topic-index
            {'all-comments : (+ all-comments [comment-index])})
          (update comments parent-index
            {'child-indexs : (+ child-indexs [comment-index])}))))
    true)

  (defun vote-on-topic:bool (account:string topic-index:string vote:string)
    (with-capability (MEMBER account)
      (with-read topics topic-index {'upvotes:=upvotes, 'downvotes:=downvotes}
        (let ((vu (filter (!= account) upvotes))
              (vd (filter (!= account) downvotes)))
          (if (= vote "upvote")
              (update topics topic-index {'upvotes:(+ vu [account]), 'downvotes:vd})
          (if (= vote "downvote")
              (update topics topic-index {'upvotes:vu, 'downvotes:(+ vd [account])})
          (if (= vote "remove")
              (update topics topic-index {'upvotes:vu, 'downvotes:vd})
          ;else
              (format "for typecheck {}" [(enforce false "votes must be one of: ['upvote' 'downvote' 'remove']")])
    ))))))
    true)

  (defun vote-on-comment:bool (account:string comment-index:string vote:string)
    (with-capability (MEMBER account)
      (with-read comments comment-index {'upvotes:=upvotes, 'downvotes:=downvotes}
        (let ((vu (filter (!= account) upvotes))
              (vd (filter (!= account) downvotes)))
          (if (= vote "upvote")
              (update comments comment-index {'upvotes:(+ vu [account]), 'downvotes:vd})
          (if (= vote "downvote")
              (update comments comment-index {'upvotes:vu, 'downvotes:(+ vd [account])})
          (if (= vote "remove")
              (update comments comment-index {'upvotes:vu, 'downvotes:vd})
          ;else
              (format "for typecheck {}" [(enforce false "votes must be one of: ['upvote' 'downvote' 'remove']")])
    ))))))
    true)
  ;
  (defun init (mjolnir-guard:guard)
    (init-state mjolnir-guard))

  (defun state-table-migration ()
    (insert state STATE_KEY (read forum-state STATE_KEY)))
)


(create-table comments)
(create-table topics)
(create-table modlogs)
(create-table state)
(create-table members)
(forum.init (read-keyset "mjolnir"))
