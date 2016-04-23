#lang racket
(require db)
(require racket/trace)
(require db/util/datetime)
;;(require (planet williams/describe/describe)) ; debug
(require "db.rkt")
(require "db-utils.rkt")
(require "arc-utils.rkt")
(require "publications.rkt")
(require "db-publication.rkt")

(provide
 db-save-user-no-delete-no-transaction ; used by the conversion script, for speed
 db-load-all-users
 db-load-user
 db-save-user
 db-get-users
 db-user-exists
 )

;; \todo move to db-utils; duplicated in publications
(define (pub-ids-list-vecs->jsexpr l)
  (map (lambda (id-vec) (vector-ref id-vec 0)) l))

(define query-get-users (virtual-statement "select id from users;"))

(define (db-get-users)
    (pub-ids-list-vecs->jsexpr (query-rows db-conn query-get-users)))

(define query-get-user-exists (virtual-statement "select count(1) from users where id = $1;"))

(define (db-user-exists id)
  (bool->arc-bool (not (eq? (query-value db-conn query-get-user-exists id) 0))))


(define (save-votes id arc-votes)
  (let ([votes (arc-list->list arc-votes)]
        [table "users_votes"])
    (if (and (list? votes) (not (empty? votes)))
        (begin
          (db-delete-from table id)
          (map (lambda (arc-vote)
                 (if (not (eq? arc-vote 'nil))
                     (begin
                       (let*
                           ([vote (arc-list->list arc-vote)]
                            [vote-id (first vote)]
                            [publication-id (second vote)]
                            [username (third vote)]
                            [domain (sqlnil (fourth vote))]
                            [up-atom (fifth vote)]
                            [up (equal? up-atom 'up)]
                            [query (string-append "insert into \"" table "\" (id, publication_id, vote_id, username, web_domain, up) values ($1, $2, $3, $4, $5, $6);")]
                            )
                         (query-exec db-conn query id publication-id vote-id username domain up)
                         )
                       )
                     void
                     ))
               votes))
        void)))

(define (save-suggested-tags id arc-suggested-tags)
  (let ([suggested-tags (arc-list->list arc-suggested-tags)]
        [table "users_suggested_tags"])
    (if (and (list? suggested-tags) (not (empty? suggested-tags)))
        (begin
          (db-delete-from table id)
          (map (lambda (arc-suggested-tag)
                 (if (not (eq? arc-suggested-tag 'nil))
                     (let* ([suggested-tag (arc-list->list arc-suggested-tag)]
                            [publication-id (first suggested-tag)]
                            [tag (second suggested-tag)]
                            [query (string-append "insert into \"" table "\" (id, publication_id, tag) values ($1, $2, $3);")])
                       (query-exec db-conn query id publication-id tag))
                     void))
                 suggested-tags))
          void)))

(define (save-submitted id lst) (save-otm-pair id (arc-list->list lst) "users_submitted" "publication_id"))
(define (save-saved id lst) (save-otm-pair id (arc-list->list lst) "users_saved" "publication_id"))
(define (save-sticky id lst) (save-otm-pair id (arc-list->list lst) "users_sticky" "publication_id"))
(define (save-hidden id lst) (save-otm-pair id (arc-list->list lst) "users_hidden" "publication_id"))
(define (save-mail id lst) (save-otm-pair id (arc-list->list lst) "users_mail" "publication_id"))
(define (save-drafts id lst) (save-otm-pair id (arc-list->list lst) "users_drafts" "publication_id"))
(define (save-shareds id lst) (save-otm-pair id (arc-list->list lst) "users_shareds" "publication_id"))
(define (save-cubbeds id lst) (save-otm-pair id (arc-list->list lst) "users_cubbeds" "publication_id"))
(define (save-badged id lst) (save-otm-pair id (arc-list->list lst) "users_badged" "publication_id"))
(define (save-badges id lst) (save-otm-pair id (arc-list->list lst) "users_badges" "publication_id"))
(define (save-ignoring id lst) (save-otm-pair id (arc-list->list lst) "users_ignoring" "ignoring_id"))
(define (save-muting id lst) (save-otm-pair id (arc-list->list lst) "users_muting" "muting_id"))
(define (save-hushing id lst) (save-otm-pair id (arc-list->list lst) "users_hushing" "hushing_id"))
(define (save-blocking id lst) (save-otm-pair id (arc-list->list lst) "users_blocking" "blocking_id"))
(define (save-ignoring-tag id lst) (save-otm-pair id (arc-list->list lst) "users_ignoring_tag" "tag"))
(define (save-ignoring-dom id lst) (save-otm-pair id (arc-list->list lst) "users_ignoring_dom" "dom"))
(define (save-ignored-by id lst) (save-otm-pair id (arc-list->list lst) "users_ignored_by" "by_id"))
(define (save-muted-by id lst) (save-otm-pair id (arc-list->list lst) "users_muted_by" "by_id"))
(define (save-hushed-by id lst) (save-otm-pair id (arc-list->list lst) "users_hushed_by" "by_id"))
(define (save-blocked-by id lst) (save-otm-pair id (arc-list->list lst) "users_blocked_by" "by_id"))
(define (save-followed id lst) (save-otm-pair id (arc-list->list lst) "users_followed" "followed_id"))
(define (save-follower id lst) (save-otm-pair id (arc-list->list lst) "users_follower" "follower_id"))
(define (save-personal-tags id lst) (save-otm-pair id (arc-list->list lst) "users_personal_tags" "tag"))
(define (save-followed-tags id lst) (save-otm-pair id (arc-list->list lst) "users_followed_tags" "tag"))
(define (save-followed-dom id lst) (save-otm-pair id (arc-list->list lst) "users_followed_dom" "dom"))
(define (save-notified id lst) (save-otm-pair id (arc-list->list lst) "users_notified" "notified_id"))

(define (safe-numerator n)
  (if (rational? n)
      (numerator n)
      n))

(define (safe-denominator n)
  (if (rational? n)
      (denominator n)
      n))

(define (safe-sqldate n)
  (if (real? n)
      (srfi-date->sql-timestamp (seconds->date n))
      n))

(define (ymd->date year month day)
  (date 0 0 0 day month year 0 0 #f 0))

(define (safe-ymd->sqldate l)
  (if (and (list? l) (> (length l) 2) (number? (first l)) (number? (second l)) (number? (third l)))
      (srfi-date->sql-timestamp (date* 0 0 0 (third l) (second l) (first l) 0 0 #f 0 0 "UTC"))
      l))

(define insert-user-query (virtual-statement "insert into users (id, joined, inactive, clout, word_count, average_com_numerator, average_com_denominator, ignore_newbies, global_ignored, new_tabs, publication_tabs, reply_alerts, pm_alerts, follower_alerts, shout_outs, badge_alerts, saved_notifications, feed_times, share_counts, show_global_filtered, follows_badges, embed_videos, bio, email, hubski_style, homepage, follower_count, posts_count, shareds_count, unread_notifications, last_com_time, com_clout_date, zen, spammer) values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24, $25, $26, $27, $28, $29, $30, $31, $32, $33, $34);"))

; MUST be called in a transaction. Becuase one-to-many (OTM) queries delete+insert, which MUST be atomic
(define (db-save-user-no-delete-no-transaction h)
  (let*
   (
  [tosql (lambda (arc-tag) (sqlnil (safe-car (val-if h arc-tag))))]
  [tosql-list (lambda (arc-tag) (sqlnil (val-if h arc-tag)))]
  [tosql-bool (lambda (arc-tag) (arc-bool->bool (safe-car (val-if h arc-tag))))]
  [if-sqlnull (lambda (v default) (if (eq? v sql-null) default v))]
  [id (tosql 'id)]
;;  [dbg (begin (write id) (newline))] ;; debug
  [joined (safe-sqldate (tosql 'joined))]
  [inactive (tosql-bool 'inactive)]
  [clout (if-sqlnull (tosql 'clout) 1)]
  [word-count (tosql-bool 'wordcount)]
  [average-com-numerator (safe-numerator (tosql 'avgcom))]
  [average-com-denominator (safe-denominator (tosql 'avgcom))]
  [ignore-newbies (tosql-bool 'ignorenewbies)]
  [global-ignored (tosql-bool 'global-ignored)]
  [new-tabs (tosql-bool 'new-tabs)]
  [publication-tabs (tosql-bool 'pub-tabs)]
  [reply-alerts (tosql-bool 'reply-alerts)]
  [pm-alerts (tosql-bool 'pm-alerts)]
  [follower-alerts (tosql-bool 'follower-alerts)]
  [shout-outs (tosql-bool 'shout-outs)]
  [badge-alerts (tosql-bool 'badge-alerts)]
  [saved-notifications (tosql-bool 'saved-notifications)]
  [feed-times (tosql-bool 'feed-times)]
  [share-counts (tosql-bool 'share-counts)]
  [show-global-filtered (tosql-bool 'show-global-filtered)]
  [follows-badges (tosql-bool 'follows-badges)]
  [embed-videos (tosql-bool 'embed-videos)]
  [bio (tosql 'bio)]
  [email (tosql-bool 'email)]
  [hubski-style (tosql 'hubski-style)]
  [homepage (tosql 'homepage)]
  [follower-count (safe-car (tosql 'followercount))]
  [posts-count (safe-car (tosql 'postscount))]
  [shareds-count (safe-car (tosql 'shareds-count))]
  [unread-notifications (tosql-bool 'unread-notifications)]
  [last-com-time (safe-sqldate (tosql 'lastcomtime))]
  [com-clout-date (safe-sqldate (safe-ymd->sqldate (tosql 'comcloutdate)))]
  [zen (tosql-bool 'zen)]
  [spammer (tosql-bool 'spammer)]
  [submitted (tosql-list 'submitted)]
  [saved (tosql-list 'saved)]
  [sticky (tosql-list 'sticky)]
  [hidden (tosql-list 'hidden)]
  [mail (tosql-list 'mail)]
  [drafts (tosql-list 'drafts)]
  [shareds (tosql-list 'shareds)]
  [cubbeds (tosql-list 'cubbeds)]
  [votes (tosql-list 'votes)]
  [suggested-tags (tosql-list 'suggtags)]
  [badged (tosql-list 'badged)]
  [badges (tosql-list 'badges)]
  [ignoring (tosql-list 'ignoring)]
  [muting (tosql-list 'muting)]
  [hushing (tosql-list 'hushing)]
  [blocking (tosql-list 'blocking)]
  [ignoring-tag (tosql-list 'ignoring-tag)]
  [ignoring-dom (tosql-list 'ignoring-dom)]
  [ignored-by (tosql-list 'ignored-by)]
  [muted-by (tosql-list 'muted-by)]
  [hushed-by (tosql-list 'hushed-by)]
  [blocked-by (tosql-list 'blocked-by)]
  [followed (tosql-list 'followed)]
  [follower (tosql-list 'follower)]
  [personal-tags (tosql-list 'personal-tags)]
  [followed-tags (tosql-list 'followed-tags)]
  [followed-dom (tosql-list 'followed-dom)]
  [notified (tosql-list 'notified)]
  )
 ;; (write id)
 ;; (newline)
 ;; (write followed)
 ;; (newline)
;; (write suggested-tags)
;; (newline)
  ;; \todo change to use upsert, when we update to postgresl 9.5
 (query-exec db-conn insert-user-query id joined inactive clout word-count average-com-numerator average-com-denominator ignore-newbies global-ignored new-tabs publication-tabs reply-alerts pm-alerts follower-alerts shout-outs badge-alerts saved-notifications feed-times share-counts show-global-filtered follows-badges embed-videos bio email hubski-style homepage follower-count posts-count shareds-count unread-notifications last-com-time com-clout-date zen spammer)
 (save-submitted id submitted)
 (save-saved id saved)
 (save-sticky id sticky)
 (save-hidden id hidden)
 (save-mail id mail)
 (save-drafts id drafts)
 (save-shareds id shareds)
 (save-cubbeds id cubbeds)
 (save-votes id votes)
 (save-suggested-tags id suggested-tags)
 (save-badged id badged)
 (save-badges id badges)
 (save-ignoring id ignoring)
 (save-muting id muting)
 (save-hushing id hushing)
 (save-blocking id blocking)
 (save-ignoring-tag id ignoring-tag)
 (save-ignoring-dom id ignoring-dom)
 (save-ignored-by id ignored-by)
 (save-muted-by id muted-by)
 (save-hushed-by id hushed-by)
 (save-blocked-by id blocked-by)
 (save-followed id followed)
 (save-follower id follower)
 (save-personal-tags id personal-tags)
 (save-followed-tags id followed-tags)
 (save-followed-dom id followed-dom)
 (save-notified id notified)
  (void)))

; \todo write a pipeline macro?
(define (jsexpr->profile-sexp-not-null j)
  (list-add-hash-member        j 'joined 'joined ; time
  (list-add-hash-member-bool   j 'inactive 'inactive
  (list-add-hash-member        j 'clout 'clout
  (list-add-hash-member-bool   j 'word_count 'wordcount
  (list-add-hash-member        j 'average_com_numerator 'avgcomnum ; \todo fix avgcom
  (list-add-hash-member        j 'average_com_denominator 'avgcomden
  (list-add-hash-member-bool   j 'ignore_newbies 'ignorenewbies
  (list-add-hash-member-bool   j 'global_ignored 'global-ignored
  (list-add-hash-member-bool   j 'new_tabs 'new-tabs
  (list-add-hash-member-bool   j 'publication_tabs 'pub-tabs
  (list-add-hash-member-bool   j 'reply_alerts 'reply-alerts
  (list-add-hash-member-bool   j 'pm_alerts 'pm-alerts
  (list-add-hash-member-bool   j 'follower_alerts 'follower-alerts
  (list-add-hash-member-bool   j 'shout_outs 'shout-outs
  (list-add-hash-member-bool   j 'badge_alerts 'badge-alerts
  (list-add-hash-member-bool   j 'saved_notifications 'saved-notifications
  (list-add-hash-member-bool   j 'feed_times 'feed-times
  (list-add-hash-member-bool   j 'share_counts 'share-counts
  (list-add-hash-member-bool   j 'show_global_filtered 'show-global-filtered
  (list-add-hash-member-bool   j 'follows_badges 'follows-badges
  (list-add-hash-member-bool   j 'embed_videos 'embed-videos
  (list-add-hash-member        j 'bio 'bio
  (list-add-hash-member-bool   j 'email 'email
  (list-add-hash-member        j 'hubski_style 'hubski-style
  (list-add-hash-member        j 'homepage 'homepage
  (list-add-hash-member        j 'follower_count 'followercount
  (list-add-hash-member        j 'posts_count 'postscount
  (list-add-hash-member        j 'shareds_count 'sharedscount
  (list-add-hash-member-bool   j 'unread_notifications 'unread-notifications
  (list-add-hash-member        j 'last_com_time 'lastcomtime ; time
  (list-add-hash-member        j 'com_clout_date 'comcloutdate ; time
  (list-add-hash-member-bool   j 'zen 'zen
  (list-add-hash-member-bool   j 'spammer 'spammer
  (list-add-hash-member-list   j 'submitted 'submitted
  (list-add-hash-member-list   j 'saved 'saved
  (list-add-hash-member-list   j 'sticky 'sticky
  (list-add-hash-member-list   j 'hidden 'hidden
  (list-add-hash-member-list   j 'mail 'mail
  (list-add-hash-member-list   j 'drafts 'drafts
  (list-add-hash-member-list   j 'shareds 'shareds
  (list-add-hash-member-list   j 'cubbeds 'cubbeds
  (list-add-hash-member-votes  j 'votes 'votes
  (list-add-hash-member-list   j 'suggested_tags 'suggtags
  (list-add-hash-member-list   j 'badged 'badged
  (list-add-hash-member-list   j 'badges 'badges
  (list-add-hash-member-list   j 'ignoring 'ignoring
  (list-add-hash-member-list   j 'muting 'muting
  (list-add-hash-member-list   j 'hushing 'hushing
  (list-add-hash-member-list   j 'blocking 'blocking
  (list-add-hash-member-list   j 'ignoring_tag 'ignoringtag
  (list-add-hash-member-list   j 'ignoring_dom 'ignoringdom
  (list-add-hash-member-list   j 'ignored_by 'ignoredby
  (list-add-hash-member-list   j 'muted_by 'mutedby
  (list-add-hash-member-list   j 'hushed_by 'hushedby
  (list-add-hash-member-list   j 'blocked_by 'blockedby
  (list-add-hash-member-list   j 'followed 'followed
  (list-add-hash-member-list   j 'follower 'follower
  (list-add-hash-member-list   j 'personal_tags 'personaltags
  (list-add-hash-member-list   j 'followed_tags 'followtags
  (list-add-hash-member-list   j 'followed_dom 'followdom
  (list-add-hash-member-list   j 'notified 'notified
  (list-add-hash-member-list   j 'submitted 'submitted
  '()
  )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

; we can't just use hash->list because the hash has member hashes which must be converted
(define (jsexpr->profile-sexp j)
  (if (equal? j 'null)
      'null
      (jsexpr->profile-sexp-not-null j)))

(define load-all-users-query (virtual-statement "select extract(epoch from joined), inactive, clout, word_count, average_com_numerator, average_com_denominator, ignore_newbies, global_ignored, new_tabs, publication_tabs, reply_alerts, pm_alerts, follower_alerts, shout_outs, badge_alerts, saved_notifications, feed_times, share_counts, show_global_filtered, follows_badges, embed_videos, bio, email, hubski_style, homepage, follower_count, posts_count, shareds_count, unread_notifications, extract(epoch from last_com_time), extract(epoch from com_clout_date), zen, spammer, id from users;")) ; debug limit 10

(define users-submitted-all-query (otm-all-query-val-ascending "users_submitted" "publication_id"))
(define users-saved-all-query (otm-all-query-val-ascending "users_saved" "publication_id"))
(define users-sticky-all-query (otm-all-query-val-ascending "users_sticky" "publication_id"))
(define users-hidden-all-query (otm-all-query-val-ascending "users_hidden" "publication_id"))
(define users-mail-all-query (otm-all-query-val-ascending "users_mail" "publication_id"))
(define users-drafts-all-query (otm-all-query-val-ascending "users_drafts" "publication_id"))
(define users-shareds-all-query (otm-all-query-val-ascending "users_shareds" "publication_id"))
(define users-cubbeds-all-query (otm-all-query-val-ascending "users_cubbeds" "publication_id"))
(define users-votes-all-query (virtual-statement "select vote_id, publication_id, username, web_domain, up, id from users_votes order by vote_id asc;"))
(define users-suggested-tags-all-query (virtual-statement "select publication_id, tag, id from users_suggested_tags order by publication_id asc;"))
(define users-badged-all-query (otm-all-query-val-ascending "users_badged" "publication_id"))
(define users-badges-all-query (otm-all-query-val-ascending "users_badges" "publication_id"))
(define users-ignoring-all-query (otm-all-query-val-ascending "users_ignoring" "ignoring_id"))
(define users-muting-all-query (otm-all-query-val-ascending "users_muting" "muting_id"))
(define users-hushing-all-query (otm-all-query-val-ascending "users_hushing" "hushing_id"))
(define users-blocking-all-query (otm-all-query-val-ascending "users_blocking" "blocking_id"))
(define users-ignoring-tag-all-query (otm-all-query-val-ascending "users_ignoring_tag" "tag"))
(define users-ignoring-dom-all-query (otm-all-query-val-ascending "users_ignoring_dom" "dom"))
(define users-ignored-by-all-query (otm-all-query-val-ascending "users_ignored_by" "by_id"))
(define users-muted-by-all-query (otm-all-query-val-ascending "users_muted_by" "by_id"))
(define users-hushed-by-all-query (otm-all-query-val-ascending "users_hushed_by" "by_id"))
(define users-blocked-by-all-query (otm-all-query-val-ascending "users_blocked_by" "by_id"))
(define users-followed-all-query (otm-all-query-val-ascending "users_followed" "followed_id"))
(define users-follower-all-query (otm-all-query-val-ascending "users_follower" "follower_id"))
(define users-personal-tags-all-query (otm-all-query-val-ascending "users_personal_tags" "tag"))
(define users-followed-tags-all-query (otm-all-query-val-ascending "users_followed_tags" "tag"))
(define users-followed-dom-all-query (otm-all-query-val-ascending "users_followed_dom" "dom"))
(define users-notified-all-query (otm-all-query-val-ascending "users_notified" "notified_id"))
; password hashes
; cookies
; emails

(define get-string (compose1
                    nil->null
                    safe-unlist
                    hash-ref-or-nil))
(define get-bool (compose1
                  arc-bool->bool
                  safe-unlist
                  hash-ref-or-nil))
(define get-list (compose1
                  nil->null
                  un-nil-list
                  hash-ref-or-nil))

(define (+all-votes h)
  (let* ([votes (query-rows db-conn users-votes-all-query)])
    (for-each
     (lambda (vote)
       (let*
           ([vote-id (vector-ref vote 0)]
            [publication-id (vector-ref vote 1)]
            [user (vector-ref vote 2)]
            [domain (from-sqlnil (vector-ref vote 3))]
            [up (if (vector-ref vote 4) 'up 'down)]
            [user-id (vector-ref vote 5)]
            [user-h (hash-ref! h user-id (make-hash))]
            [votes-l (hash-ref! user-h 'votes (list 'nil))])
         (hash-set! user-h 'votes (cons (list vote-id publication-id user domain up 'nil) votes-l))
         (hash-set! h user-id user-h) ; \todo determine if necessary
         ))
     votes)
    h))

(define (+all-suggested-tags h)
  (let* ([tags (query-rows db-conn users-suggested-tags-all-query)])
    (for-each
     (lambda (suggested-tag)
       (let*
           ([publication-id (vector-ref suggested-tag 0)]
            [tag (vector-ref suggested-tag 1)]
            [user-id (vector-ref suggested-tag 2)]
            [user-h (hash-ref! h user-id (make-hash))]
            [tags-l (hash-ref! user-h 'suggtags (list 'nil))])
         (hash-set! user-h 'suggtags (cons (list publication-id tag 'nil) tags-l))
         (hash-set! h user-id user-h) ; \todo determine if necessary
         ))
     tags)
    h))

(define (db-load-all-users)
  (let*
      ([+submitted (lambda (h) (+otm-all h users-submitted-all-query 'submitted))]
       [+saved (lambda (h) (+otm-all h users-saved-all-query 'saved))]
       [+sticky (lambda (h) (+otm-all h users-sticky-all-query 'sticky))]
       [+hidden (lambda (h) (+otm-all h users-hidden-all-query 'hidden))]
       [+mail (lambda (h) (+otm-all h users-mail-all-query 'mail))]
       [+drafts (lambda (h) (+otm-all h users-drafts-all-query 'drafts))]
       [+shareds (lambda (h) (+otm-all h users-shareds-all-query 'shareds))]
       [+cubbeds (lambda (h) (+otm-all h users-cubbeds-all-query 'cubbeds))]
       [+badgeds (lambda (h) (+otm-all h users-badged-all-query 'badged))]
       [+badgeses (lambda (h) (+otm-all h users-badges-all-query 'badges))]
       [+ignorings (lambda (h) (+otm-all h users-ignoring-all-query 'ignoring))]
       [+mutings (lambda (h) (+otm-all h users-muting-all-query 'muting))]
       [+hushings (lambda (h) (+otm-all h users-hushing-all-query 'hushing))]
       [+blockings (lambda (h) (+otm-all h users-blocking-all-query 'blocking))]
       [+ignoringtags (lambda (h) (+otm-all h users-ignoring-tag-all-query 'ignoringtag))]
       [+ignoringdoms (lambda (h) (+otm-all h users-ignoring-dom-all-query 'ignoringdom))]
       [+ignoredbys (lambda (h) (+otm-all h users-ignored-by-all-query 'ignoredby))]
       [+mutedbys (lambda (h) (+otm-all h users-muted-by-all-query 'mutedby))]
       [+hushedbys (lambda (h) (+otm-all h users-hushed-by-all-query 'hushedby))]
       [+blockedbys (lambda (h) (+otm-all h users-blocked-by-all-query 'blockedby))]
       [+followeds (lambda (h) (+otm-all h users-followed-all-query 'followed))]
       [+followers (lambda (h) (+otm-all h users-follower-all-query 'follower))]
       [+personaltagses (lambda (h) (+otm-all h users-personal-tags-all-query 'personaltags))]
       [+followtagses (lambda (h) (+otm-all h users-followed-tags-all-query 'followtags))]
       [+followdoms (lambda (h) (+otm-all h users-followed-dom-all-query 'followdom))]
       [+notifieds (lambda (h) (+otm-all h users-notified-all-query 'notified))]
       [+all-otms (compose1 +submitted +saved +sticky +hidden +mail +drafts +shareds +cubbeds +badgeds +badgeses +ignorings +mutings +hushings +blockings +ignoringtags +ignoringdoms +ignoredbys +mutedbys +hushedbys +blockedbys +followeds +followers +personaltagses +followtagses +followdoms +notifieds +all-votes +all-suggested-tags)]
       [user-vec-list (query-rows db-conn load-all-users-query)]
       [h (user-vec-list->users-hash user-vec-list)])
    (+all-otms h)))

;;        (+otms (+id (user-sexp->user-hash (jsexpr->user-sexp (user-vec->jsexpr maybe-user-vec))) id)))))

(define (otm-from-id-query table value-column) (virtual-statement (string-append "select \"" value-column "\" from \"" table "\" where \"id\" = $1;")))

(define load-user-query (virtual-statement "select extract(epoch from joined), inactive, clout, word_count, average_com_numerator, average_com_denominator, ignore_newbies, global_ignored, new_tabs, publication_tabs, reply_alerts, pm_alerts, follower_alerts, shout_outs, badge_alerts, saved_notifications, feed_times, share_counts, show_global_filtered, follows_badges, embed_videos, bio, email, hubski_style, homepage, follower_count, posts_count, shareds_count, unread_notifications, extract(epoch from last_com_time), extract(epoch from com_clout_date), zen, spammer from users where id = $1;"))

(define users-submitted-query (otm-from-id-query "users_submitted" "publication_id"))
(define users-saved-query (otm-from-id-query "users_saved" "publication_id"))
(define users-sticky-query (otm-from-id-query "users_sticky" "publication_id"))
(define users-hidden-query (otm-from-id-query "users_hidden" "publication_id"))
(define users-mail-query (otm-from-id-query "users_mail" "publication_id"))
(define users-drafts-query (otm-from-id-query "users_drafts" "publication_id"))
(define users-shareds-query (otm-from-id-query "users_shareds" "publication_id"))
(define users-cubbeds-query (otm-from-id-query "users_cubbeds" "publication_id"))
(define users-votes-query (virtual-statement "select vote_id, publication_id, username, web_domain, up from users_votes where id = $1;"))
(define users-suggested-tags-query (virtual-statement "select publication_id, tag from users_suggested_tags where id = $1;"))
(define users-badged-query (otm-from-id-query "users_badged" "publication_id"))
(define users-badges-query (otm-from-id-query "users_badges" "publication_id"))
(define users-ignoring-query (otm-from-id-query "users_ignoring" "ignoring_id"))
(define users-muting-query (otm-from-id-query "users_muting" "muting_id"))
(define users-hushing-query (otm-from-id-query "users_hushing" "hushing_id"))
(define users-blocking-query (otm-from-id-query "users_blocking" "blocking_id"))
(define users-ignoring-tag-query (otm-from-id-query "users_ignoring_tag" "tag"))
(define users-ignoring-dom-query (otm-from-id-query "users_ignoring_dom" "dom"))
(define users-ignored-by-query (otm-from-id-query "users_ignored_by" "by_id"))
(define users-muted-by-query (otm-from-id-query "users_muted_by" "by_id"))
(define users-hushed-by-query (otm-from-id-query "users_hushed_by" "by_id"))
(define users-blocked-by-query (otm-from-id-query "users_blocked_by" "by_id"))
(define users-followed-query (otm-from-id-query "users_followed" "followed_id"))
(define users-follower-query (otm-from-id-query "users_follower" "follower_id"))
(define users-personal-tags-query (otm-from-id-query "users_personal_tags" "tag"))
(define users-followed-tags-query (otm-from-id-query "users_followed_tags" "tag"))
(define users-followed-dom-query (otm-from-id-query "users_followed_dom" "dom"))
(define users-notified-query (otm-from-id-query "users_notified" "notified_id"))

(define (+otm h stmt json-key)
  (let* ([id (hash-ref h 'id)]
         [vals (query-rows db-conn stmt id)]
         [vals-list (map (lambda (val) (vector-ref val 0)) vals)])
    (hash-set! h json-key vals-list))
  h)

(define (+submitted h) (+otm h users-submitted-query 'submitted))
(define (+saved h) (+otm h users-saved-query 'saved))
(define (+sticky h) (+otm h users-sticky-query 'sticky))
(define (+hidden h) (+otm h users-hidden-query 'hidden))
(define (+mail h) (+otm h users-mail-query 'mail))
(define (+drafts h) (+otm h users-drafts-query 'drafts))
(define (+shareds h) (+otm h users-shareds-query 'shareds))
(define (+cubbeds h) (+otm h users-cubbeds-query 'cubbeds))
(define (+badgeds h) (+otm h users-badged-query 'badged))
(define (+badgeses h) (+otm h users-badges-query 'badges))
(define (+ignorings h) (+otm h users-ignoring-query 'ignoring))
(define (+mutings h) (+otm h users-muting-query 'muting))
(define (+hushings h) (+otm h users-hushing-query 'hushing))
(define (+blockings h) (+otm h users-blocking-query 'blocking))
(define (+ignoringtags h) (+otm h users-ignoring-tag-query 'ignoringtag))
(define (+ignoringdoms h) (+otm h users-ignoring-dom-query 'ignoringdom))
(define (+ignoredbys h) (+otm h users-ignored-by-query 'ignoredby))
(define (+mutedbys h) (+otm h users-muted-by-query 'mutedby))
(define (+hushedbys h) (+otm h users-hushed-by-query 'hushedby))
(define (+blockedbys h) (+otm h users-blocked-by-query 'blockedby))
(define (+followeds h) (+otm h users-followed-query 'followed))
(define (+followers h) (+otm h users-follower-query 'follower))
(define (+personaltagses h) (+otm h users-personal-tags-query 'personaltags))
(define (+followtagses h) (+otm h users-followed-tags-query 'followtags))
(define (+followdoms h) (+otm h users-followed-dom-query 'followdom))
(define (+notifieds h) (+otm h users-notified-query 'notified))
(define (+votes h)
  (let* ([user-id (hash-ref h 'id)]
         [votes (query-rows db-conn users-votes-query user-id)]
         [votes-list (map (lambda (val) (vector-ref val 0)) votes)])
    (for-each
     (lambda (vote)
       (let*
           ([vote-id (vector-ref vote 0)]
            [publication-id (vector-ref vote 1)]
            [user (vector-ref vote 2)]
            [domain (from-sqlnil (vector-ref vote 3))]
            [up (if (vector-ref vote 4) 'up 'down)]
            [votes-l (hash-ref! h 'votes (list 'nil))])
         (hash-set! h 'votes (cons (list vote-id publication-id user domain up 'nil) votes-l))
         ))
     votes))
  h)
(define (+suggested-tags h)
  (let* ([user-id (hash-ref h 'id)]
         [tags (query-rows db-conn users-suggested-tags-query user-id)]
         [tags-list (map (lambda (val) (vector-ref val 0)) tags)])
    (for-each
     (lambda (suggested-tag)
       (let*
           ([publication-id (vector-ref suggested-tag 0)]
            [tag (vector-ref suggested-tag 1)]
            [tags-l (hash-ref! h 'suggtags (list 'nil))])
         (hash-set! h 'suggtags (cons (list publication-id tag 'nil) tags-l))
         ))
     tags)
    h))
(define +otms (compose1 +submitted +saved +sticky +hidden +mail +drafts +shareds +cubbeds +badgeds +badgeses +ignorings +mutings +hushings +blockings +ignoringtags +ignoringdoms +ignoredbys +mutedbys +hushedbys +blockedbys +followeds +followers +personaltagses +followtagses +followdoms +notifieds +votes +suggested-tags))

(define (+id h id)
  (hash-set! h 'id id)
  h)

(define (db-load-user id)
  (let* ([maybe-user-vec (query-maybe-row db-conn load-user-query id)])
    (if (not maybe-user-vec)
        'null
        (+otms (+id (user-sexp->user-hash (jsexpr->user-sexp (user-vec->jsexpr maybe-user-vec))) id)))))


;; \returns a hash of pub sexprs, as expected by Arc (as opposed to jsexprs)
;; \todo (pub-vec-list->pubs-hash calls `make-hasheq`. Verify if `make-hash` or `make-hasheq` is correct/required by Arc.
;; \todo FIX MAGIC NUMBER 33
(define (user-vec-list->users-hash user-vec-list)
  (let ([h (make-hash)])
    (for-each (lambda (v)
                (let* ([id (vector-ref v 33)]
                       [user (user-vec->jsexpr v)])
                  (hash-set! user 'id id)
                  (hash-set! h id (user-sexp->user-hash (jsexpr->user-sexp user)))))
              user-vec-list)
    h))

;; \todo add otm tables
;; NOTE Specifically does not convert ID. If ID is included in the query, wrap this function.
;;      This is because queries are often "where id = ?" and already have the ID.
(define (user-vec->jsexpr v)
  (make-hash (list
   (cons 'joined                  (sql-null->null (vector-ref v 0)))
   (cons 'inactive                (sql-null->null (vector-ref v 1)))
   (cons 'clout                   (sql-null->null (vector-ref v 2)))
   (cons 'word_count              (sql-null->null (vector-ref v 3)))
   (cons 'average_com_numerator   (sql-null->null (vector-ref v 4)))
   (cons 'average_com_denominator (sql-null->null (vector-ref v 5)))
   (cons 'ignore_newbies          (sql-null->null (vector-ref v 6)))
   (cons 'global_ignored          (sql-null->null (vector-ref v 7)))
   (cons 'new_tabs                (sql-null->null (vector-ref v 8)))
   (cons 'publication_tabs        (sql-null->null (vector-ref v 9)))
   (cons 'reply_alerts            (sql-null->null (vector-ref v 10)))
   (cons 'pm_alerts               (sql-null->null (vector-ref v 11)))
   (cons 'follower_alerts         (sql-null->null (vector-ref v 12)))
   (cons 'shout_outs              (sql-null->null (vector-ref v 13)))
   (cons 'badge_alerts            (sql-null->null (vector-ref v 14)))
   (cons 'saved_notifications     (sql-null->null (vector-ref v 15)))
   (cons 'feed_times              (sql-null->null (vector-ref v 16)))
   (cons 'share_counts            (sql-null->null (vector-ref v 17)))
   (cons 'show_global_filtered    (sql-null->null (vector-ref v 18)))
   (cons 'follows_badges          (sql-null->null (vector-ref v 19)))
   (cons 'embed_videos            (sql-null->null (vector-ref v 20)))
   (cons 'bio                     (sql-null->null (vector-ref v 21)))
   (cons 'email                   (sql-null->null (vector-ref v 22)))
   (cons 'hubski_style            (sql-null->null (vector-ref v 23)))
   (cons 'homepage                (sql-null->null (vector-ref v 24)))
   (cons 'follower_count          (sql-null->null (vector-ref v 25)))
   (cons 'posts_count             (sql-null->null (vector-ref v 26)))
   (cons 'shareds_count           (sql-null->null (vector-ref v 27)))
   (cons 'unread_notifications    (sql-null->null (vector-ref v 28)))
   (cons 'last_com_time           (sql-null->null (vector-ref v 29)))
   (cons 'com_clout_date          (sql-null->null (vector-ref v 30)))
   (cons 'zen                     (sql-null->null (vector-ref v 31)))
   (cons 'spammer                 (sql-null->null (vector-ref v 32)))
   )))


;; \todo combine with pub-sexp->pub-hash
(define (user-sexp->user-hash s)
  (letrec ([acc (lambda (s h)
             (if (equal? s '()) h
                 (let* ([head (first s)]
                        [key (first head)]
                        [value (second head)])
                   (hash-set! h key value)
                   (acc (rest s) h))))])
    (acc s (make-hash))))

; we can't just use hash->list because the hash has member hashes which must be converted
(define (jsexpr->user-sexp j)
  (if (equal? j 'null)
      'null
      (jsexpr->user-sexp-not-null j)))

; \todo write a pipeline macro?
; \todo make a func to return jsexpr->sexp and sexp->jsexpr key names?
(define (jsexpr->user-sexp-not-null j)
  (list-add-hash-member         j 'id                      'id
  (list-add-hash-member         j 'joined                  'joined
  (list-add-hash-member-bool    j 'inactive                'inactive
  (list-add-hash-member         j 'clout                   'clout
  (list-add-hash-member         j 'word_count              'wordcount
  (list-add-hash-member         j 'average_com_numerator   'avgcomn ; \todo fix
  (list-add-hash-member         j 'average_com_denominator 'avgcomd
  (list-add-hash-member-bool    j 'ignore_newbies          'ignorenewbies
  (list-add-hash-member-bool    j 'global_ignored          'global-ignored
  (list-add-hash-member-bool    j 'new_tabs                'new-tabs
  (list-add-hash-member-bool    j 'publication_tabs        'pub-tabss
  (list-add-hash-member-bool    j 'reply_alerts            'reply-alerts
  (list-add-hash-member-bool    j 'pm_alerts               'pm-alerts
  (list-add-hash-member-bool    j 'follower_alerts         'follower-alerts
  (list-add-hash-member-bool    j 'shout_outs              'shout-outs
  (list-add-hash-member-bool    j 'badge_alerts            'badge-alerts
  (list-add-hash-member-bool    j 'saved_notifications     'saved-notifications
  (list-add-hash-member-bool    j 'feed_times              'feed-times
  (list-add-hash-member-bool    j 'share_counts            'share-counts
  (list-add-hash-member-bool    j 'show_global_filtered    'show-global-filtered
  (list-add-hash-member-bool    j 'follows_badges          'follows-badges
  (list-add-hash-member-bool    j 'embed_videos            'embed-videos
  (list-add-hash-member         j 'bio                     'bio
  (list-add-hash-member-bool    j 'email                   'email
  (list-add-hash-member         j 'hubski_style            'hubski-style
  (list-add-hash-member         j 'homepage                'homepage
  (list-add-hash-member         j 'follower_count          'followercount
  (list-add-hash-member         j 'posts_count             'postscount
  (list-add-hash-member         j 'shareds_count           'sharedscount
  (list-add-hash-member-bool    j 'unread_notifications    'unread-notifications
  (list-add-hash-member         j 'last_com_time           'lastcomtime
  (list-add-hash-member         j 'com_clout_date          'comcloutdate
  (list-add-hash-member-bo\ol    j 'zen                     'zen
  (list-add-hash-member-bool    j 'spammer                 'spammer
  (list-add-hash-member         j 'submitted               'submitted
  '()
  ))))))))))))))))))))))))))))))))))))

(define delete-user-query (virtual-statement "delete from users where id = $1;"))

;; \todo create separate insert and update?
(define (db-save-user h)
  (db-transaction
   (lambda ()
     (query-exec db-conn delete-user-query (hash-ref! h 'id 'nil))
     (db-save-user-no-delete-no-transaction h)
     )))
