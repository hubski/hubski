;; This file has been VERSIONED into the hub repo.
;; DO NOT change it from hub. Change in github.com/hubski/hubski,
;; and update the versioned file. Contributors WILL update this
;; file in hub without checking for changes.

#lang racket
(require db)
(require racket/trace)
(require "db.rkt")
(require "db-utils.rkt"
(require "publications.rkt")

(provide
 db-insert-publication
 db-update-publication
 db-load-publication
 db-get-publications
 db-get-publications-public
 db-save-publication-no-delete-no-transaction ; used by the conversion script, for speed
 db-has-community-tag?
 db-set-user-community-tag
 db-get-commonest-community-tag
 db-get-publication-recursive-public
 db-set-md
 db-get-md
 db-add-donation
 db-get-donations
 db-get-donations-month
 db-get-donations-year
 db-get-donations-total
 db-get-donations-total-month
 db-get-donations-total-year
 db-load-all-publications
 )

(define query-get-md
  (virtual-statement "select md from publications where id = $1::integer;"))
;; \return the md for the given publication, or #f if the id does not exist
(define (db-get-md publication-id)
  (query-value db-conn query-get-md publication-id))

(define query-set-md
  (virtual-statement "update publications set md = $1::text where id = $2::integer;"))
;; \return the md for the given publication, or #f if the id does not exist
(define (db-set-md publication-id md)
  (query-exec db-conn query-set-md md publication-id))

(define query-add-donation
  (virtual-statement "insert into donations (username, donation_cents, donation_time) values ($1::text, $2::integer, now());"))
(define (db-add-donation username donation)
  (query-exec db-conn query-add-donation username donation))

(define query-get-donations
  (virtual-statement "select sum(donation_cents) from donations where username = $1::text;"))
(define (db-get-donations username)
  (let ([val (query-value db-conn query-get-donations username)])
    (if (eq? val sql-null) 0.0 val)))

(define query-get-donations-month
  (virtual-statement "select sum(donation_cents) from donations where username = $1::text and donation_time >= now() - interval '1 month';"))
(define (db-get-donations-month username)
  (let ([val (query-value db-conn query-get-donations-month username)])
    (if (eq? val sql-null) 0.0 val)))

(define query-get-donations-year
  (virtual-statement "select sum(donation_cents) from donations where username = $1::text and donation_time >= now() - interval '12 month';"))
(define (db-get-donations-year username)
  (let ([val (query-value db-conn query-get-donations-year username)])
    (if (eq? val sql-null) 0.0 val)))

(define query-get-donations-total
  (virtual-statement "select sum(donation_cents) from donations;"))
(define (db-get-donations-total)
  (let ([val (query-value db-conn query-get-donations-total)])
    (if (eq? val sql-null) 0.0 val)))

(define query-get-donations-total-month
  (virtual-statement "select sum(donation_cents) from donations where donation_time >= now() - interval '1 month';"))
(define (db-get-donations-total-month)
  (let ([val (query-value db-conn query-get-donations-total-month)])
    (if (eq? val sql-null) 0.0 val)))

(define query-get-donations-total-year
  (virtual-statement "select sum(donation_cents) from donations where donation_time >= now() - interval '12 month';"))
(define (db-get-donations-total-year)
  (let ([val (query-value db-conn query-get-donations-total-year)])
    (if (eq? val sql-null) 0.0 val)))

(define query-has-community-tag?
  (virtual-statement "select count(1) from publication_community_tagses where id = $1::integer and username = $2::text;"))
(define (db-has-community-tag? publication-id username)
  (not (equal? 0 (query-value db-conn query-has-community-tag? publication-id username))))

(define query-get-commonest-community-tag
  (virtual-statement "select tag, count(tag) from publication_community_tagses where id = $1::integer group by tag order by count(tag) desc limit 1;"))
;; \return A list, of the most common community tag, and the count.
;;         If the publication has no ctags, returns ("" 0).
(define (db-get-commonest-community-tag publication-id)
  (let ([maybe-row (query-maybe-row db-conn query-get-commonest-community-tag publication-id)])
    (if (equal? maybe-row #f)
        '("" 0)
        (list (vector-ref maybe-row 0) (vector-ref maybe-row 1)))))

(define query-set-user-community-tag (virtual-statement "insert into publication_community_tagses (id, username, tag) values ($1::integer, $2::text, $3::text);"))
(define query-delete-user-community-tag (virtual-statement "delete from publication_community_tagses where id = $1::integer and username = $2::text;"))
(define (db-set-user-community-tag publication-id username ctag)
  (db-transaction
   (lambda ()
     (query-exec db-conn query-delete-user-community-tag publication-id username)
     (query-exec db-conn query-set-user-community-tag publication-id username ctag))))

(define (pub-ids-list-vecs->jsexpr l)
  (map (lambda (id-vec) (vector-ref id-vec 0)) l))

(define query-get-publications (virtual-statement "select id from publications;"))
(define (db-get-publications)
    (pub-ids-list-vecs->jsexpr (query-rows db-conn query-get-publications)))

(define query-get-publications-public (virtual-statement "select id from publications where mail = false and deleted = false and draft = false;"))
;; gets a list of publications (which is also a jsexpr), which are not mail, deleted, or drafts
;; \todo add db indices for publications mail, deleted, draft
(define (db-get-publications-public)
    (pub-ids-list-vecs->jsexpr (query-rows db-conn query-get-publications-public)))

(define (+id h id)
  (hash-set h 'id id))

(define (pub-vec-id->jsexpr v)
  (+id (pub-vec->jsexpr v) (sql-null->null (vector-ref v 16))))

;; \todo define the list of publication columns, instead of duplicating them everywhere.
(define query-get-publication-recursive (virtual-statement "with recursive pubchilds(username, time, date, url, title, mail, tag, tag2, text, web_domain, score, deleted, draft, parent_id, locked, no_kill, id) as (select username, time, date, url, title, mail, tag, tag2, text, web_domain, score, deleted, draft, parent_id, locked, no_kill, id from publications where id = $1::integer union all select p.username, p.time, p.date, p.url, p.title, p.mail, p.tag, p.tag2, p.text, p.web_domain, p.score, p.deleted, p.draft, p.parent_id, p.locked, p.no_kill, p.id from pubchilds c, publications p where p.parent_id = c.id) select username, time, date, url, title, mail, tag, tag2, text, web_domain, score, deleted, draft, parent_id, locked, no_kill, id from pubchilds;"))
;; Returns a jsexpr of the requested publication and all its descendants.
(define (db-get-publication-recursive-public id)
  (letrec ([jsexpr-pubs-list->jsexpr-recursive-pub
            (λ (l root-id)
              (letrec ([pub-add-children
                        (λ (p plist)
                          (let ([children (filter (λ (child) (eq? (hash-ref child 'parent_id) (hash-ref p 'id))) plist)])
                            (if (empty? children) p
                                (let ([recursive-added-children (map (λ (child) (pub-add-children child plist)) children)])
                                  (hash-set p 'children recursive-added-children)))))])
                (if (or (not (list? l)) (empty? l)) 'null
                    (let ([pub (findf (λ (p) (equal? root-id (hash-ref p 'id))) l)])
                      (if (equal? pub #f) 'null (pub-add-children pub l))))))]
           [all-pubs-vecs (query-rows db-conn query-get-publication-recursive id)]
           [all-pubs-jsexprs (map pub-vec-id->jsexpr all-pubs-vecs)]
           [pubs (filter publication-is-public all-pubs-jsexprs)])
    (if (empty? pubs) 'null
        (jsexpr-pubs-list->jsexpr-recursive-pub pubs id))))

(define (type->id type story-id comment-id)
  (cond [(equal? type 'story) story-id]
        [(equal? type 'comment) comment-id]
        [else sql-null]))

(define (get-type-id type db)
  (vector-ref (query-row db-conn "select \"id\" from \"publication_types\" where \"publication_type\" =  $1"
                         type)
              0))

(define (db-insert-publication sexp)
  (db-transaction
   (lambda () (db-save-publication-no-transaction sexp #t))))

(define (db-update-publication sexp)
  (db-transaction
   (lambda () (db-save-publication-no-transaction sexp #f))))

(define (save-votes id votes)
  (let ([table "publication_votes"])
    (if (and (list? votes) (not (empty? votes)))
        (begin
          (db-delete-from table id)
          (map (lambda (arc-vote)
                 (let*
                     ([vote (arc-list->list arc-vote)]
                      [vote_id (first vote)]
                      ; sometimes vote has an IP in the middle, and is thus length 6, with user,up,num one farther in the list.
                      [username (if (equal? (length vote) 5)
                                    (third vote)
                                    (fourth vote))]
                      [up-atom (if (equal? (length vote) 5)
                                   (fourth vote)
                                   (fifth vote))]
                      [up (equal? up-atom 'up)]
                      [num (if (equal? (length vote) 5)
                               (fifth vote)
                               (sixth vote))]
                      [query (string-append "insert into \"" table "\" (id, vote_id, username, up, num) values ($1, $2, $3, $4, $5);")])
                   (query-exec db-conn query id vote_id username up num)
                   ))
               votes))
        void)))

(define query-delete-publication (virtual-statement "delete from \"publications\" where \"id\" = $1::integer;"))
(define query-insert-publication (virtual-statement "insert into \"publications\" (id, type_id, username, time, date, url, title, mail, tag, tag2, text, web_domain, score, deleted, draft, parent_id, locked, no_kill) values ($1::integer, $2::integer, $3::text, $4::integer, $5::text, $6::text, $7::text, $8::boolean, $9::text, $10::text, $11::text, $12::text, $13::integer, $14::boolean, $15::boolean, $16::integer, $17::boolean, $18::boolean);"))
(define query-update-publication (virtual-statement "update publications set type_id = $1::integer, username = $2::text, time = $3::integer, date = $4::text, url = $5::text, title = $6::text, mail = $7::boolean, tag = $8::text, tag2 = $9::text, text = $10::text, web_domain = $11::text, score = $12::integer, deleted = $13::boolean, draft = $14::boolean, parent_id = $15::integer, locked = $16::boolean, no_kill = $17::boolean where id = $18::integer;"))
;; insert or update a publication.
;; \param sexp the publication s-expression to insert or update
;; \param insert boolean, whether to insert or update
(define (db-save-publication-no-transaction sexp insert)
  (let*
      (
       [save-valid? (lambda (v) (not (or (equal? v 'nil) (equal? v sql-null))))]
       [save-otm-pair-no-delete
        (lambda (id vals table val-column)
          (if (and (list? vals) (save-valid? id))
              (map (lambda (val)
                     (if (save-valid? val)
                         (db-insert-idval id val table val-column)
                         void)
                     ) vals)
              void))]
       [save-otm-pair
        ; NOTE this doesn't delete, if the value(s) are all nil. This is how the old temstore works. I think.
        (lambda (id vals table val-column)
          (let ([valid-vals (if (pair? vals) (filter save-valid? vals) '())])
            (if (or (not (save-valid? id)) (empty? valid-vals)) void
                (begin
                  (db-delete-from table id)
                  (save-otm-pair-no-delete id valid-vals table val-column)))))]
       [save-cc (lambda (id ccs) (save-otm-pair id ccs "publication_cc" "username"))]
       [save-community-tags (lambda (id ctags) (save-otm-pair id ctags "publication_community_tags" "tag"))]
       [save-saved-by (lambda (id lst) (save-otm-pair id lst "publication_saved_by" "username"))]
       [save-shared-by (lambda (id lst) (save-otm-pair id lst "publication_shared_by" "username"))]
       [save-badged-by (lambda (id lst) (save-otm-pair id lst "publication_badged_by" "username"))]
       [save-badged-kids (lambda (id lst) (save-otm-pair id lst "publication_badged_kids" "kid_id"))]
       [save-cubbed-by (lambda (id lst) (save-otm-pair id lst "publication_cubbed_by" "username"))]
       [save-kids  (lambda (id lst) (save-otm-pair id lst "publication_kids" "kid_id"))]
       [type-id-story (get-type-id "story" db-conn)]
       [type-id-comment (get-type-id "comment" db-conn)]
       [h (make-hash sexp)]
       [p-id (sqlnil (safe-car (val-if h 'id)))]
       [p-type (sqlnil (type->id (safe-car (val-if h 'type)) type-id-story type-id-comment))]
       [p-username (sqlnil (safe-car (val-if h 'by)))]
       [p-time (sqlnil (safe-car (val-if h 'time)))]
       [p-date (sqlnil (safe-car (val-if h 'date)))]
       [p-url (sqlnil (safe-car (val-if h 'url)))]
       [p-title (sqlnil (safe-car (val-if h 'title)))]
       [p-mail (sqlnil (not (equal? 'nil (safe-car (val-if h 'mail)))))]
       [p-tag (sqlnil (safe-car (safe-car (val-if h 'tag))))]
       [p-tag2 (sqlnil (safe-car (safe-car (val-if h 'tag2))))]
       [p-text (sqlnil (safe-car (val-if h 'text)))]
       [p-webdomain (sqlnil (safe-car (safe-car (val-if h 'domain))))]
       [p-score (sqlnil (safe-car (val-if h 'score)))]
       [p-deleted (sqlnil (not (equal? 'nil(val-if h 'deleted))))]
       [p-draft (sqlnil (not (equal? 'nil (val-if h 'draft))))]
       [p-parent-id (sqlnil (val-if h 'parent))]
       [keys (sqlnil (val-if h 'keys))]
       [p-locked (sqlnil (safe-member keys 'locked))]
       [p-no-kill (sqlnil (safe-member keys 'nokill))]
       [p-cc (arc-list->list (sqlnil (val-if h 'cc)))]
       [p-ctags (arc-list->list (sqlnil (val-if h 'ctag)))]
       [p-votes (arc-list->list (sqlnil     (val-if h 'votes)))]
       [p-saved-by (arc-list->list (sqlnil  (val-if h 'savedby)))]
       [p-shared-by (arc-list->list (sqlnil (val-if h 'sharedby)))]
       [p-badged-by (arc-list->list (sqlnil (val-if h 'badgedby)))]
       [p-badged-kids (arc-list->list (sqlnil (val-if h 'badgedkids)))]
       [p-cubbed-by (arc-list->list (sqlnil (val-if h 'cubbedby)))]
       [p-kids (arc-list->list (sqlnil (val-if h 'kids)))])
    ;; MUST use a transaction. Because we delete-then-insert, removing the transaction would be a race condition.
    (query-exec db-conn "BEGIN;") 
    ;; \todo change to use upsert, when we update to postgresl 9.5
    (cond
      [insert (begin (query-exec db-conn query-delete-publication p-id)
                     (query-exec db-conn query-insert-publication p-id p-type p-username p-time p-date p-url p-title p-mail p-tag p-tag2 p-text p-webdomain p-score p-deleted p-draft p-parent-id p-locked p-no-kill))]
      [else (query-exec db-conn query-update-publication p-type p-username p-time p-date p-url p-title p-mail p-tag p-tag2 p-text p-webdomain p-score p-deleted p-draft p-parent-id p-locked p-no-kill p-id)])
    (save-cc p-id p-cc)
    (save-community-tags p-id p-ctags)
    (save-votes p-id p-votes)
    (save-saved-by p-id p-saved-by)
    (save-shared-by p-id p-shared-by)
    (save-badged-by p-id p-badged-by)
    (save-badged-kids p-id p-badged-kids)
    (save-cubbed-by p-id p-cubbed-by)
    (save-kids p-id p-kids)
    ;; (save-kid p-parent-id p-id)
    (query-exec db-conn "COMMIT;")
    ;; (write "db-save-publication finished") (newline)
    (void)))

(define (pub-vec->jsexpr v)
  (make-hasheq (list
   (cons 'user      (sql-null->null (vector-ref v 0)))
   (cons 'time      (sql-null->null (vector-ref v 1)))
   (cons 'date      (sql-null->null (vector-ref v 2)))
   (cons 'url       (sql-null->null (vector-ref v 3)))
   (cons 'title     (sql-null->null (vector-ref v 4)))
   (cons 'mail      (sql-null->null (vector-ref v 5)))
   (cons 'tag       (sql-null->null (vector-ref v 6)))
   (cons 'tag2      (sql-null->null (vector-ref v 7)))
   (cons 'text      (sql-null->null (vector-ref v 8)))
   (cons 'domain    (sql-null->null (vector-ref v 9)))
   (cons 'score     (sql-null->null (vector-ref v 10)))
   (cons 'deleted   (sql-null->null (vector-ref v 11)))
   (cons 'draft     (sql-null->null (vector-ref v 12)))
   (cons 'parent_id (sql-null->null (vector-ref v 13)))
   (cons 'locked    (sql-null->null (vector-ref v 14)))
   (cons 'no_kill   (sql-null->null (vector-ref v 15)))
   )))

(define by-id-query  (virtual-statement "select \"username\", \"time\", \"date\", \"url\", \"title\", \"mail\", \"tag\", \"tag2\", \"text\", \"web_domain\", \"score\", \"deleted\", \"draft\", \"parent_id\", \"locked\", \"no_kill\" from \"publications\" where \"id\" =  $1"))

; this could be made more efficient, by querying the type_id with the publication query, and passing the result here.
(define type-query  (virtual-statement "select \"publication_type\" from \"publication_types\" where \"id\" = (select \"type_id\" from \"publications\" where \"id\" = $1);"))
(define votes-query (virtual-statement "select \"vote_id\", \"username\", \"up\", \"num\" from \"publication_votes\" where \"id\" = $1;"))
(define (otm-from-id-query table value-column) (virtual-statement (string-append "select \"" value-column "\" from \"" table "\" where \"id\" = $1;")))
(define cc-query             (otm-from-id-query "publication_cc"             "username"))
(define community-tags-query (otm-from-id-query "publication_community_tags" "tag"))
(define saved-by-query       (otm-from-id-query "publication_saved_by"       "username"))
(define shared-by-query      (otm-from-id-query "publication_shared_by"      "username"))
(define badged-by-query      (otm-from-id-query "publication_badged_by"      "username"))
(define badged-kids-query    (otm-from-id-query "publication_badged_kids"    "kid_id"))
(define cubbed-by-query      (otm-from-id-query "publication_cubbed_by"      "username"))
(define kids-query           (otm-from-id-query "publication_kids"           "kid_id"))


(define load-all-pubs-query  (virtual-statement "select username, time, date, url, title, mail, tag, tag2, text, web_domain, score, deleted, draft, parent_id, locked, no_kill, type_id, id from publications;"))

(define cc-all-query             (otm-all-query "publication_cc"             "username"))
(define community-tags-all-query (otm-all-query "publication_community_tags" "tag"))
(define saved-by-all-query       (otm-all-query "publication_saved_by"       "username"))
(define shared-by-all-query      (otm-all-query "publication_shared_by"      "username"))
(define badged-by-all-query      (otm-all-query "publication_badged_by"      "username"))
(define badged-kids-all-query    (otm-all-query "publication_badged_kids"    "kid_id"))
(define cubbed-by-all-query      (otm-all-query "publication_cubbed_by"      "username"))
(define kids-all-query           (otm-all-query "publication_kids"           "kid_id"))
(define votes-all-query (virtual-statement "select vote_id, username, up, num, id from publication_votes;"))
(define types-all-query (virtual-statement "select id, publication_type from publication_types;"))

(define (get-type-hash)
  (let ([vals (query-rows db-conn types-all-query)]
        [h (make-hasheq)])
    (for-each
     (lambda (v)
       (hash-set! h (vector-ref v 0) (vector-ref v 1)))
     vals)
    h))

;; \returns a hash of pub sexprs, as expected by Arc (as opposed to jsexprs)
(define (pub-vec-list->pubs-hash pub-vec-list)
  (let ([h (make-hasheq)]
        [type-hash (get-type-hash)])
    (for-each (lambda (v)
           (let* ([id (vector-ref v 17)]
                 [typeid (vector-ref v 16)]
                 [pub (pub-vec->jsexpr v)])
             (hash-set! pub 'id id)
             (hash-set! pub 'type (hash-ref! type-hash typeid 'missing))
             (hash-set! h id (pub-sexp->pub-hash (jsexpr->pub-sexp pub)))))
         pub-vec-list)
    h))

(define (+otm-all h stmt json-key)
  (let* ([vals (query-rows db-conn stmt)])
    (hash-for-each
     h
     (lambda (key-id value-pub)
       (hash-set! value-pub json-key '())))
    (if (empty? vals) 'nil
        (for-each
         (lambda (v)
           (let* ([row-id (vector-ref v 0)]
                  [pub-h (hash-ref! h row-id (make-hasheq))]
                  [pub-otm-list (hash-ref! pub-h json-key '())]
                  [otm-val (vector-ref v 1)])
             (hash-set! pub-h json-key (cons otm-val pub-otm-list))))
         vals))
        h))

(define (+votes-all h)
  (let* ([votes (query-rows db-conn votes-all-query)])
    (hash-for-each h
     (lambda (id pub)
       (hash-set! pub 'votes '())
       (hash-set! h id pub)))
    (for-each
     (lambda (vote)
       (let* ([id (vector-ref vote 0)]
              [vote-hash (hasheq
                          'id id
                          'user (vector-ref vote 1)
                          'up (vector-ref vote 2)
                          'num (vector-ref vote 3))]
              [pub (hash-ref! h id (make-hasheq))]
              [pub-votes (hash-ref! pub 'votes '())])
         (hash-set! pub 'votes (cons vote-hash pub-votes))
         (hash-set! h id pub) ; \todo determine if necessary
         )) votes)
    h))

;; \todo change +x to use new all queries, change +otm to add to hash-of-pubs
(define (db-load-all-publications)
  (let*
      (
       [+cc             (lambda (h) (+otm-all h cc-all-query             'cc))]
       [+community-tags (lambda (h) (+otm-all h community-tags-all-query 'ctag))]
       [+saved-by       (lambda (h) (+otm-all h saved-by-all-query       'savedby))]
       [+shared-by      (lambda (h) (+otm-all h shared-by-all-query      'sharedby))]
       [+badged-by      (lambda (h) (+otm-all h badged-by-all-query      'badgedby))]
       [+badged-kids    (lambda (h) (+otm-all h badged-kids-all-query    'badgedkids))]
       [+cubbed-by      (lambda (h) (+otm-all h cubbed-by-all-query      'cubbedby))]
       [+kids           (lambda (h) (+otm-all h kids-all-query           'kids))]
       [+otms (compose1 +votes-all +kids +cubbed-by +badged-kids +badged-by +shared-by +saved-by +community-tags +cc)] ;; +type ;; type is added by pub-vec-list->pub-hash       
       [pub-vec-list (query-rows db-conn load-all-pubs-query)])
    (+otms (pub-vec-list->pubs-hash pub-vec-list))))

(define (db-load-publication id)
  (let*
  ([+type
     (lambda (h)
     (let* ([id (hash-ref h 'id)]
            [maybe-type-id (query-maybe-row db-conn type-query id)])
       (if (not maybe-type-id)
           h
           (hash-set h 'type (vector-ref maybe-type-id 0)))))]
  [+otm
   (lambda (h stmt json-key)
     (let* ([id (hash-ref h 'id)]
           [vals (query-rows db-conn stmt id)]
           [vals-list (map (lambda (val) (vector-ref val 0)) vals)])
       (hash-set h json-key vals-list)))]
  [+cc (lambda (h) (+otm h cc-query 'cc))]
  [+community-tags (lambda (h) (+otm h community-tags-query 'community_tag))]
  [+saved-by (lambda (h) (+otm h saved-by-query 'saved_by))]
  [+shared-by (lambda (h) (+otm h shared-by-query 'shared_by))]
  [+badged-by (lambda (h) (+otm h badged-by-query 'badged_by))]
  [+badged-kids (lambda (h) (+otm h badged-kids-query 'badged_kids))]
  [+cubbed-by (lambda (h) (+otm h cubbed-by-query 'cubbed_by))]
  [+kids (lambda (h) (+otm h kids-query 'kids))]
  [+votes
   (lambda (h)
     (let* ([id (hash-ref h 'id)]
           [votes (query-rows db-conn votes-query id)]
           [votes-list
            (map (lambda (vote)
                   (make-hasheq (list
                    (cons 'id (vector-ref vote 0))
                    (cons 'user (vector-ref vote 1))
                    (cons 'up (vector-ref vote 2))
                    (cons 'num (vector-ref vote 3)))))
                 votes)])
       (hash-set h 'votes votes-list)))]
  [maybe-pub-vec (query-maybe-row db-conn by-id-query id)])
    (if (not maybe-pub-vec)
        'null
        (+votes
        (+kids
        (+cubbed-by
        (+badged-kids
        (+badged-by
        (+shared-by
        (+saved-by
        (+community-tags
        (+cc
        (+type
        (+id (pub-vec->jsexpr maybe-pub-vec) id))))))))))))))

(define (db-save-publication-no-delete sexp)
  (db-transaction (lambda () (db-save-publication-no-delete-no-transaction sexp))))

; Used by the conversion. Deletes are expensive, because there are no indexes.
; \todo combine with db-save-publication
;
; NOTE this has a difference from db-save-publication (this is why duplicate code is bad!).
;      This calls (string-or-nil) on 'text, because there is ONE old pub with (text t). This shouldn't be a problem once the conversion to SQL is done. Hence, I'm leaving db-save-publication not doing it, and only doing it in this function, which should only be used for the conversion.
(define (db-save-publication-no-delete-no-transaction sexp)
  (let*
      ([save-otm-pair
        (lambda (id vals table val-column)
          (if (pair? vals)
              (map (lambda (val)
                     (if (not (equal? val 'nil))
                         (db-insert-idval id val table val-column)
                         (void))
                     ) vals)
              void))]
    [save-cc (lambda (id ccs) (save-otm-pair id ccs "publication_cc" "username"))]
    [save-community-tags (lambda (id ctagses) (save-otm-pair id ctagses "publication_community_tags" "tag"))]
    [save-community-tagses
     (lambda (id ctags)
       (if (not (pair? ctags)) (void)
           (letrec ([table "publication_community_tagses"]
                    [acc (lambda (id ctags)
                           (if (empty? ctags) (void)
                               (let* ([ctagpair (first ctags)]
                                      [ctag (first ctagpair)]
                                      [username (second ctagpair)])
                                 (query-exec db-conn (string-append "insert into \"" table "\" (id, username, tag) values ($1::integer, $2::text, $3::text);")
                                             id username ctag)
                                 (acc id (rest ctags)))))])
             (query-exec db-conn (string-append "delete from \"" table "\" where \"id\" = $1::integer;") id)
             (acc id ctags))))]
    [save-votes
     (lambda (id votes)
       (let
           ([table "publication_votes"])
         (if (pair? votes)
             (map (lambda (vote)
                    (let* (
                          [vote_id (first vote)]
                    ; sometimes vote has an IP in the middle, and is thus length 6, with user,up,num one farther in the list.
                    [username (if (equal? (length vote) 5)
                                         (third vote)
                                         (fourth vote))]
                    [up-atom (if (equal? (length vote) 5)
                                        (fourth vote)
                                        (fifth vote))]
                    [up (equal? up-atom 'up)]
                    [num (if (equal? (length vote) 5)
                                    (fifth vote)
                                    (sixth vote))]
                    [query (string-append "insert into \"" table "\" (id, vote_id, username, up, num) values ($1, $2, $3, $4, $5);")])
                          (query-exec db-conn query id vote_id username up num)
                          ))
                    votes)
                  void)))]
  [save-saved-by (lambda (id lst) (save-otm-pair id lst "publication_saved_by" "username"))]
  [save-shared-by (lambda (id lst) (save-otm-pair id lst "publication_shared_by" "username"))]
  [save-badged-by (lambda (id lst) (save-otm-pair id lst "publication_badged_by" "username"))]
  [save-badged-kids (lambda (id lst) (save-otm-pair id lst "publication_badged_kids" "kid_id"))]
  [save-cubbed-by (lambda (id lst) (save-otm-pair id lst "publication_cubbed_by" "username"))]
  [save-kids  (lambda (id lst) (save-otm-pair id lst "publication_kids" "kid_id"))]
  [type-id-story (get-type-id "story" db-conn)]
  [type-id-comment (get-type-id "comment" db-conn)]
  [h (make-hash sexp)]
  [p-id (sqlnil (car (val-if h 'id)))]
  [p-type (sqlnil (type->id (car (val-if h 'type)) type-id-story type-id-comment))]
  [p-username (sqlnil (car (val-if h 'by)))]
  [p-time (sqlnil (car (val-if h 'time)))]
  [p-date (sqlnil (safe-car (val-if h 'date)))]
  [p-url (sqlnil (car (val-if h 'url)))]
  [p-title (sqlnil (car (val-if h 'title)))]
  [p-mail (sqlnil (not (equal? 'nil (safe-car (val-if h 'mail)))))]
  [p-tag (sqlnil (safe-car (car (val-if h 'tag))))]
  [p-tag2 (sqlnil (safe-car (car (val-if h 'tag2))))]
  [p-text (sqlnil (string-or-nil (car (val-if h 'text))))]
  [p-md (sqlnil (car (val-if h 'md)))]
  [p-webdomain (sqlnil (safe-car (car (val-if h 'domain))))]
  [p-score (sqlnil (car (val-if h 'score)))]
  [p-deleted (sqlnil (not (equal? 'nil (safe-car (val-if h 'deleted)))))]
  [p-draft (sqlnil (not (equal? 'nil (safe-car (val-if h 'draft)))))]
  [p-parent-id (sqlnil (car (val-if h 'parent)))]
  [keys (sqlnil (safe-car (val-if h 'keys)))]
  [p-locked (sqlnil (safe-member keys 'locked))]
  [p-no-kill (sqlnil (safe-member keys 'nokill))]
  [p-cc (safe-car (val-if h 'cc))]
  [p-ctags (safe-car (val-if h 'ctag))]
  [p-ctagses (safe-car (val-if h 'ctags))]
  [p-votes (safe-car (val-if h 'votes))]
  [p-saved-by (safe-car (val-if h 'savedby))]
  [p-shared-by (safe-car (val-if h 'sharedby))]
  [p-badged-by (safe-car (val-if h 'badgedby))]
  [p-badged-kids (safe-car (val-if h 'badgedkids))]
  [p-cubbed-by (safe-car (val-if h 'cubbedby))]
  [p-kids (safe-car (val-if h 'kids))])
  ;; \todo change to use upsert, when we update to postgresl 9.5
 (query-exec db-conn "insert into \"publications\" (id, type_id, username, time, date, url, title, mail, tag, tag2, text, md, web_domain, score, deleted, draft, parent_id, locked, no_kill) values ($1::integer, $2::integer, $3::text, $4::integer, $5::text, $6::text, $7::text, $8::boolean, $9::text, $10::text, $11::text, $12::text, $13::text, $14::integer, $15::boolean, $16::boolean, $17::integer, $18::boolean, $19::boolean);" p-id p-type p-username p-time p-date p-url p-title p-mail p-tag p-tag2 p-text p-md p-webdomain p-score p-deleted p-draft p-parent-id p-locked p-no-kill)
  (save-cc p-id p-cc)
  (save-community-tags p-id p-ctags)
  (save-community-tagses p-id p-ctagses)
  (save-votes p-id p-votes)
  (save-saved-by p-id p-saved-by)
  (save-shared-by p-id p-shared-by)
  (save-badged-by p-id p-badged-by)
  (save-badged-kids p-id p-badged-kids)
  (save-cubbed-by p-id p-cubbed-by)
  (save-kids p-id p-kids)
  (void)))
