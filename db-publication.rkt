#lang racket
(require db)
(require "db.rkt")

(provide
 db-save-publication
 db-load-publication
 db-get-publications
 db-save-publication-no-delete-no-transaction ; used by the conversion script, for speed 
 )

(define (pub-ids-list-vecs->jsexpr l)
  (map (lambda (id-vec) (vector-ref id-vec 0)) l))

(define (db-get-publications)
  (let* ([publication-ids-query "select \"id\" from \"publications\";"]
        [pubs-vec (query-rows db-conn publication-ids-query)])
    (pub-ids-list-vecs->jsexpr pubs-vec)))

; nil if list is nil, else (car l)
(define (safe-car l)
  (if (not (list? l)) l (car l)))

(define (type->id type story-id comment-id)
  (cond [(equal? type 'story) story-id]
        [(equal? type 'comment) comment-id]
        [else sql-null]))

(define (get-type-id type db)
  (vector-ref (query-row db-conn "select \"id\" from \"publication_types\" where \"publication_type\" =  $1"
                         type)
              0))

; gets the hash key if it exists, else 'nil
(define (val-if h key)
  (if (hash-has-key? h key) (hash-ref h key) 'nil))

; like (member) but returns #t instead of the list, and #f if l isn't a list
(define (safe-member l val)
  (and (list? l) (not (equal? (member val l) #f))))

(define (string-or-nil val)
  (if (string? val) val 'nil))

(define (sqlnil val)
  (if (equal? val 'nil) sql-null val))

; many of the one-to-many tables have a single key and string.
; this lets us insert into them easily.
(define (db-insert-idval id val table val-column)
  (let ([query (string-append "insert into \"" table "\" (id, \"" val-column "\") values ($1, $2);")])
    (query-exec db-conn query id val)))

  ; used for deleting one-to-many table ids before insertion
(define (db-delete-from table id)
  (let ([query (string-append "delete from \"" table "\" where \"id\" = $1::integer;")])
    (query-exec db-conn query id)))


(define (db-save-publication sexp)
  (db-transaction (lambda () (db-save-publication-no-transaction sexp))))

(define (db-save-publication-no-transaction sexp)
  (let*
      ([save-otm-pair
        (lambda (id vals table val-column)
          (db-delete-from table id)
          (if (list? vals)
              (map (lambda (val)
                     (if (not (equal? val 'nil))
                         (db-insert-idval id val table val-column)
                         (void))
                     ) vals)
              void))]
    [save-cc (lambda (id ccs) (save-otm-pair id ccs "publication_cc" "username"))]
    [save-community-tags (lambda (id ctags) (save-otm-pair id ctags "publication_community_tags" "tag"))]
    [save-community-tagses (lambda (id ctagses) (save-otm-pair id ctagses "publication_community_tagses" "tag"))]
    [save-search-text (lambda (id lst) (save-otm-pair id lst "publication_search_text" "word"))]
    [save-search-title (lambda (id lst) (save-otm-pair id lst "publication_search_title" "word"))]
    [save-search-url (lambda (id lst) (save-otm-pair id lst "publication_search_url" "word"))]
    [save-votes
     (lambda (id votes)
       (let
           ([table "publication_votes"])
         (db-delete-from table id)
         (if (list? votes)
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
  [type-id-comment (get-type-id "story" db-conn)]
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
  [p-text (sqlnil (car (val-if h 'text)))]
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
  [p-ctagses (safe-car (safe-car (val-if h 'ctags)))]
  [p-search-text (safe-car (safe-car (val-if h 'searchtext)))]
  [p-search-title (safe-car (safe-car (val-if h 'searchtitle)))]
  [p-search-url (safe-car (val-if h 'searchurl))]
  [p-votes (safe-car (val-if h 'votes))]
  [p-saved-by (safe-car (val-if h 'savedby))]
  [p-shared-by (safe-car (val-if h 'sharedby))]
  [p-badged-by (safe-car (val-if h 'badgedby))]
  [p-badged-kids (safe-car (val-if h 'badgedkids))]
  [p-cubbed-by (safe-car (val-if h 'cubbedby))]
  [p-kids (safe-car (val-if h 'kids))])
  ;; (write (string-append "db-save-publication " (number->string p-id) " starting")) (newline) 
  ;; (write "type: ")      (write p-type) (newline)
  ;; (write "user: ")      (write p-username) (newline)
  ;; (write "time: ")      (write p-time) (newline)
  ;; (write "date: ")      (write p-date) (newline)
  ;; (write "url: ")       (write p-url) (newline)
  ;; (write "title: ")     (write p-title) (newline)
  ;; (write "mail: ")      (write p-mail) (newline)
  ;; (write "tag: ")       (write p-tag) (newline)
  ;; (write "tag2: ")      (write p-tag2) (newline)
  ;; (write "text: ")      (write p-text) (newline)
  ;; (write "md: ")        (write p-md) (newline)
  ;; (write "webdomain: ") (write p-webdomain) (newline)
  ;; (write "score: ")     (write p-score) (newline)
  ;; (write "deleted: ")   (write p-deleted) (newline)
  ;; (write "draft: ")     (write p-draft) (newline)
  ;; (write "parentid: ")  (write p-parent-id); (newline)
  ;; (write "locked: ")    (write p-locked) (newline)
  ;; (write "nokill: ")    (write p-no-kill) (newline)
  ;; MUST use a transaction. Because we delete-then-insert, removing the transaction would be a race condition.    
  (query-exec db-conn "BEGIN;") 
  ;; \todo change to use upsert, when we update to postgresl 9.5
  (query-exec db-conn "delete from \"publications\" where \"id\" = $1::integer;" p-id)
 (query-exec db-conn "insert into \"publications\" (id, type_id, username, time, date, url, title, mail, tag, tag2, text, md, web_domain, score, deleted, draft, parent_id, locked, no_kill) values ($1::integer, $2::integer, $3::text, $4::integer, $5::text, $6::text, $7::text, $8::boolean, $9::text, $10::text, $11::text, $12::text, $13::text, $14::integer, $15::boolean, $16::boolean, $17::integer, $18::boolean, $19::boolean);" p-id p-type p-username p-time p-date p-url p-title p-mail p-tag p-tag2 p-text p-md p-webdomain p-score p-deleted p-draft p-parent-id p-locked p-no-kill)
  (save-cc p-id p-cc)  
  (save-community-tags p-id p-ctags)  
  (save-community-tagses p-id p-ctagses)  
  (save-search-text p-id p-search-text)  
  (save-search-title p-id p-search-title)  
  (save-search-url p-id p-search-url)
  (save-votes p-id p-votes)
  (save-saved-by p-id p-saved-by)  
  (save-shared-by p-id p-shared-by)
  (save-badged-by p-id p-badged-by)
  (save-badged-kids p-id p-badged-kids)
  (save-cubbed-by p-id p-cubbed-by)
  (save-kids p-id p-kids)
  (query-exec db-conn "COMMIT;")
  ;; (write "db-save-publication finished") (newline) 
  (void)))

; if val is sql-null, returns 'null, else val
(define (sql-null->null val)
  (if (equal? val sql-null) 'null val))

(define (pub-vec->jsexpr v)
  (hasheq
   'user      (sql-null->null (vector-ref v 0))
   'time      (sql-null->null (vector-ref v 1))
   'date      (sql-null->null (vector-ref v 2))
   'url       (sql-null->null (vector-ref v 3))
   'title     (sql-null->null (vector-ref v 4))
   'mail      (sql-null->null (vector-ref v 5))
   'tag       (sql-null->null (vector-ref v 6))
   'tag2      (sql-null->null (vector-ref v 7))
   'text      (sql-null->null (vector-ref v 8))
   'md        (sql-null->null (vector-ref v 9))
   'domain    (sql-null->null (vector-ref v 10))
   'score     (sql-null->null (vector-ref v 11))
   'deleted   (sql-null->null (vector-ref v 12))
   'draft     (sql-null->null (vector-ref v 13))
   'parent_id (sql-null->null (vector-ref v 14))
   'locked    (sql-null->null (vector-ref v 15))
   'no_kill   (sql-null->null (vector-ref v 16))
   ))

(define (db-load-publication id)
  (let*
      ([by-id-query "select \"username\", \"time\", \"date\", \"url\", \"title\", \"mail\", \"tag\", \"tag2\", \"text\", \"md\", \"web_domain\", \"score\", \"deleted\", \"draft\", \"parent_id\", \"locked\", \"no_kill\" from \"publications\" where \"id\" =  $1"]
  ; this could be made more efficient, by querying the type_id with the publication query, and passing the result here.
  [type-query "select \"publication_type\" from \"publication_types\" where \"id\" = (select \"type_id\" from \"publications\" where \"id\" = $1);"]
  [votes-query "select \"vote_id\", \"username\", \"up\", \"num\" from \"publication_votes\" where \"id\" = $1;"]
  [+type
   (lambda (h)
     (let* ([id (hash-ref h 'id)]
            [maybe-type-id (query-maybe-row db-conn type-query id)])
       (if (not maybe-type-id)
           h
           (hash-set h 'type (vector-ref maybe-type-id 0)))))]
  [+id (lambda (h id) (hash-set h 'id id))]
  [otm-from-id-query (lambda (table value-column) (string-append "select \"" value-column "\" from \"" table "\" where \"id\" = $1;"))]
  [+otm
   (lambda (h table value-column json-key)
     (let* ([id (hash-ref h 'id)]
           [vals (query-rows db-conn (otm-from-id-query table value-column) id)]
           [vals-list (map (lambda (val) (vector-ref val 0)) vals)])
       (hash-set h json-key vals-list)))]
  [+cc (lambda (h) (+otm h "publication_cc" "username" 'cc))]
  [+community-tags (lambda (h) (+otm h "publication_community_tags" "tag" 'community_tag))]
  [+community-tagses (lambda (h) (+otm h "publication_community_tagses" "tag" 'community_tags))]
  [+search-text (lambda (h) (+otm h "publication_search_text" "word" 'search_text))]
  [+search-title (lambda (h) (+otm h "publication_search_title" "word" 'search_title))]
  [+search-url (lambda (h) (+otm h "publication_search_url" "word" 'search_url))]
  [+saved-by (lambda (h) (+otm h "publication_saved_by" "username" 'saved_by))]
  [+shared-by (lambda (h) (+otm h "publication_shared_by" "username" 'shared_by))]
  [+badged-by (lambda (h) (+otm h "publication_badged_by" "username" 'badged_by))]
  [+badged-kids (lambda (h) (+otm h "publication_badged_kids" "kid_id" 'badged_kids))]
  [+cubbed-by (lambda (h) (+otm h "publication_cubbed_by" "username" 'cubbed_by))]
  [+kids (lambda (h) (+otm h "publication_kids" "kid_id" 'kids))]
  [+votes
   (lambda (h)
     (let* ([id (hash-ref h 'id)]
           [votes (query-rows db-conn votes-query id)]
           [votes-list
            (map (lambda (vote)
                   (hasheq
                    'id (vector-ref vote 0)
                    'user (vector-ref vote 1)
                    'up (vector-ref vote 2)
                    'num (vector-ref vote 3)))
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
        (+search-url
        (+search-title
        (+search-text
        (+community-tagses
        (+community-tags
        (+cc
        (+type
        (+id (pub-vec->jsexpr maybe-pub-vec) id))))))))))))))))))

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
          (if (list? vals)
              (map (lambda (val)
                     (if (not (equal? val 'nil))
                         (db-insert-idval id val table val-column)
                         (void))
                     ) vals)
              void))]
    [save-cc (lambda (id ccs) (save-otm-pair id ccs "publication_cc" "username"))]
    [save-community-tags (lambda (id ctags) (save-otm-pair id ctags "publication_community_tags" "tag"))]
    [save-community-tagses (lambda (id ctagses) (save-otm-pair id ctagses "publication_community_tagses" "tag"))]
    [save-search-text (lambda (id lst) (save-otm-pair id lst "publication_search_text" "word"))]
    [save-search-title (lambda (id lst) (save-otm-pair id lst "publication_search_title" "word"))]
    [save-search-url (lambda (id lst) (save-otm-pair id lst "publication_search_url" "word"))]
    [save-votes
     (lambda (id votes)
       (let
           ([table "publication_votes"])
         (if (list? votes)
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
  [type-id-comment (get-type-id "story" db-conn)]
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
  [p-ctagses (safe-car (safe-car (val-if h 'ctags)))]
  [p-search-text (safe-car (safe-car (val-if h 'searchtext)))]
  [p-search-title (safe-car (safe-car (val-if h 'searchtitle)))]
  [p-search-url (safe-car (val-if h 'searchurl))]
  [p-votes (safe-car (val-if h 'votes))]
  [p-saved-by (safe-car (val-if h 'savedby))]
  [p-shared-by (safe-car (val-if h 'sharedby))]
  [p-badged-by (safe-car (val-if h 'badgedby))]
  [p-badged-kids (safe-car (val-if h 'badgedkids))]
  [p-cubbed-by (safe-car (val-if h 'cubbedby))]
  [p-kids (safe-car (val-if h 'kids))])
  ;; (write (string-append "db-save-publication " (number->string p-id) " starting")) (newline)
  ;; (write "type: ")      (write p-type) (newline)
  ;; (write "user: ")      (write p-username) (newline)
  ;; (write "time: ")      (write p-time) (newline)
  ;; (write "date: ")      (write p-date) (newline)
  ;; (write "url: ")       (write p-url) (newline)
  ;; (write "title: ")     (write p-title) (newline)
  ;; (write "mail: ")      (write p-mail) (newline)
  ;; (write "tag: ")       (write p-tag) (newline)
  ;; (write "tag2: ")      (write p-tag2) (newline)
  ;; (write "text: ")      (write p-text) (newline)
  ;; (write "md: ")        (write p-md) (newline)
  ;; (write "webdomain: ") (write p-webdomain) (newline)
  ;; (write "score: ")     (write p-score) (newline)
  ;; (write "deleted: ")   (write p-deleted) (newline)
  ;; (write "draft: ")     (write p-draft) (newline)
  ;; (write "parentid: ")  (write p-parent-id); (newline)
  ;; (write "locked: ")    (write p-locked) (newline)
  ;; (write "nokill: ")    (write p-no-kill) (newline)
  ;; \todo change to use upsert, when we update to postgresl 9.5
 (query-exec db-conn "insert into \"publications\" (id, type_id, username, time, date, url, title, mail, tag, tag2, text, md, web_domain, score, deleted, draft, parent_id, locked, no_kill) values ($1::integer, $2::integer, $3::text, $4::integer, $5::text, $6::text, $7::text, $8::boolean, $9::text, $10::text, $11::text, $12::text, $13::text, $14::integer, $15::boolean, $16::boolean, $17::integer, $18::boolean, $19::boolean);" p-id p-type p-username p-time p-date p-url p-title p-mail p-tag p-tag2 p-text p-md p-webdomain p-score p-deleted p-draft p-parent-id p-locked p-no-kill)
  (save-cc p-id p-cc)
  (save-community-tags p-id p-ctags)
  (save-community-tagses p-id p-ctagses)
  (save-search-text p-id p-search-text)
  (save-search-title p-id p-search-title)
  (save-search-url p-id p-search-url)
  (save-votes p-id p-votes)
  (save-saved-by p-id p-saved-by)
  (save-shared-by p-id p-shared-by)
  (save-badged-by p-id p-badged-by)
  (save-badged-kids p-id p-badged-kids)
  (save-cubbed-by p-id p-cubbed-by)
  (save-kids p-id p-kids)
  ;; (write "db-save-publication finished") (newline)
  (void)))
