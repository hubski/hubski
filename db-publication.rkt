;; This file has been VERSIONED into the hub repo.
;; DO NOT change it from hub. Change in github.com/hubski/hubski,
;; and update the versioned file. Contributors WILL update this
;; file in hub without checking for changes.

 #lang racket
 (require db)
 (require racket/trace)
 (require "db.rkt")

(provide
 db-save-publication
 db-load-publication
 db-get-publications
 db-get-publications-public 
 db-save-publication-no-delete-no-transaction ; used by the conversion script, for speed
 db-has-community-tag?
 db-set-user-community-tag
 db-get-commonest-community-tag
 list->arc-list
 arc-list->list 
 )

(define (improper-list->list l)
  (letrec ([acc
            (lambda (proper improper)
              (if (not (pair? improper))
                  proper
                  (acc (append proper (list (car improper))) (cdr improper))))])
    (acc '() l)))

;; Takes an Arc list, which is nested pairs terminated with 'nil,
;; and returns a proper Racket list, which is a series of pairs
;; terminated with '().
;; If the given value is not a list, it is simply returned.
;; If the given list is proper, it is simply returned.
;; If the given list is improper, but not terminated with 'nil,
;; it is returned as a proper list but not stripped of the final value.
;; In other words, this function is 'safe' UNLESS the list is an
;; improper list terminated by a 'nil which represents nil, not
;; an Arc list. BE AWARE - it is not possible to detect this situation!
;; This is horrifically inefficient, but hopefully won't be necessary long.
(define (arc-list->list l)
  (if (or (list? l) (not (pair? l))) l
      (let ([properlist (improper-list->list l)])
        (if (not (equal? (last properlist) 'nil))
            properlist
            (take properlist (- (length properlist) 1))))))

;; Horribly inefficient. See arc-list->list comment.
(define (list->arc-list l)
  (if (null? (cdr l))
      (cons (car l) 'nil)
      (cons (car l) (list->arc-list (cdr l)))))

(define query-has-community-tag?
  (virtual-statement "select count(1) from publication_community_tagses where id = $1::integer and username = $2::text;"))
(define (db-has-community-tag? publication-id username)
  (not (equal? 0 (query-value db-conn query-has-community-tag? publication-id username))))

(define query-get-commonest-community-tag
  (virtual-statement "select tag, count(tag) from publication_community_tagses where id = $1::integer group by tag order by count(tag) desc limit 1;"))
;; \return A list, of the most common community tag, and the count.
;;         If the publication has no ctags, returns ("" 0).
(define (db-get-commonest-community-tag publication-id)
  ;; (write-to-file "db-get-commonest-community-tag" "debug" #:exists 'append)
  ;; (write-to-file publication-id "debug" #:exists 'append)
  ;; (write-to-file "\n" "debug" #:exists 'append)
  (let ([maybe-row (query-maybe-row db-conn query-get-commonest-community-tag publication-id)])
    (if (equal? maybe-row #f)
;;        (begin (write-to-file " maybe row false!\n" "debug" #:exists 'append)
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

(define (safe-car l)
  (if (not (pair? l)) l (car l)))

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
    ;; (write-to-file "db-insert-idval" "debug" #:exists 'append)
    ;; (write-to-file id "debug" #:exists 'append)
    ;; (write-to-file " " "debug" #:exists 'append)
    ;; (write-to-file val "debug" #:exists 'append)
    ;; (write-to-file " " "debug" #:exists 'append)
    ;; (write-to-file table "debug" #:exists 'append)
    ;; (write-to-file " " "debug" #:exists 'append)
    ;; (write-to-file val-column "debug" #:exists 'append)
    ;; (write-to-file "\n" "debug" #:exists 'append)
    (query-exec db-conn query id val)))

  ; used for deleting one-to-many table ids before insertion
(define (db-delete-from table id)
  (let ([query (string-append "delete from \"" table "\" where \"id\" = $1::integer;")])
    ;; (write-to-file "deleting " "debug" #:exists 'append)
    ;; (write-to-file table "debug" #:exists 'append)
    ;; (write-to-file " " "debug" #:exists 'append)
    ;; (write-to-file id "debug" #:exists 'append)
    ;; (write-to-file "\n" "debug" #:exists 'append)
    (query-exec db-conn query id)))

(define (db-save-publication sexp)
  (db-transaction
   (lambda () (db-save-publication-no-transaction sexp))))

(define (save-votes id votes)
  (let ([table "publication_votes"])
    ;; (write-to-file "save-votes " "debug" #:exists 'append)
    ;; (write-to-file id "debug" #:exists 'append)
    ;; (write-to-file " " "debug" #:exists 'append)
    ;; (write-to-file votes "debug" #:exists 'append)
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
    
;; \todo fix to delete from otm tables
(define (db-save-publication-no-transaction sexp)
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
         ;; (write-to-file "save-otm-pair " "debug" #:exists 'append)
         ;; (write-to-file id "debug" #:exists 'append)
         ;; (write-to-file " " "debug" #:exists 'append)
         ;; (write-to-file vals "debug" #:exists 'append)
         ;; (write-to-file " " "debug" #:exists 'append)
         ;; (write-to-file valid-vals "debug" #:exists 'append)
         ;; (write-to-file "\n" "debug" #:exists 'append)
         (if (or (not (save-valid? id)) (empty? valid-vals)) void
             (begin
               (db-delete-from table id)
               (save-otm-pair-no-delete id valid-vals table val-column)))))]
    [save-cc (lambda (id ccs) (save-otm-pair id ccs "publication_cc" "username"))]
    [save-community-tags (lambda (id ctags) (save-otm-pair id ctags "publication_community_tags" "tag"))]       
    [save-search-text (lambda (id lst) (save-otm-pair id lst "publication_search_text" "word"))]
    [save-search-title (lambda (id lst) (save-otm-pair id lst "publication_search_title" "word"))]
    [save-search-url (lambda (id lst) (save-otm-pair id lst "publication_search_url" "word"))]
  [save-saved-by (lambda (id lst) (save-otm-pair id lst "publication_saved_by" "username"))]
  [save-shared-by (lambda (id lst) (save-otm-pair id lst "publication_shared_by" "username"))]
  [save-badged-by (lambda (id lst) (save-otm-pair id lst "publication_badged_by" "username"))]
  [save-badged-kids (lambda (id lst) (save-otm-pair id lst "publication_badged_kids" "kid_id"))]
  [save-cubbed-by (lambda (id lst) (save-otm-pair id lst "publication_cubbed_by" "username"))]
  [save-kids  (lambda (id lst) (save-otm-pair id lst "publication_kids" "kid_id"))]
  ;; [save-kid   (lambda (id kid)
  ;;               (let* ([table "publication_kids"]
  ;;                      [column "kid_id"]
  ;;                      [delete-query (string-append "delete from \"" table "\" where \"id\" = $1::integer and \"" column "\" = $2::integer;")])
  ;;                 (if (and (save-valid? id) (save-valid? kid))
  ;;                          (query-exec db-conn delete-query id kid)
  ;;                          void)
  ;;                 (save-otm-pair-no-delete id (list kid) table column)))] ; DOES NOT delete other existing children
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
  [p-md (sqlnil (safe-car (val-if h 'md)))]
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
  [p-search-text (arc-list->list (sqlnil  (safe-car (val-if h 'searchtext))))]
  [p-search-title (arc-list->list (sqlnil (safe-car (val-if h 'searchtitle))))]
  [p-search-url (arc-list->list (sqlnil (val-if h 'searchurl)))]
  [p-votes (arc-list->list (sqlnil     (val-if h 'votes)))]
  [p-saved-by (arc-list->list (sqlnil  (val-if h 'savedby)))]
  [p-shared-by (arc-list->list (sqlnil (val-if h 'sharedby)))]
  [p-badged-by (arc-list->list (sqlnil (val-if h 'badgedby)))]
  [p-badged-kids (arc-list->list (sqlnil (val-if h 'badgedkids)))]
  [p-cubbed-by (arc-list->list (sqlnil (val-if h 'cubbedby)))]
  [p-kids (arc-list->list (sqlnil (val-if h 'kids)))])
 ;; (write-to-file (string-append "db-save-publication "  (number->string p-id) " starting") "debug" #:exists 'append) (write-to-file "\n" "debug" #:exists 'append)
 ;; (write-to-file "type: " "debug" #:exists 'append)      (write-to-file p-type "debug" #:exists 'append) (write-to-file "\n" "debug" #:exists 'append)
 ;; (write-to-file "user: " "debug" #:exists 'append)      (write-to-file p-username "debug" #:exists 'append) (write-to-file "\n" "debug" #:exists 'append)
 ;; (write-to-file "time: " "debug" #:exists 'append)      (write-to-file p-time "debug" #:exists 'append) (write-to-file "\n" "debug" #:exists 'append)
 ;; (write-to-file "date: " "debug" #:exists 'append)      (write-to-file p-date "debug" #:exists 'append) (write-to-file "\n" "debug" #:exists 'append)
 ;; (write-to-file "url: " "debug" #:exists 'append)       (write-to-file p-url "debug" #:exists 'append) (write-to-file "\n" "debug" #:exists 'append)
 ;; (write-to-file "title: " "debug" #:exists 'append)     (write-to-file p-title "debug" #:exists 'append) (write-to-file "\n" "debug" #:exists 'append)
 ;; (write-to-file "mail: " "debug" #:exists 'append)      (write-to-file p-mail "debug" #:exists 'append) (write-to-file "\n" "debug" #:exists 'append)
 ;; (write-to-file "tag: " "debug" #:exists 'append)       (write-to-file p-tag "debug" #:exists 'append) (write-to-file "\n" "debug" #:exists 'append)
 ;; (write-to-file "tag2: " "debug" #:exists 'append)      (write-to-file p-tag2 "debug" #:exists 'append) (write-to-file "\n" "debug" #:exists 'append)
 ;; (write-to-file "text: " "debug" #:exists 'append)      (write-to-file p-text "debug" #:exists 'append) (write-to-file "\n" "debug" #:exists 'append)
 ;; (write-to-file "md: " "debug" #:exists 'append)        (write-to-file p-md "debug" #:exists 'append) (write-to-file "\n" "debug" #:exists 'append)
 ;; (write-to-file "webdomain: " "debug" #:exists 'append) (write-to-file p-webdomain "debug" #:exists 'append) (write-to-file "\n" "debug" #:exists 'append)
 ;; (write-to-file "score: " "debug" #:exists 'append)     (write-to-file p-score "debug" #:exists 'append) (write-to-file "\n" "debug" #:exists 'append)
 ;; (write-to-file "deleted: " "debug" #:exists 'append)   (write-to-file p-deleted "debug" #:exists 'append) (write-to-file "\n" "debug" #:exists 'append)
 ;; (write-to-file "draft: " "debug" #:exists 'append)     (write-to-file p-draft "debug" #:exists 'append) (write-to-file "\n" "debug" #:exists 'append)
 ;; (write-to-file "parentid: " "debug" #:exists 'append)  (write-to-file p-parent-id "debug" #:exists 'append) (write-to-file "\n" "debug" #:exists 'append)
 ;; (write-to-file "locked: " "debug" #:exists 'append)    (write-to-file p-locked "debug" #:exists 'append) (write-to-file "\n" "debug" #:exists 'append)
 ;; (write-to-file "nokill: " "debug" #:exists 'append)    (write-to-file p-no-kill "debug" #:exists 'append) (write-to-file "\n" "debug" #:exists 'append)
 ;; (write-to-file "ctags: " "debug" #:exists 'append)     (write-to-file p-ctags "debug" #:exists 'append) (write-to-file "\n" "debug" #:exists 'append)
 ;; (write-to-file "votes: " "debug" #:exists 'append)     (write-to-file p-votes "debug" #:exists 'append) (write-to-file "\n" "debug" #:exists 'append)
 ;; (write-to-file "savedby: " "debug" #:exists 'append)   (write-to-file p-saved-by "debug" #:exists 'append) (write-to-file "\n" "debug" #:exists 'append)
 ;; (write-to-file "sharedby: " "debug" #:exists 'append)  (write-to-file p-shared-by "debug" #:exists 'append) (write-to-file "\n" "debug" #:exists 'append)
 ;; (write-to-file "badgedby: " "debug" #:exists 'append)  (write-to-file p-badged-by "debug" #:exists 'append) (write-to-file "\n" "debug" #:exists 'append)
 ;; (write-to-file "badgedkids: " "debug" #:exists 'append)(write-to-file p-badged-kids "debug" #:exists 'append) (write-to-file "\n" "debug" #:exists 'append)
 ;; (write-to-file "cubbedby: " "debug" #:exists 'append)  (write-to-file p-cubbed-by "debug" #:exists 'append) (write-to-file "\n" "debug" #:exists 'append)
 ;; (write-to-file "kids: " "debug" #:exists 'append)      (write-to-file p-kids "debug" #:exists 'append) (write-to-file "\n\n" "debug" #:exists 'append)
  
  ;; MUST use a transaction. Because we delete-then-insert, removing the transaction would be a race condition.
  (query-exec db-conn "BEGIN;") 
  ;; \todo change to use upsert, when we update to postgresl 9.5
  (query-exec db-conn "delete from \"publications\" where \"id\" = $1::integer;" p-id)
 (query-exec db-conn "insert into \"publications\" (id, type_id, username, time, date, url, title, mail, tag, tag2, text, md, web_domain, score, deleted, draft, parent_id, locked, no_kill) values ($1::integer, $2::integer, $3::text, $4::integer, $5::text, $6::text, $7::text, $8::boolean, $9::text, $10::text, $11::text, $12::text, $13::text, $14::integer, $15::boolean, $16::boolean, $17::integer, $18::boolean, $19::boolean);" p-id p-type p-username p-time p-date p-url p-title p-mail p-tag p-tag2 p-text p-md p-webdomain p-score p-deleted p-draft p-parent-id p-locked p-no-kill)
  (save-cc p-id p-cc)  
  (save-community-tags p-id p-ctags)  
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
  ;; (save-kid p-parent-id p-id)
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

(define by-id-query  (virtual-statement "select \"username\", \"time\", \"date\", \"url\", \"title\", \"mail\", \"tag\", \"tag2\", \"text\", \"md\", \"web_domain\", \"score\", \"deleted\", \"draft\", \"parent_id\", \"locked\", \"no_kill\" from \"publications\" where \"id\" =  $1"))
; this could be made more efficient, by querying the type_id with the publication query, and passing the result here.
(define type-query  (virtual-statement "select \"publication_type\" from \"publication_types\" where \"id\" = (select \"type_id\" from \"publications\" where \"id\" = $1);"))
(define votes-query (virtual-statement "select \"vote_id\", \"username\", \"up\", \"num\" from \"publication_votes\" where \"id\" = $1;"))
(define (otm-from-id-query table value-column) (virtual-statement (string-append "select \"" value-column "\" from \"" table "\" where \"id\" = $1;")))
(define cc-query             (otm-from-id-query "publication_cc"             "username"))
(define community-tags-query (otm-from-id-query "publication_community_tags" "tag"))
(define search-text-query    (otm-from-id-query "publication_search_text"    "word"))
(define search-title-query   (otm-from-id-query "publication_search_title"   "word"))
(define search-url-query     (otm-from-id-query "publication_search_url"     "word"))
(define saved-by-query       (otm-from-id-query "publication_saved_by"       "username"))
(define shared-by-query      (otm-from-id-query "publication_shared_by"      "username"))
(define badged-by-query      (otm-from-id-query "publication_badged_by"      "username"))
(define badged-kids-query    (otm-from-id-query "publication_badged_kids"    "kid_id"))
(define cubbed-by-query      (otm-from-id-query "publication_cubbed_by"      "username"))
(define kids-query           (otm-from-id-query "publication_kids"           "kid_id"))

(define (db-load-publication id)
  (let*
  ([+type
     (lambda (h)
     (let* ([id (hash-ref h 'id)]
            [maybe-type-id (query-maybe-row db-conn type-query id)])
       (if (not maybe-type-id)
           h
           (hash-set h 'type (vector-ref maybe-type-id 0)))))]
  [+id (lambda (h id) (hash-set h 'id id))]
  [+otm
   (lambda (h stmt json-key)
     (let* ([id (hash-ref h 'id)]
           [vals (query-rows db-conn stmt id)]
           [vals-list (map (lambda (val) (vector-ref val 0)) vals)])
       (hash-set h json-key vals-list)))]
  [+cc (lambda (h) (+otm h cc-query 'cc))]
  [+community-tags (lambda (h) (+otm h community-tags-query 'community_tag))]
  [+search-text (lambda (h) (+otm h search-text-query 'search_text))]
  [+search-title (lambda (h) (+otm h search-title-query 'search_title))]
  [+search-url (lambda (h) (+otm h search-url-query 'search_url))]
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
        (+community-tags
        (+cc
        (+type
        (+id (pub-vec->jsexpr maybe-pub-vec) id)))))))))))))))))

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
    [save-search-text (lambda (id lst) (save-otm-pair id lst "publication_search_text" "word"))]
    [save-search-title (lambda (id lst) (save-otm-pair id lst "publication_search_title" "word"))]
    [save-search-url (lambda (id lst) (save-otm-pair id lst "publication_search_url" "word"))]
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
  (void)))
