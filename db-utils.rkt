#lang racket
(require db)
(require "db.rkt")

(provide
 val-if
 safe-member
 string-or-nil
 sqlnil
 from-sqlnil
 db-insert-idval
 db-delete-from
 safe-car
 sql-null->null
 otm-all-query
 otm-all-query-val-ascending
 save-otm-pair
 +otm-all
 )

; gets the hash key if it exists, else 'nil
(define (val-if h key)
  (if (hash-has-key? h key) (hash-ref h key) 'nil))

; like (member) but returns #t instead of the list, and #f if l isn't a list
(define (safe-member l val)
  (and (list? l) (not (equal? (member val l) #f))))

(define (string-or-nil val)
  (if (string? val) val 'nil))

; converts 'nil to sql-null. Anything else is returned without modification
(define (sqlnil val)
  (if (equal? val 'nil) sql-null val))

(define (from-sqlnil val)
  (if (equal? val sql-null) 'nil val))

; many of the one-to-many tables have a single key and string.
; this lets us insert into them easily.
(define (db-insert-idval id val table val-column)
  (let ([query (string-append "insert into \"" table "\" (id, \"" val-column "\") values ($1, $2);")])
    (query-exec db-conn query id val)))

  ; used for deleting one-to-many table ids before insertion
(define (db-delete-from table id)
  (let ([query (string-append "delete from \"" table "\" where \"id\" = $1;")])
    (query-exec db-conn query id)))

(define (safe-car l)
  (if (not (pair? l)) l (car l)))

; if val is sql-null, returns 'null, else val
(define (sql-null->null val)
  (if (equal? val sql-null) 'null val))

(define (otm-all-query table value-column) (virtual-statement (string-append "select id, \"" value-column "\" from \"" table "\";")))

(define (otm-all-query-val-ascending table value-column) (virtual-statement (string-append "select id, \"" value-column "\" from \"" table "\" order by \"" value-column "\" asc;")))

(define (otm-delete-query table) (virtual-statement (string-append "delete from \"" table "\" where id = $1;")))

(define (save-otm-pair id vals table val-column)
  (query-exec db-conn (otm-delete-query table) id)
  (if (pair? vals)
      (map (lambda (val)
             (if (not (equal? val 'nil))
                 (db-insert-idval id val table val-column)
                 (void))
             ) vals)
      void))

(define (+otm-all h stmt json-key)
  (let* ([vals (query-rows db-conn stmt)])
    (for-each
     (lambda (v)
       (let* ([row-id (vector-ref v 0)]
              [pub-h (hash-ref! h row-id (make-hash))]
              [pub-otm-list (hash-ref! pub-h json-key '())]
              [otm-val (vector-ref v 1)]
              [new-otm-list (cons otm-val pub-otm-list)])
         (hash-set! pub-h json-key new-otm-list)
         (hash-set! h row-id pub-h) ; \todo determine if necessary
         ))
     vals)
    h))
