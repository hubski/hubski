#lang racket
(require db)

(provide
 db-conn
 db-transaction
 )

; \todo make command line args
(define db-user     "")
(define db-pass     "")
(define db-database "")
(define db-server   "")

(define db-conn
  (virtual-connection
   (connection-pool
    (lambda () (postgresql-connect
           #:user db-user
           #:password db-pass
           #:database db-database
           #:server db-server
           )))))

(define (db-transaction f)
  (query-exec db-conn "BEGIN;")
  (f)
  (query-exec db-conn "COMMIT;"))
