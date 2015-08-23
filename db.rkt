;; This file has been VERSIONED into the hub repo.
;; DO NOT change it from hub. Change in github.com/hubski/hubski,
;; and update the versioned file. Contributors WILL update this
;; file in hub without checking for changes.

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
