#lang racket

(require "db.rkt")
(require "db-publication.rkt")

;; (write (db-get-user "rob05c"))
(define allusers (db-load-all-users))
(write allusers)
(newline)
