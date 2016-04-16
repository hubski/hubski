#lang racket

(require "db.rkt")
(require "db-users.rkt")

;; (write (db-get-user "rob05c"))
(define allusers (db-load-all-users))

;; (for/hash ([(id user) (in-hash allusers)])
;;   (for/hash ([(k v) (in-hash user)])
;;     (begin (write v) (newline))
;;     ))


;; (for/hash ([(id user) (in-hash allusers)])
;;   ;; (begin
;;   ;;   (write id)
;;   ;;   (newline)
;;   ;;   (write user)
;;   ;;   (newline)
;;   ;;   (newline))
;;   (values id user))


(define user (hash-ref allusers "317"))

(write "loaded user 317:") (newline)
(print-hash-table #t)
(write user) (newline)


(write "saving user") (newline)

(write "votes:")
(newline)
(write (hash-ref user 'votes))
(newline)
(write (hash-ref user 'followed))
(newline)

(hash-set! user 'bio "yo yo")
(hash-set! user 'saved (list 42 'nil))
(db-save-user user)

(write "saved user")
(newline)

;(hash-for-each allusers (lambda (k v) (write k) (newline) (write v) (newline) (newline)))
