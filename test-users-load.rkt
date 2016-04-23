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



;; (write "votes:")
;; (newline)
;; (write (hash-ref user 'votes))
;; (newline)
;; (write (hash-ref user 'followed))
;; (newline)

(write "modifying user") (newline)
(hash-set! user 'bio "yo yo")
(hash-set! user 'saved (list 42 'nil))

(write "saving user") (newline)
(db-save-user user)

(write "saved user")
(newline)

(define user2 (db-load-user "317"))
(write "user2:") (newline)
(write user2) (newline)

;(hash-for-each allusers (lambda (k v) (write k) (newline) (write v) (newline) (newline)))

(define user-id-list (db-get-users))

(write "num users:") (newline)
(write (length user-id-list)) (newline)
