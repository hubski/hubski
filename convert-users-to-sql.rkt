#lang racket

(require "db.rkt")
(require "db-users.rkt")

; \todo make command line arg
(define filepath "/home/rob/sync/lang/arc/hub/arc/news/profile/")

; DOES NOT use transactions, to allow batching.
; Caller MUST transact.
(define (parse-file f)
  (let* ([absolute-path (build-path filepath f)]
         [file (open-input-file absolute-path)]
         [sexp (read file)])
    (db-save-user-no-delete-no-transaction sexp)
    (close-input-port file)))

(define batch-size 10000)

(define (convert-file-list l)
  (if (equal? l '()) (void)
      (let-values ([(h t) (if (<= (length l) batch-size)
                              (values l '())
                              (split-at l batch-size))])
        (db-transaction
         (lambda ()
           (map (lambda (f)
                  (parse-file f)) h)))
        (convert-file-list t))))

(define (convert-users pub-dir)
  (let ([files (directory-list pub-dir #:build? #f)])
    (convert-file-list files)))
 ;; (define file (last (take files 233))) ; 233 is useful
 ;; (parse-file file)))

(convert-users filepath)
