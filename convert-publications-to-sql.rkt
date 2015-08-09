#lang racket

(require "db.rkt")
(require "db-publication.rkt")

; \todo make command line arg
(define filepath "")

; DOES NOT use transactions, to allow batching.
; Caller MUST transact.
(define (parse-file f)
  (let* ([absolute-path (build-path filepath f)]
         [file (open-input-file absolute-path)]
         [sexp (read file)])
    (db-save-publication-no-delete-no-transaction sexp)
    (close-input-port file)))

(define batch-size 10000)

(define (convert-publications-file-list l)
  (if (equal? l '()) (void)
      (let-values ([(h t) (if (<= (length l) batch-size)
                              (values l '())
                              (split-at l batch-size))])
        (db-transaction
         (lambda ()
           (map (lambda (f)
                  (parse-file f)) h)))
        (convert-publications-file-list t))))

(define (convert-publications pub-dir)
  (let ([files (directory-list pub-dir #:build? #f)])
    (convert-publications-file-list files)))

;  (define file (last (take files 1))) ; 233 is useful
;  (parse-file file)))

(convert-publications filepath)
