#lang racket

(require "db.rkt")
(require "db-publication.rkt")

; \todo make command line arg
(define filepath "")

; \todo use transaction, for atomicity
(define (parse-file f)
  (let* ([absolute-path (build-path filepath f)]
         [file (open-input-file absolute-path)]
         [sexp (read file)])
    (db-save-publication-no-delete sexp)
    (close-input-port file)))

(define (convert-publications pub-dir)
  (let ([files (directory-list pub-dir #:build? #f)])
    (map (lambda (f)
           (parse-file f)) files)))
;  (define file (last (take files 1))) ; 233 is useful
;  (parse-file file)))

(convert-publications filepath)
