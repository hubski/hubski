#lang racket
(require db)
(require "db.rkt")

(provide
 list->arc-list
 arc-list->list
 bool->arc-bool
 arc-bool->bool 
 )

(define (bool->arc-bool b)
  (if b 't 'nil))

(define (arc-bool->bool b)
  (if (equal? b 'nil) #f #t))

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
