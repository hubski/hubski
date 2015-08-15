#lang racket
(require json)
(require "db.rkt")
(require "db-publication.rkt")
(require "publication-client.rkt")
(require "publications.rkt")

; \todo make command line arg
(define filepath "")

(define (file-to-sexp f)
  (let* ([absolute-path (build-path filepath f)]
         [file (open-input-file absolute-path)]
         [sexp (read file)])
    (close-input-port file)
    sexp))

; Return whether b has all the keys of a.
; Does NOT check if a has all the keys of b. See hash-keys-equal?
(define (hash-keys-subset? a b)
  (letrec ([hash-subset-iter
         (lambda (a b i)
           (let ([key (hash-iterate-key a i)])
             (if (and (not (equal? key 'ip)) ; ip is in old pubs, but not stored in the db
                      (not (equal? key 'ptag)) ; "
                      (not (equal? key 'sockvotes)) ; "                      
                      (not (hash-has-key? b key)))
                 (let ()
                   (
                    (write "not has") (write key) (newline)   
                    #f))
                 (let ([next (hash-iterate-next a i)])
                   (if (equal? next #f)
                       #t
                       (hash-subset-iter a b next))))))])
    (if (hash-empty? a)
        #t
        (hash-subset-iter a b (hash-iterate-first a)))))

; return whether a and b have the same keys
(define (hash-keys-equal? a b)
  (and (hash-keys-subset? a b) (hash-keys-subset? b a)))

(define (votes-equal? al bl)
  (let* (
         ; sometimes a has an IP in the middle, and is thus length 6, with user,up,num one farther in the list.
         [a (car (car al))]
         [b (car (car bl))]
         [a-id (first a)]
         [a-user (if (equal? (length a) 5)
                       (third a)
                       (fourth a))]
         [a-up (if (equal? (length a) 5)
                      (fourth a)
                      (fifth a))]
         [a-num (if (equal? (length a) 5)
                  (fifth a)
                  (sixth a))]
         [b-id (first a)]
         [b-user (if (equal? (length b) 5)
                       (third b)
                       (fourth b))]
         [b-up (if (equal? (length b) 5)
                      (fourth b)
                      (fifth b))]
         [b-num (if (equal? (length b) 5)
                  (fifth b)
                  (sixth b))]         
         )
    (and (equal? a-id b-id) (equal? a-user b-user) (equal? a-up b-up) (equal? a-num b-num))))

(define (hash-values-subset-equal? key a b)
  (if (equal? key 'votes)
      (votes-equal? a b)
      (equal? a b)))

; Checks that all values in a match all values of the same key in b.
; Assumes (hash-keys-subset? a b).
; Does not check that values in b match a. Call (hash-values-subset? b a).
(define (hash-values-subset? a b)
  (letrec ([hash-values-iter
         (lambda (a b i)
           (let* ([key (hash-iterate-key a i)]
                 [a-val (hash-iterate-value a i)]
                 [b-val (hash-ref! b key 'nil)])
             (if (and (not (equal? key 'ip)) ; ip is in old pubs, but not stored in the db
                      (not (equal? key 'ptag)) ; "
                      (not (equal? key 'sockvotes)) ; "
                      (not (and (equal? key 'text) (equal? a-val '(t)))) ; for corrupt pub 78
                      (not (hash-values-subset-equal? key a-val b-val)))
                 (let ()
                   (
                    (write "not equal") (write key) (write " ") (write a-val) (write " ") (write "b-val") (newline)
                    #f))
                 (let ([next (hash-iterate-next a i)])
                   (if (equal? next #f)
                       #t
                       (hash-values-iter a b next))))))])
    (if (hash-empty? a)
        #t
        (hash-values-iter a b (hash-iterate-first a)))))

(define (sexp-hash-equal a b)
  (and (hash-keys-equal? a b) (hash-values-subset? a b)))

(define (hash-subset? a b)
  (and (hash-keys-subset? a b) (hash-values-subset? a b)))

(let* (
      [api-pub  (get-publication 78)]
;;      [api-pub      (jsexpr->pub-sexp (string->jsexpr api-pub-str))]
      [file-pub     (file-to-sexp "78")]
      [api-pub-h    (make-hash api-pub)]
      [file-pub-h   (make-hash file-pub)]
      )
  ;; (write "api-pub-string") (newline)
  ;; (write api-pub-str) (newline)
  ;; (newline)  
  (write "api-pub") (newline)
  (write api-pub) (newline)
  (newline)
  (write "file-pub") (newline)
  (write file-pub) (newline)
  (newline)  
  (write "equal? ") (write (hash-subset? file-pub-h api-pub-h)) (newline)
  )

(newline)