#lang web-server
(require web-server/dispatch)
(require web-server/servlet-env)
(require db)
(require json)
(require "db.rkt")
(require "publications.rkt")
(require "db-publication.rkt")

;; \todo make command line arg
(define port 8003)

;; creates an OK HTTP response, of text, from the given string.
(define (make-response-string str)
  (response
   200 #"OK"
   (current-seconds) #"application/json; charset=utf-8"
   empty
   (λ (op) (write-string str op))))

;; \todo get endpoints dynamically. Remove /api/ ?
(define (serve-api-info req)
  (let ([api-info-json
         (hasheq 'endpoints
                 '("/publications" "/publication/{id}")
                 )
         ])
    (make-response-string (jsexpr->string api-info-json))))

(define (jsexpr-err msg)
  (hasheq 'result "error" 'message msg))

(define (serve-api-err req)
  (make-response-jsexpr (jsexpr-err (string-append "invalid endpoint: " (url->string (request-uri req))))))

(define (make-response-jsexpr jsexpr)
  (make-response-string (jsexpr->string jsexpr)))

(define (serve-publication req id)
  (let ([p (db-load-publication id)])
    (if (or (equal? p 'null) (publication-is-public p))
        (make-response-jsexpr p)
        (make-response-jsexpr 'null))))

(define (serve-publication-tree req id)
  (make-response-jsexpr (db-get-publication-recursive-public id)))

(define (serve-publications req)
  (make-response-jsexpr (db-get-publications-public)))

(define-values (blog-dispatch blog-url)
  (dispatch-rules
   [("publication" (integer-arg)) #:method "get" serve-publication]
   [("publication" (integer-arg) "tree") #:method "get" serve-publication-tree]
   [("publications") #:method "get" serve-publications]
   [else serve-api-info]))

(define (start req)
  (start
   (send/suspend
    (lambda (k-url)
      (blog-dispatch req)))))

;(write (string-append "serving on " (number->string port)))
(serve/servlet start
               #:stateless?    #t
               #:port          port
               #:listen-ip     #f ; listen for external connections
               #:command-line? #t ; don't open a web browser on start
;               #:servlet-path "/"
               #:servlet-regexp #rx""
               )
