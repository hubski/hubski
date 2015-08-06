#lang web-server
(require web-server/dispatch)
(require web-server/servlet-env)
(require db)
(require json)
(require "db.rkt")
(require "publications.rkt")
(require "db-publication.rkt")

; \todo make command line arg
(define port 8003)

; creates an OK HTTP response, of text, from the given string.
(define (make-response-string str)
  (response
   200 #"OK"
   (current-seconds) TEXT/HTML-MIME-TYPE
   empty
   (λ (op) (write-string str op))))

(define (serve-api-info req)
  (let ([api-info-json
         (hasheq 'endpoints
                 '("/api/publications" "/api/publication/{id}")
                 )
         ])
    (make-response-string (jsexpr->string api-info-json))))

(define (jsexpr-err msg)
  (hasheq 'result "error" 'message msg))

(define (serve-api-err req)
  (make-response-jsexpr (jsexpr-err (string-append "invalid endpoint: " (url->string (request-uri req))))))

(define (make-response-jsexpr jsexpr)
  (make-response-string (jsexpr->string jsexpr)))

(define (serve-publication-post req id)
  (let* ([data (request-post-data/raw req)]
         [str (if (equal? data #f) 'nil (bytes->string/utf-8 data))]
         [js (if (equal? str 'nil) 'nil (try-string->jsexpr str))]
         [pub (if (not (publication-jsexpr? js)) 'nil (jsexpr->pub-sexp js))]
         [save-pub (lambda (sexp)
                     (db-save-publication sexp)
                     (make-response-jsexpr (hasheq 'result "success")))])
    (if (equal? pub 'nil)
        (make-response-jsexpr (jsexpr-err (string-append "invalid POST: " str)))
        (save-pub pub))))

(define (serve-publication req id)
  (make-response-jsexpr (db-load-publication id)))

(define (serve-publications req)
  (make-response-jsexpr (db-get-publications)))

(define-values (blog-dispatch blog-url)
  (dispatch-rules
   [("") serve-api-info]
   [("api") #:method "get" serve-api-info]
   [("api" "publication" (integer-arg)) #:method "get" serve-publication]
   [("api" "publications") #:method "get" serve-publications]
   [("api" "publication" (integer-arg)) #:method "post" serve-publication-post]
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
