;;
;; This file is copied from github.com/hubski/hubski to the hubski Arc repo.
;;
;; Changes MUST be made in the github.com/hubski/hubski repo and copied to Arc.
;; Changes MUST NOT be made to this file in Arc without copying them verbatim to github.com/hubski/hubski
;; This file SHALL be overwritten in Arc from github.com/hubski/hubski without checking for uncopied changes.
;;
;; Eventually, the Arc hub and github.com/hubski/hubski repos will converge.
;; Until then, copying is the easiest way to manage two repos; there should be no reason to modify this file from the Arc repo.
;;

#lang racket
(require net/http-client)
(require json)
(require "publications.rkt")

(provide
 get-publication
 set-publication
 )

(define data-host "localhost")
(define data-port 8003)
(define pub-uri "/api/publication/")

(define rest-conn-internal 'nil)
(define (rest-conn)
  (define (get-and-set-rest-conn)
    (set! rest-conn-internal (http-conn-open
                              data-host
                              #:port data-port))
    rest-conn-internal)  
  (if (and (http-conn? rest-conn-internal) (http-conn-live? rest-conn-internal))
      rest-conn-internal
      (get-and-set-rest-conn)))
          
(define (get-publication id)
  (define-values (status-line headers response)
    (http-conn-sendrecv!
     (rest-conn)
     (string-append pub-uri (number->string id))))
  (define str (read-json response))
  (define pub (jsexpr->pub-sexp str))
;;  (define pub-jsexpr (pub-sexp->jsexpr pub))
;;  (define pub-json-string (jsexpr->string pub-jsexpr))
  pub)

(define (set-publication pub)
  (define js (pub-sexp->jsexpr pub))
  (define id (hash-ref js 'id))  
  (define js-str (jsexpr->string js))
  (define-values (status-line headers response)
    (http-conn-sendrecv!
     (rest-conn)
     (string-append pub-uri (number->string id))
     #:method #"POST"
     #:data js-str))
  (define response-js (try-string->jsexpr response))
  (and
   (hash? response-js)
   (equal? (hash-has-key? response-js 'result))
   (equal? (hash-ref response-js 'result) "success")))
