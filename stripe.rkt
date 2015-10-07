;; This file has been VERSIONED into the hub repo.
;; DO NOT change it from hub. Change in github.com/hubski/hubski,
;; and update the versioned file. Contributors WILL update this
;; file in hub without checking for changes.

#lang racket
(require json)
(require net/http-client)
(require net/uri-codec)
(require net/base64)
(require racket/port)
(require net/url)
(require "publications.rkt")

(provide
 stripe-charge
 )

(define (make-auth-header user pass)
  (string-append
   "Authorization: Basic "
   (bytes->string/utf-8
    (base64-encode
     (string->bytes/utf-8
      (string-append user ":" pass)) #""))))

;; Charges the given stripe token, by the given amount.
;; \param token the Stripe charge token, as returned by Stripe.js
;; \param the amount in USD cents to charge
;; \param key the Stripe API private key
;; \return #t if the charge was successful, #f if unknown failure, the error string if known failure
(define (stripe-charge key token amount user)
  (let-values
      ([(code headers response)
        (http-sendrecv
         "api.stripe.com" "/v1/charges"
         #:ssl? 'tls
         #:method "POST"
         #:headers (list
                    "Content-Type: application/x-www-form-urlencoded"
                    (make-auth-header key ""))
         #:data (alist->form-urlencoded
                 (list  (cons 'amount (number->string (inexact->exact (floor amount))))
                        (cons 'currency "usd")
                        (cons 'description (string-append "donation by " user))
                        (cons 'source token)))
         )])
    (let ([j (read-json response)])
      (cond [(not (hash? j)) (bool->arc-bool #f)]
            [(and (hash-has-key? j 'failure_message) (not (eq? (hash-ref j 'failure_message) 'null))) (hash-ref j 'failure_message)]
            [(and (hash-has-key? j 'error) (hash? (hash-ref j 'error)) (hash-has-key? (hash-ref j 'error) 'message)) (hash-ref (hash-ref j 'error) 'message)]
            [(not (hash-has-key? j 'paid)) (bool->arc-bool #f)]
            [else (bool->arc-bool (hash-ref j 'paid))]))))

;    (if (not (eq? code #"HTTP/1.1 200 OK")) (list code "T" token "T" (read-json response))
