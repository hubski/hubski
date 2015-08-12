#lang racket
(require json)

(provide
 jsexpr->pub-sexp
 pub-sexp->jsexpr
 publication-jsexpr?
 try-string->jsexpr
 )

(define (try-string->jsexpr s)
  (with-handlers ([exn:fail:read? (lambda (e) 'nil)]) (string->jsexpr s)))

(define (hash-ref-or-nil h memb)
  (if (hash-has-key? h memb) (hash-ref h memb) 'nil))

(define (null->nil v)
  (if (equal? v 'null) 'nil v))

(define (list-add-hash-member h memb sexp-memb l)
  (let ([val (null->nil (hash-ref-or-nil h memb))])
    (cons (list sexp-memb val) l)))

(define (list-add-hash-member-symbol h memb sexp-memb l)
  (let ([val (null->nil (hash-ref-or-nil h memb))])
    (cons (list sexp-memb (string->symbol val)) l)))

(define (bool->arc-bool b)
  (if b 't 'nil))

(define (list-add-hash-member-bool h memb sexp-memb l)
  (let ([val (null->nil (hash-ref-or-nil h memb))])
    (cons (list sexp-memb (bool->arc-bool val)) l)))

; returns a list of the given value. Used because Arc pub lists are doubled for some reason.
(define (list-add-hash-member-list h memb sexp-memb l)
  (let ([val (null->nil (hash-ref-or-nil h memb))])
    (if (or (equal? val 'nil) (equal? val '()))
        (cons (list sexp-memb 'nil) l)
        (cons (list sexp-memb (list (hash-ref h memb))) l))))

(define (vote-hash->list h publication-id)
  (list (hash-ref h 'id)
        publication-id
        (hash-ref h 'user)
        (if (hash-ref h 'up) 'up 'down)
        (hash-ref h 'num)))

(define (list-add-hash-member-votes h l)
  (let* ([id (hash-ref h 'id)]
        [memb 'votes]
        [sexp-memb 'votes]
        [votes (null->nil (hash-ref-or-nil h memb))]
        [votes-list (map (lambda (v) (vote-hash->list v id)) votes)])
    (cons (list 'votes votes-list) l)))

; \todo write a pipeline macro?
(define (jsexpr->pub-sexp-not-null j)
  (list-add-hash-member        j 'keys           'keys
  (list-add-hash-member        j 'parent_id      'parent
  (list-add-hash-member-bool   j 'draft          'draft
  (list-add-hash-member-bool   j 'deleted        'deleted
  (list-add-hash-member-list   j 'cubbed_by      'cubbedby
  (list-add-hash-member-list   j 'badged_kids    'badgedkids
  (list-add-hash-member-list   j 'saved_by       'savedby
  (list-add-hash-member-list   j 'community_tags 'ctags
  (list-add-hash-member-list   j 'community_tag  'ctag
  ; ptag?
  (list-add-hash-member-bool   j 'mail           'mail
  (list-add-hash-member-list   j 'cc             'cc
  (list-add-hash-member        j 'date           'date
  (list-add-hash-member        j 'tag2           'tag2
  (list-add-hash-member-list   j 'badged_by      'badgedby
  (list-add-hash-member-votes  j
  (list-add-hash-member        j 'id             'id
  (list-add-hash-member        j 'title          'title
  (list-add-hash-member        j 'text           'text
  (list-add-hash-member        j 'kids           'kids
  (list-add-hash-member-list   j 'shared_by      'sharedby
  (list-add-hash-member-list   j 'search_url     'searchurl
  (list-add-hash-member-list   j 'search_text    'searchtext
  (list-add-hash-member        j 'time           'time
  (list-add-hash-member-symbol j 'type           'type
  (list-add-hash-member        j 'md             'md
  (list-add-hash-member        j 'url            'url
  (list-add-hash-member-list   j 'domain         'domain
  (list-add-hash-member-list   j 'search_title   'searchtitle
  (list-add-hash-member        j 'tag            'tag
  (list-add-hash-member        j 'score          'score
  (list-add-hash-member        j 'user           'by
  (list-add-hash-member-bool   j 'no_kill        'nokill
  (list-add-hash-member-bool   j 'locked         'locked
  '()
  ))))))))))))))))))))))))))))))))))

(define (votes-list-list->votes-list-hash l)
  (if (list? l)
      (map (lambda (vote)
             (vote-list->hash vote)) l)
      l))

; we can't just use hash->list because the hash has member hashes which must be converted
(define (jsexpr->pub-sexp j)
  (if (equal? j 'null)
      'null
      (jsexpr->pub-sexp-not-null j)))

(define (safe-unlist l)
  (if (list? l)
      (car l)
      l))

(define (un-nil-list l)
  (if (and (list? l) (equal? (length l) 1) (equal? (car l) 'nil))
      '()
      l))

(define (nil->null v)
  (if (equal? v 'nil) 'null v))

(define (arc-bool->bool b)
  (if (equal? b 'nil) #f #t))

(define get-string (compose1
                    nil->null
                    safe-unlist
                    hash-ref-or-nil))
(define get-bool (compose1
                  arc-bool->bool
                  safe-unlist
                  hash-ref-or-nil))
(define get-list (compose1
                  nil->null
                  un-nil-list
                  hash-ref-or-nil))

(define (vote-list->hash vote)
  (let* ([vote-id (first vote)]
  ; sometimes vote has an IP in the middle, and is thus length 6, with user,up,num one farther in the list.
        [username (if (equal? (length vote) 5)
                      (third vote)
                      (fourth vote))]
        [up-atom (if (equal? (length vote) 5)
                      (fourth vote)
                      (fifth vote))]
        [up (equal? up-atom 'up)]
        [num (if (equal? (length vote) 5)
                  (fifth vote)
                  (sixth vote))])
    (hasheq
     'id vote-id
     'up up
     'user username
     'num num)))

(define (pub-sexp->jsexpr-not-null p)
  (let ([h (make-hash p)])
    (hasheq
     'keys           (get-string h 'keys)
     'parent_id      (get-string h 'parent)
     'draft          (get-bool h 'draft)
     'deleted        (get-bool h 'deleted)
     'cubbed_by      (get-list h 'cubbedby)
     'badged_kids    (get-list h 'badgedkids)
     'saved_by       (get-list h 'savedby)
     'community_tags (get-list h 'ctags)
     'community_tag  (get-list h 'ctag)
     ; ptag?   
     'mail           (get-bool h 'mail)
     'cc             (get-list h 'cc)
     'date           (get-string h 'date)
     'tag2           (get-string h 'tag2)
     'badged_by      (get-list h 'badgedby)
     'votes          (votes-list-list->votes-list-hash
                      (get-string h 'votes))
     'id             (get-string h 'id)
     'title          (get-string h 'title)
     'text           (get-string h 'text)
     'kids           (get-list h 'kids)
     'shared_by      (get-list h 'sharedby)
     'search_url     (get-string h 'searchurl)
     'search_text    (get-list h 'searchtext)
     'time           (get-string h 'time)
     'type           (symbol->string (get-string h 'type))
     'md             (get-string h 'mdn)
     'url            (get-string h 'url)
     'domain         (get-list h 'domain)
     'search_title   (get-string h 'searchtitle)
     'tag            (get-string h 'tag)
     'score          (get-string h 'score)
     'user           (get-string h 'by)
     'no_kill        (get-bool h 'nokill)
     'locked         (get-bool h 'locked)
     )))

(define (pub-sexp->jsexpr p)
  (if (or (equal? p 'null) (equal? p 'nil) (equal? p '()))
      'null
      (pub-sexp->jsexpr-not-null p)))

(define (publication-jsexpr-vote? v)
  (and (hash? v)
       (hash-has-key? v 'id)   (number? (hash-ref v 'id))
       (hash-has-key? v 'up)   (boolean? (hash-ref v 'up))
       (hash-has-key? v 'user) (string? (hash-ref v 'user))
       (hash-has-key? v 'num)  (number? (hash-ref v 'num))))

(define (publication-jsexpr-votes? l)
  (cond
    [(empty? l) #t]
    [(pair? l) (and (publication-jsexpr-vote? (first l)) (publication-jsexpr-votes? (rest l)))]
    [else #f]))

; \todo validate the types of values
(define (publication-jsexpr? p)
   (and (hash? p)
;;        (hash-has-key? p 'keys)
        (hash-has-key? p 'parent_id)
        (hash-has-key? p 'draft)
        (hash-has-key? p 'deleted)
        (hash-has-key? p 'cubbed_by)
        (hash-has-key? p 'badged_kids)
        (hash-has-key? p 'saved_by)
        (hash-has-key? p 'community_tags)
        (hash-has-key? p 'community_tag)
        ;; ; ptag?
        (hash-has-key? p 'mail)
        (hash-has-key? p 'cc)
        (hash-has-key? p 'date)
        (hash-has-key? p 'tag2)
        (hash-has-key? p 'badged_by)
        (hash-has-key? p 'votes)        (publication-jsexpr-votes? (hash-ref p 'votes))
        (hash-has-key? p 'id)
        (hash-has-key? p 'title)
        (hash-has-key? p 'text)
        (hash-has-key? p 'kids)
        (hash-has-key? p 'shared_by)
        (hash-has-key? p 'search_url)
        (hash-has-key? p 'search_text)
        (hash-has-key? p 'time)
        (hash-has-key? p 'type)
        (hash-has-key? p 'md)
        (hash-has-key? p 'url)
        (hash-has-key? p 'domain)
        (hash-has-key? p 'search_title)
        (hash-has-key? p 'tag)
        (hash-has-key? p 'score)
        (hash-has-key? p 'user)
        (hash-has-key? p 'no_kill)
        (hash-has-key? p 'locked)))
