#lang racket/base
(require "utils.rkt"
         (contract-req)
         racket/match
         (for-syntax racket/base racket/match))

;; Lightweight variant of sets

(provide hset hseteq hseteqv
         hset?
         hset-empty?
         hset-member?
         hset-count
         hset-add
         hset-remove
         hset-first
         hsubset?
         hset-overlap?
         hset=?
         hset-subtract
         hset-union
         hset-intersect
         hset-partition
         hset-map
         hset-filter
         hset-for-each
         hset->list
         list->hset
         list->hseteq
         for/hset
         for/hseteq
         for/hseteqv
         for*/hset
         for*/hseteq
         in-hset)

(provide-for-cond-contract hsetof)

(define-for-cond-contract (hsetof c) (hash/c c #t #:immutable #t #:flat? #t))

(define build-hset
  (case-lambda
    [() #hash()]
    [l (for/fold ([s #hash()]) ([e (in-list l)])
         (hash-set s e #t))]))


(define hset
  (case-lambda
    [() #hash()]
    [l (for/fold ([s #hash()]) ([e (in-list l)])
         (hash-set s e #t))]))

(define hseteq
  (case-lambda
    [() #hasheq()]
    [l (for/fold ([s #hasheq()]) ([e (in-list l)])
         (hash-set s e #t))]))

(define (hseteqv)
  (case-lambda
    [() #hasheqv()]
    [l (for/fold ([s #hasheqv()]) ([e (in-list l)])
         (hash-set s e #t))]))

(define (hset? s) (hash? s))

(define (hset-empty? s) (zero? (hash-count s)))
(define (hset-member? s e) (hash-ref s e #f))
(define (hset-count s) (hash-count s))

(define (hset-add s e) (hash-set s e #t))
(define (hset-remove s e) (hash-remove s e))
(define (hset-first s) (hash-iterate-key s (hash-iterate-first s)))

(define-syntax in-hset (make-rename-transformer #'in-immutable-hash-keys))

(define (hsubset? s1 s2)
  (hash-keys-subset? s1 s2))

(define (hset-overlap? s1 s2)
  (if ((hset-count s1) . < . (hset-count s2))
      (hset-overlap? s2 s1)
      (for/or ([k (in-hset s2)])
        (hset-member? s1 k))))

(define (hset=? s1 s2)
  (or (eq? s1 s2)
      (and (= (hash-count s1) (hash-count s2))
           (hash-keys-subset? s1 s2))))

(define (hset-subtract s1 s2)
  (for/fold ([s1 s1]) ([k (in-hset s2)])
    (hash-remove s1 k)))

(define (hset-union s1 s2)
  (if ((hset-count s1) . < . (hset-count s2))
      (hset-union s2 s1)
      (for/fold ([s1 s1]) ([k (in-hset s2)])
        (hash-set s1 k #t))))

(define (hset-intersect s1 s2)
  (if ((hset-count s1) . < . (hset-count s2))
      (hset-union s2 s1)
      (for/fold ([s s2]) ([k (in-hset s2)])
        (if (hash-ref s1 k #f)
            s
            (hash-remove s k)))))

(define (hset-partition s pred empty-y-set empty-n-set)
  (for/fold ([y empty-y-set] [n empty-n-set]) ([v (in-hset s)])
    (if (pred v)
        (values (hset-add y v) n)
        (values y (hset-add n v)))))

(define (hset->list s) (hash-keys s))

(define (list->hset l)
  (for/hset ([k (in-list l)])
    k))

(define (list->hseteq l)
  (for/hseteq ([k (in-list l)])
    k))

(define (hset-map h f)
  (for/fold ([result '()])
            ([x (in-hset h)])
    (cons (f x) result)))

(define (hset-filter h f)
  (for/fold ([result h])
            ([x (in-hset h)])
    (if (f x)
        result
        (hset-remove result x))))

(define (hset-for-each h f)
  (for ([x (in-hset h)]) (f x)))

(define-syntax-rule (for/hset bindings body ...)
  (for/hash bindings (values
                      (let ()
                        body ...)
                      #t)))

(define-syntax-rule (for/hseteq bindings body ...)
  (for/hasheq bindings (values
                        (let ()
                          body ...)
                        #t)))

(define-syntax-rule (for/hseteqv bindings body ...)
  (for/hasheqv bindings (values
                         (let ()
                           body ...)
                         #t)))

(define-syntax-rule (for*/hset bindings body ...)
  (for*/hash bindings (values
                       (let ()
                         body ...)
                       #t)))

(define-syntax-rule (for*/hseteq bindings body ...)
  (for*/hasheq bindings (values
                         (let ()
                           body ...)
                         #t)))
