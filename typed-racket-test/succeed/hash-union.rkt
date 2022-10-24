#lang typed/racket

(module+ test
  (require typed/rackunit)

  (require racket/hash)
  (check-equal? ((inst hash-union Symbol Integer)
                 #hash((a . 1)(b . 2))
                 #hash((a . 3)(c . 4))
                 #hash((a . 5))
                 (make-hash '((c . 6)))
                 #:combine +)
                #hash((a . 9)(b . 2)(c . 10)))

  (check-equal? ((inst hash-intersect Symbol Integer)
                 #hash((a . 1)(b . 2))
                 #hash((a . 2)(c . 4))
                 #hash((a . 5))
                 (make-hash '((a . 1)))
                 #:combine *)
                #hash((a . 10)))

  (let ([A (make-hash '((a . 1)(b . 2)))])
    ((inst hash-union! Symbol Integer)
     A
     #hash((a . 3)(c . 4))
     #hash((a . 5))
     (make-hash '((c . 6)))
     #:combine +)
    (check-equal? (hash-ref A 'a) 9))
  

    (check-equal? ((inst hash-union Symbol Integer)
                 #hash((a . 1)(b . 2))
                 #hash((a . 3)(c . 4))
                 #hash((a . 5))
                 (make-hash '((c . 6)))
                 #:combine/key (λ ([k : Symbol][a : Integer][b : Integer]) (+ a b)))
                #hash((a . 9)(b . 2)(c . 10)))

  (check-equal? ((inst hash-intersect Symbol Integer)
                 #hash((a . 1)(b . 2))
                 #hash((a . 2)(c . 4))
                 #hash((a . 5))
                 (make-hash '((a . 1)))
                 #:combine/key (λ ([k : Symbol][a : Integer][b : Integer]) (* a b)))
                #hash((a . 10)))

  (let ([A (make-hash '((a . 1)(b . 2)))])
    ((inst hash-union! Symbol Integer)
     A
     #hash((a . 3)(c . 4))
     #hash((a . 5))
     (make-hash '((c . 6)))
     #:combine/key (λ ([k : Symbol][a : Integer][b : Integer]) (+ a b)))
    (check-equal? (hash-ref A 'a) 9)))