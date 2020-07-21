#lang racket/base

(require "test-utils.rkt" 
         (rep type-rep)
         (types utils kw-types abbrev numeric-tower)
         racket/match racket/set
         rackunit)
(provide tests)
(gen-test-main)

(define-syntax-rule (t arg expected)
  (begin
    (test-equal? (format "~a" '(arg expected))
                 (kw-convert arg #f)
                 expected)))

(define (extract-arrs t)
  (match t
    [(Fun: arrs) (apply set arrs)]
    [t t]))

(define-syntax-rule (t-opt ((req-arg ...) (opt-arg ...)) expected)
  (let ()
    (define (get-false v) #f)
    (test-equal? (format "~a" '(opt-convert (->opt req-arg ... (opt-arg ...) result) expected))
                 (extract-arrs
                   (opt-convert (->opt req-arg ... (opt-arg ...) result)
                                (length (list 'req-arg ...))
                                (length (list 'opt-arg ...))
                                (list (get-false 'opt-arg) ...)))
                 (extract-arrs expected))))

(define true (-val #t))
(define false (-val #f))
(define result (-val 'result))
(define one (-val 'one))
(define two (-val 'two))
(define three (-val 'three))
(define four (-val 'four))
(define (-or-undefined t) (Un -Unsafe-Undefined t))

(define tests
  (test-suite "Tests for keyword expansion"

    [t (-> result) (-> result)]
    [t (-> one result)
       (-> one result)]
    [t (-> one two three four result)
       (-> one two three four result)]
    [t (->opt (one) result)
       (-> (-or-undefined one) result)]
    [t (->opt (one two) result)
       (-> (-or-undefined one) (-or-undefined two) result)]
    [t (->opt one (two three) result)
       (-> one (-or-undefined two) (-or-undefined three) result)]

    [t-opt (() ()) (-> result)]
    [t-opt ((one) ())
           (-> one result)]
    [t-opt ((one two three four) ())
           (-> one two three four result)]
    [t-opt (() (one))
           (cl->*
            (-> -Unsafe-Undefined result)
            (-> (-or-undefined one) result))]
    [t-opt (() (one two))
           (cl->*
            (-> -Unsafe-Undefined -Unsafe-Undefined result)
            (-> (-or-undefined one) -Unsafe-Undefined result)
            (-> (-or-undefined one) (-or-undefined two) result))]
    [t-opt ((one) (two three))
           (cl->*
            (-> one -Unsafe-Undefined -Unsafe-Undefined result)
            (-> one (-or-undefined two) -Unsafe-Undefined result)
            (-> one (-or-undefined two) (-or-undefined three) result))]
    ))
