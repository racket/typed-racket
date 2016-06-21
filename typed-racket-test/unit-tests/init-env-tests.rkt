#lang racket/base

(require "test-utils.rkt"
         rackunit
         (rep object-rep type-rep)
         (env init-envs)
         (types abbrev union))

(provide tests)
(gen-test-main)

(define (convert v)
  (syntax->datum (datum->syntax #f (type->sexp v))))


(define tests
  (test-suite "Init Env"
    (test-suite "Convert"
      (check-equal?
        (convert (-> -String -Symbol))
        '(simple-> (list -String) -Symbol))
      (check-equal?
        (convert (make-pred-ty -String))
        '(make-pred-ty (list Univ) -Boolean -String (-arg-path 0)))
      (check-equal?
        (convert (->acc (list (-lst -String)) -String (list -car)))
        '(->acc (list (-lst -String)) -String (list -car)))
      (check-equal?
        (convert (-mu x (-lst* Univ (-box x))))
        '(make-Mu 'x (make-Pair Univ (make-Pair (make-Box (make-F 'x)) -Null))))
      (check-equal?
        (convert (make-StructTypeTop))
        '-StructTypeTop)
      (check-equal?
        (convert (make-BoxTop))
        '-BoxTop)
      (check-equal?
        (convert (make-ClassTop))
        '-ClassTop)
      (check-equal?
        (convert (make-FieldPE))
        '-field)
      (check-equal?
        (convert (make-StructType (make-Struct #'foo #f null #f #f #'foo?)))
        '(make-StructType
          (make-Struct (quote-syntax foo) #f (list) #f #f (quote-syntax foo?))))
      (check-equal?
        (convert (make-StructTop (make-Struct #'foo #f null #f #f #'foo?)))
        '(make-StructTop
          (make-Struct (quote-syntax foo) #f (list) #f #f (quote-syntax foo?))))
    )
  ))
