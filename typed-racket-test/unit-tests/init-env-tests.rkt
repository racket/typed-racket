#lang racket/base

(require "test-utils.rkt"
         rackunit
         syntax/id-set
         (rep object-rep type-rep)
         (env init-envs)
         (types abbrev))

(provide tests)
(gen-test-main)

(define (convert-type v)
  (syntax->datum (datum->syntax #f (type->sexp v))))
(define (convert-obj v)
  (syntax->datum (datum->syntax #f (object->sexp v))))


(define tests
  (test-suite "Init Env"
    (test-suite "Convert"
      (check-equal?
        (convert-type (-> -String -Symbol))
        '(simple-> (list -String) -Symbol))
      (check-equal?
        (convert-type (make-pred-ty -String))
        '(make-pred-ty (list Univ) -Boolean -String (-arg-path 0)))
      (check-equal?
        (convert-type (->acc (list (-lst -String)) -String (list -car)))
        '(->acc (list (-lst -String)) -String (list -car)))
      (check-equal?
        (convert-obj (make-Path '() (cons 0 0)))
        '(-arg-path 0))
      (check-equal?
        (convert-obj (make-Path '() (cons 1 0)))
        '(-arg-path 0 1))
      (check-equal?
        (convert-obj (make-Path (list -car) (cons 0 0)))
        '(make-Path (list -car) (cons 0 0)))
      (check-equal?
        (convert-type (-mu x (-lst* Univ (-box x))))
        '(make-Mu 'x (make-Pair Univ (make-Pair (make-Box (make-F 'x)) -Null))))
      (check-equal?
        (convert-type -StructTypeTop)
        '-StructTypeTop)
      (check-equal?
        (convert-type -BoxTop)
        '-BoxTop)
      (check-equal?
        (convert-type -ClassTop)
        '-ClassTop)
      (check-equal?
        (convert-type -field)
        '-field)
      (check-equal?
       (convert-type (make-StructType (make-Struct #'foo #f null #f #f #'foo? (immutable-free-id-set (list)))))
        '(make-StructType
          (make-Struct (quote-syntax foo) #f (list) #f #f (quote-syntax foo?) (immutable-free-id-set (list)))))
      (check-equal?
       (convert-type (make-StructTop (make-Struct #'foo #f null #f #f #'foo? (immutable-free-id-set (list)))))
        '(make-StructTop
          (make-Struct (quote-syntax foo) #f (list) #f #f (quote-syntax foo?) (immutable-free-id-set (list)))))
      (check-equal?
        (convert-type (make-Row null null null null #f))
        '(make-Row (list) (list) (list) (list) #f))
      (check-equal?
        (convert-type (make-Row (list (list 'foo -String #t))
                           (list (list 'bar -String))
                           null null #f))
        '(make-Row (list (list 'foo -String #t))
                   (list (list 'bar -String))
                   (list) (list) #f)))))
