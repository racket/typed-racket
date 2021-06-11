#lang typed/racket
(module a-mod typed/racket/base
  (provide (except-out (all-defined-out) make-avocado))
  (struct kiwi ([a : Number]) #:type-name Kiwi)
  (Kiwi 42)
  (struct super-kiwi Kiwi ([a : Number]))
  (struct avocado ([a : Number]) #:type-name Avocado #:constructor-name make-avocado))

(module b-mod typed/racket/base
  (provide (struct-out apple)
           (struct-out orange)
           (struct-out pear)
           (struct-out pineapple))

  (struct apple ([a : Number]))
  (struct orange ([a : Number]) #:constructor-name make-orange)
  (struct pear ([a : Number]) #:extra-constructor-name pear)
  (struct pineapple ([a : Number]) #:extra-constructor-name make-pineapple))

(module test-typed typed/racket
  (require (submod ".." b-mod))
  (require (submod ".." a-mod))
  (apple 42)
  (make-orange 42)
  (kiwi 42)
  (make-pineapple 42)
  (: avocado-id (-> Avocado Avocado))
  (define (avocado-id arg) arg)
  (struct hass-avocado avocado ())
  (struct HASS-AVOCADO Avocado ())

  (define-syntax (define-logn-ids stx)
    (syntax-case stx ()
      [(_ A)
       (with-syntax ([(g) (generate-temporaries #'(g))])
         #`(begin
             (struct g ())
             (struct A2 g () #:type-name A)))]))

  (define-logn-ids A))

(module test-untyped racket
  (require (submod ".." b-mod))
  (require (submod ".." a-mod))
  (define-syntax-rule (verify-contract expr ...)
    (begin
      (with-handlers ([exn:fail:contract? (lambda _
                                            (void))])
        expr
        (error "no contract violation: " (quote expr)))
      ...))
  (verify-contract (apple 'xxx)
                   (make-orange 'xxx)
                   (Kiwi 'xxx)
                   (kiwi 'xxx)
                   (pear 'xxx)
                   (make-pineapple 'xxx)))

(require 'test-typed)
(require 'test-untyped)
