#lang typed/racket
(module a-mod typed/racket/base
  (provide (except-out (all-defined-out) make-avocado))
  (struct apple ())
  (struct orange () #:constructor-name make-orange)
  (struct kiwi () #:type-name Kiwi)
  (Kiwi)
  (struct super-kiwi Kiwi ())
  (struct pear () #:extra-constructor-name pear)
  (struct avocado () #:type-name Avocado #:constructor-name make-avocado))

(require 'a-mod)
(apple)
(make-orange)
(kiwi)
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

(define-logn-ids A)
