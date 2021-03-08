#lang typed/racket/base
(module a-mod typed/racket/base
  (provide (except-out (all-defined-out) make-avocado))
  (struct apple ())
  (struct orange () #:constructor-name make-orange)
  (struct kiwi () #:constructor-name kiwi)
  (struct pear () #:extra-constructor-name pear)
  (struct avocado () #:type-name Avocado #:constructor-name make-avocado))

(require 'a-mod)
(apple)
(make-orange)
(kiwi)
(: avocado-id (-> Avocado Avocado))
(define (avocado-id arg) arg)
(struct hass-avocado avocado ())
