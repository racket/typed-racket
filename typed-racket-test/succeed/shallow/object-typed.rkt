#lang racket

(module t typed/racket/shallow
  (define-type C0% (Class (init-field (s String)) (f (-> Integer Integer))))
  (: c0% C0%)
  (define c0%
    (class object%
      (super-new)
      (init-field s)
      (define/public (f x) (+ 4 4))))

  (define-type C1% (Class (init-field (s String)) (f (->* [Integer] [#:y Integer] Integer))))
  (: c1% C1%)
  (define c1%
    (class object%
      (super-new)
      (init-field s)
      (define/public (f x #:y [y 0]) (+ 4 4))))
  (provide c0% c1%))

(require 't rackunit)

(define o0 (new c0% (s "hello")))
(check-exn exn:fail:contract?
  (lambda () (send o0 f 'NaN)))
(check-not-exn
  (lambda () (new c0% (s 'NotString))))

(define o1 (new c1% (s "hello")))
(check-exn exn:fail:contract?
  (lambda () (send o1 f 'NaN #:y 1)))
(check-exn exn:fail:contract?
  (lambda () (send o1 f 0 #:y 'NaN)))
(check-not-exn
  (lambda () (send o1 f 0)))
(check-not-exn
  (lambda () (send o1 f 0 #:y 1)))
