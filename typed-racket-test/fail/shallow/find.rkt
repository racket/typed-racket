#;
(exn-pred exn:fail:contract? #rx"shape-check")

#lang typed/racket/base/shallow

(module a racket/base
  (provide fake-int* checker)
  (define fake-int* '(NaN xxx))
  (define (checker x)
    #true))

(require/typed 'a
  (fake-int* (Listof Integer))
  (checker (-> Integer Boolean)))

(define x : (U #f Integer)
  (findf checker fake-int*))

