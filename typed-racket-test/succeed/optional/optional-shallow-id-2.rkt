#lang typed/racket/base/shallow

;; basic optional -> shallow

(module uuu racket/base
  (provide bad-sym)
  (define bad-sym #f))

(module opt typed/racket/base/optional
  (provide get-sym)
  (require/typed (submod ".." uuu)
    (bad-sym Symbol))
  (: get-sym (-> Symbol))
  (define (get-sym)
    bad-sym))

(require 'opt typed/rackunit)

(check-exn #rx"shape-check" (lambda () (ann (get-sym) Symbol)))

