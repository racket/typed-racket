#lang typed/racket/shallow

;; Copied from TR -test/fail/ directory

(module u racket
  (define c%
    (class object%
      (super-new)
      (define/public (f x) '(+ x 1))))
  (provide c%))

(define-type C% (Class (f (-> Integer Integer))))

(require typed/rackunit)
(require/typed (submod "." u)
  (c% C%))

(: g (-> (Vector (Instance C%)) Integer))
(define (g vo)
  (define o (vector-ref vo 0))
  (send o f 2))

(check-exn exn:fail:contract?
  (lambda () (g (vector (new c%)))))

