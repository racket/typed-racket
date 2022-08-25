#lang typed/racket/base/shallow

(module deep typed/racket/base
  (provide v->v)
  (: v->v (-> (Vectorof Real) (Vectorof Symbol)))
  (define (v->v vr)
    (define n1 (vector-ref vr 0))
    (define s1 (string->symbol (number->string n1)))
    (vector 'a s1)))

(require 'deep typed/rackunit)

(check-exn #rx"v->v: contract violation"
  (lambda () (v->v (cast (vector "X") (Vectorof Real)))))

(check-exn #rx"v->v: contract violation"
  (lambda ()
    (vector-set!
      (cast (v->v (vector 1)) (Vectorof String))
      0 "X")))

