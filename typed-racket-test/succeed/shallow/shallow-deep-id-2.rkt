#lang typed/racket/base

(module shallow typed/racket/base/shallow
  (provide bad-dom bad-cod)

  (: bad-dom (-> (Vectorof Real) Void))
  (define (bad-dom vr)
    (vector-set! (cast vr (Vectorof String)) 0 "X")
    (void))

  (: bad-cod (-> (Vectorof Symbol)))
  (define (bad-cod)
    (cast (vector 0) (Vectorof Symbol))))

(require 'shallow typed/rackunit)

(check-exn #rx"bad-dom: broke its own contract"
  (lambda () (bad-dom (vector 0 1))))

(check-exn #rx"bad-cod: broke its own contract"
  (lambda () (vector-ref (bad-cod) 0)))
