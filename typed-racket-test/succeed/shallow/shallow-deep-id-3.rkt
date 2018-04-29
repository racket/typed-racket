#lang typed/racket/base

(module shallow typed/racket/base/shallow
  (provide bad-dom bad-cod)

  (: bad-cod (-> (Listof String)))
  (define (bad-cod)
    (cast (list 0) (Listof String)))

  (: bad-dom (-> (-> (Listof String) (Listof String)) (Listof String)))
  (define (bad-dom f)
    (f (cast (list 1) (Listof String)))))

(require 'shallow typed/rackunit)

(check-exn #rx"bad-cod: broke its own contract"
  (lambda () (car (bad-cod))))

(check-exn #rx"bad-dom: broke its own contract"
  (lambda () (bad-dom values)))

