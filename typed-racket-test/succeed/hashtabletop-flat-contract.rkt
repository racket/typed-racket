#lang typed/racket/base
(require typed/rackunit)

;; Test that `HashTableTop` generates a flat contract in cast,
;;  and a chaperone when exported to untyped code

(define h : (Mutable-HashTable Symbol String) (make-hash '((A . "A"))))
(void (cast h HashTableTop))

(module u racket/base
  (provide f)
  (define (f x)
    (hash-set! x 'A 'bad)))
(require/typed 'u
  (f (-> HashTableTop Void)))

(check-exn exn:fail:contract? (lambda () (f h)))
