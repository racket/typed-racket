#lang racket/base

;; Test 3 kinds of typed functions:
;; - a function defined by TR
;; - a function imported through `require/typed`
;; - a (safe) function from the standard library

(module u racket/base
  (provide f)
  (define (f x)
    x))

(module t typed/racket/base/shallow
  (require/typed (submod ".." u)
    (f (-> (Listof String) (Listof Natural))))

  (: g (-> Natural Boolean))
  (define (g x)
    (= x 4))

  (define (test)
    (filter g (f '("a" "bb" "ccc"))))

  (provide test))
(require 't rackunit)

(check-exn (λ (e) (or (regexp-match? #rx"expected: Natural" (exn-message e))
                      (regexp-match? #rx"shape-check" (exn-message e))))
  test)
