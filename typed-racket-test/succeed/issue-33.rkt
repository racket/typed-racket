#lang typed/racket/base

;; regression test for issue #33
;; https://github.com/racket/typed-racket/issues/33

;; In 2015, gave the error message:
;;   Type Checker: type mismatch
;;    expected: (U (List Symbol Any) (List String Symbol))
;;    given: (List String Any)
;;    in: (list "foo" (quote bold))

(: x (U (List Symbol Any)
        (List String Symbol)))
(define x (list "foo" 'bold))

(when (and (string? (car x)) (symbol? (cadr x)))
  (ann x (List String Symbol)))
