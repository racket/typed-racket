#lang racket/base

;; Test unsafe provide form

(require racket/contract/combinator
         racket/function)

(module a typed/racket
  (require typed/racket/unsafe)

  (: f (-> String String))
  (define (f x)
    (string-append x "foo"))
  (unsafe-provide f)
  (unsafe-provide (rename-out [f g]))

  (struct foo ([x : String]))
  (unsafe-provide (struct-out foo)))

(require 'a)

;; UNSAFE
;; primitive error, no blame should be raised
(with-handlers ([(negate exn:fail:contract:blame?) void])
  (f 3))

(with-handlers ([(negate exn:fail:contract:blame?) void])
  (g 3))

(with-handlers ([(negate exn:fail:contract:blame?) void])
  (foo-x (foo 3)))
