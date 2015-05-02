#lang racket/base

;; Test unsafe require forms

(module a racket/base
  (struct foo (x y))
  (define a-foo (foo 1 2))
  (provide (struct-out foo) a-foo))

(module b typed/racket
  (require/typed racket/contract/combinator [#:opaque Blame exn:fail:contract:blame?])
  (require typed/racket/unsafe)
  (unsafe-require/typed (submod ".." a)
                        [#:struct foo ([x : String] [y : String])]
                        [a-foo foo])

  ;; UNSAFE
  ;; primitive error, no blame should be raised
  (with-handlers ([(negate exn:fail:contract:blame?) void])
    (string-append (foo-x a-foo))))

(module c typed/racket
  (require/typed racket/contract/combinator [#:opaque Blame exn:fail:contract:blame?])
  (require typed/racket/unsafe)
  (unsafe-require/typed racket/base
                        [string-append (-> String String Integer)])

  ;; UNSAFE
  (with-handlers ([(negate exn:fail:contract:blame?) void])
    (number->string (string-append "foo" "bar"))))

(require 'b 'c)
