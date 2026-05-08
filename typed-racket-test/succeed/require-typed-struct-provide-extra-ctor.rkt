#lang typed/racket/base

;; Tests that require/typed/provide and require-typed-struct/provide
;; correctly export the constructor identifier when used inside a
;; submodule, including when #:extra-constructor-name or
;; #:constructor-name is specified.

(module untyped racket/base
  (struct foo (a b) #:extra-constructor-name make-foo #:transparent)
  (struct bar (x) #:constructor-name make-bar #:transparent)
  (struct baz (n) #:transparent)
  (provide (struct-out foo) (struct-out bar) (struct-out baz)))

;; require/typed/provide with #:extra-constructor-name
(module test-foo-rtp typed/racket/base
  (module inner typed/racket/base
    (require/typed/provide (submod ".." ".." untyped)
      [#:struct foo ([a : Integer] [b : Integer]) #:extra-constructor-name make-foo]))
  (require 'inner)
  (unless (foo? (foo 1 2)) (error "rtp: foo ctor failed"))
  (unless (foo? (make-foo 3 4)) (error "rtp: make-foo ctor failed"))
  (unless (= 1 (foo-a (foo 1 2))) (error "rtp: foo-a failed")))
(require 'test-foo-rtp)

;; require-typed-struct/provide with #:extra-constructor-name
(module test-foo-rts typed/racket/base
  (module inner typed/racket/base
    (require-typed-struct/provide foo ([a : Integer] [b : Integer])
                                  #:extra-constructor-name make-foo
                                  (submod ".." ".." untyped)))
  (require 'inner)
  (unless (foo? (foo 5 6)) (error "rts: foo failed"))
  (unless (foo? (make-foo 7 8)) (error "rts: make-foo failed")))
(require 'test-foo-rts)

;; require/typed/provide with #:constructor-name
(module test-bar-rtp typed/racket/base
  (module inner typed/racket/base
    (require/typed/provide (submod ".." ".." untyped)
      [#:struct bar ([x : Integer]) #:constructor-name make-bar]))
  (require 'inner)
  (unless (bar? (make-bar 9)) (error "rtp: make-bar ctor failed")))
(require 'test-bar-rtp)

;; require-typed-struct/provide with #:constructor-name
(module test-bar-rts typed/racket/base
  (module inner typed/racket/base
    (require-typed-struct/provide bar ([x : Integer])
                                  #:constructor-name make-bar
                                  (submod ".." ".." untyped)))
  (require 'inner)
  (unless (bar? (make-bar 10)) (error "rts: make-bar failed")))
(require 'test-bar-rts)

;; basic case (no constructor option)
(module test-baz typed/racket/base
  (module inner typed/racket/base
    (require/typed/provide (submod ".." ".." untyped)
      [#:struct baz ([n : Integer])]))
  (require 'inner)
  (unless (baz? (baz 11)) (error "baz failed")))
(require 'test-baz)
