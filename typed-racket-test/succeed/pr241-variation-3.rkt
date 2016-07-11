#lang racket/base

;; Generic struct predicates are OK

(module u racket/base
 (require racket/generic)
 (define-generics foo)
 (struct foo-struct () #:methods gen:foo [] #:transparent)
 (define f1 (foo-struct))
 (provide f1 foo?))

(module t typed/racket/base
 (require/typed (submod ".." u)
  (#:opaque Foo foo?)
  (f1 Foo))
 (foo? 3)
 (foo? f1))

(require 't)
