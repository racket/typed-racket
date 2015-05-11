#lang racket/base

;; Ensure that for untyped-untyped calls, the opaque object
;; contract does not interfere.

(require racket/class)

(module a racket
  (provide c%)
  (define c%
    (class object%
      (super-new)
      (define/public (m) (void)))))

(module b typed/racket
  (require/typed (submod ".." a) [c% (Class [m (-> Void)])])
  (provide o)
  (: o (Object))
  (define o (new (class c%
                   (super-new)
                   (define/public (n) (void))))))

(require 'b)

(send o m)
