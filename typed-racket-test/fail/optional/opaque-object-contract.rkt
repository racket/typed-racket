#;
(exn-pred #rx"uncontracted typed.*blaming: .*opaque-object-contract.rkt")
#lang racket/base

(module a typed/racket/optional
  (provide c%)
  (define c%
    (class object%
      (super-new)
      (define/public (m) (void)))))

(module b typed/racket/deep
  (require/typed (submod ".." a) [c% (Class [m (-> Void)])])
  (provide o)
  (: o (Object))
  (define o (new (class c%
                   (super-new)
                   (define/public (n) (void))))))

(require 'b)
(require racket/class)

(send o m)
(send o n)
