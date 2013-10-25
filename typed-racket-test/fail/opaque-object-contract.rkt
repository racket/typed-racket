#;
(exn-pred #rx"uncontracted typed.*blaming: .*opaque-object-contract.rkt")
#lang racket/base

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
(send o n)
