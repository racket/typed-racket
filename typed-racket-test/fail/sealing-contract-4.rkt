#;
(exn-pred #rx"unrelated class")
#lang racket/base

(module u racket
  (define state #f)

  ;; don't allow smuggling with mutation either
  (define (mixin cls)
    (define result
      (if (not state)
          (class cls
            (super-new)
            (define/public (n x) x))
          ;; should subclass from cls, not state since otherwise
          ;; we lose the `m` method
          (class state (super-new))))
    (set! state cls)
    result)

  (provide mixin))

(module t typed/racket
  (require/typed (submod ".." u)
                 [mixin
                  (All (r #:row)
                   (-> (Class #:row-var r)
                       (Class #:row-var r [n (-> Integer Integer)])))])

  (mixin (class object% (super-new)))
  (mixin (class object%
           (super-new)
           (define/public (m x) x))))

(require 't)
