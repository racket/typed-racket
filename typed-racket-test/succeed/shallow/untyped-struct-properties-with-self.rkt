#lang racket

(module foo racket
  (define-values (prop:hi hi? hi-ref) (make-struct-type-property 'hi))
  (provide prop:hi hi? hi-ref yyy)
  (define yyy 10))

(module ty-foo typed/racket/shallow
  (require/typed (submod ".." foo) [prop:hi (Struct-Property (-> Self Any))]  [hi-ref (-> Any (-> Any Void))])
  (struct bar () #:property prop:hi (λ ([self : bar])
                                      (display (format "instance bar\n" ))))
  (hi-ref (bar))
  )

(require 'ty-foo)
