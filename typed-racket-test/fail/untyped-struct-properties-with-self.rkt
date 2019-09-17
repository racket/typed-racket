#;
(exn-pred "Type Checker: Type \\(Struct-Property \\(-> Self Any\\)\\) could not be converted to a contract: contract generation not supported for Self")
#lang racket
(require errortrace)

(module foo racket
  (define-values (prop:hi hi? hi-ref) (make-struct-type-property 'hi))
  (provide prop:hi hi? hi-ref yyy)
  (define yyy 10))

(module ty-foo typed/racket
  (require/typed (submod ".." foo) [prop:hi (Struct-Property (-> Self Any))])
  (struct bar () #:property prop:hi (λ ([self : bar])
                                      (display (format "instance bar\n" )))))
