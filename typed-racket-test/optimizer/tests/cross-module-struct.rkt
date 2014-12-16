#;#;
#<<END
END
""
#lang typed/scheme #:optimize
#reader typed-racket-test/optimizer/reset-port

;; will be imported by cross-module-struct2
(provide (struct-out x))
(define-struct: x ((x : Integer)))
