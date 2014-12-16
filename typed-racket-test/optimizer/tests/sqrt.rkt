#;#;
#<<END
TR opt: sqrt.rkt 4:2 (sqrt x) -- unary float
END
""
#lang typed/scheme
#:optimize
#reader typed-racket-test/optimizer/reset-port

(: f (Nonnegative-Float -> Nonnegative-Float))
(define (f x)
  (sqrt x))
