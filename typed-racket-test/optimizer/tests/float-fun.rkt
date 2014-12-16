#;#;
#<<END
TR opt: float-fun.rkt 4:2 (+ x 1.0) -- binary float
END
""
#lang typed/racket
#:optimize
#reader typed-racket-test/optimizer/reset-port

(: f (Float -> Float))
(define (f x)
  (+ x 1.0))
