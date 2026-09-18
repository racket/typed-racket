#;#;
#<<END
TR opt: float-complex-square.rkt 4:5 z -- unbox float-complex
TR opt: float-complex-square.rkt 4:2 (* z z) -- unboxed float complex square
END
#<<END
-3.0+4.0i

END
#lang typed/scheme
#:optimize
#reader typed-racket-test/optimizer/reset-port

(: square (Float-Complex -> Float-Complex))
(define (square z)
  (* z z))

(square 1.0+2.0i)
