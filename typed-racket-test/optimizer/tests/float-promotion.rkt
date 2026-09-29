#;#;
#<<END
TR info: float-promotion.rkt 2:3 (assert (modulo 1 2) exact-positive-integer?) -- vector of floats
TR missed opt: float-promotion.rkt 3:0 (+ (expt 100 100) 2.0) -- all args float-arg-expr, result not Float -- caused by: 3:3 (expt 100 100)
TR opt: float-promotion.rkt 2:0 (+ (assert (modulo 1 2) exact-positive-integer?) 2.0) -- binary float
TR opt: float-promotion.rkt 2:11 (modulo 1 2) -- binary nonzero fixnum
END
#<<END
3.0
1e+200

END
#lang typed/scheme
#:optimize
#reader typed-racket-test/optimizer/reset-port

(+ (assert (modulo 1 2) exact-positive-integer?) 2.0)
(+ (expt 100 100) 2.0)
