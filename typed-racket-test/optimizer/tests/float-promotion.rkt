#;#;
#<<END
TR info: float-promotion.rkt 2:3 (assert (modulo 1 2) exact-positive-integer?) -- vector of floats
TR opt: float-promotion.rkt 2:0 (+ (assert (modulo 1 2) exact-positive-integer?) 2.0) -- binary float
TR opt: float-promotion.rkt 2:11 (modulo 1 2) -- binary nonzero fixnum
TR opt: float-promotion.rkt 3:0 (+ (expt 100 100) 2.0) -- binary float
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
