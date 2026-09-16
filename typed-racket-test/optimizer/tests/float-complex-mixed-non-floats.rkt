#;#;
#<<END
TR missed opt: float-complex-mixed-non-floats.rkt 3:0 (+ 0 1.0 1) -- all args float-arg-expr, result not Float -- caused by: 3:3 0, 3:9 1
TR opt: float-complex-mixed-non-floats.rkt 4:0 (+ 9007199254740993 1 0.0+1.0i 0) -- unboxed binary float complex
TR opt: float-complex-mixed-non-floats.rkt 4:20 1 -- non float complex in complex ops
TR opt: float-complex-mixed-non-floats.rkt 4:22 0.0+1.0i -- unboxed literal
TR opt: float-complex-mixed-non-floats.rkt 4:3 9007199254740993 -- non float complex in complex ops
TR opt: float-complex-mixed-non-floats.rkt 4:31 0 -- non float complex in complex ops
TR opt: float-complex-mixed-non-floats.rkt 7:0 (+ 1.0+1.0i 1.0 1) -- unboxed binary float complex
TR opt: float-complex-mixed-non-floats.rkt 7:12 1.0 -- float in complex ops
TR opt: float-complex-mixed-non-floats.rkt 7:16 1 -- non float complex in complex ops
TR opt: float-complex-mixed-non-floats.rkt 7:3 1.0+1.0i -- unboxed literal
END
#<<END
2.0
9007199254740994.0+1.0i
3.0+1.0i

END
#lang typed/racket/base
#:optimize
#reader typed-racket-test/optimizer/reset-port

;; A non-float before the first flonum keeps the exact prefix intact.
(+ 0 1.0 1)
(+ 9007199254740993 1 0.0+1.0i 0)

;; A non-float after the first flonum must be converted for unsafe-fl+.
(+ 1.0+1.0i 1.0 1)
