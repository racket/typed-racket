#;#;
#<<END
TR missed opt: float-complex-float.rkt 11:3 (make-polar 4.8063810141303426e-57 -1.9082319f0) -- non-complex value in complex arithmetic
TR opt: float-complex-float.rkt 10:0 (/ 2.3454025f0 (flmin (real->double-flonum 1.797693134862315e+308) (real->double-flonum -1.2848677f+32)) (make-rectangular +nan.0 0.0)) -- unboxed binary float complex
TR opt: float-complex-float.rkt 10:105 (make-rectangular +nan.0 0.0) -- make-rectangular elimination
TR opt: float-complex-float.rkt 10:15 (flmin (real->double-flonum 1.797693134862315e+308) (real->double-flonum -1.2848677f+32)) -- binary float
TR opt: float-complex-float.rkt 10:15 (flmin (real->double-flonum 1.797693134862315e+308) (real->double-flonum -1.2848677f+32)) -- float in complex ops
TR opt: float-complex-float.rkt 10:22 (real->double-flonum 1.797693134862315e+308) -- float to float
TR opt: float-complex-float.rkt 10:3 2.3454025f0 -- non float complex in complex ops
TR opt: float-complex-float.rkt 11:0 (+ (make-polar 4.8063810141303426e-57 -1.9082319f0) -0.8414709848078965) -- unboxed binary float complex
TR opt: float-complex-float.rkt 11:3 (make-polar 4.8063810141303426e-57 -1.9082319f0) -- non float complex in complex ops
TR opt: float-complex-float.rkt 11:52 -0.8414709848078965 -- float in complex ops
TR opt: float-complex-float.rkt 4:0 (+ 1.0+2.0i 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 4:12 2.0 -- float in complex ops
TR opt: float-complex-float.rkt 4:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float.rkt 4:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float.rkt 5:0 (- 1.0 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 5:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float.rkt 5:3 1.0 -- float in complex ops
TR opt: float-complex-float.rkt 5:7 2.0+4.0i -- unboxed literal
TR opt: float-complex-float.rkt 6:0 (- 1.0+2.0i 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 6:12 2.0 -- float in complex ops
TR opt: float-complex-float.rkt 6:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float.rkt 6:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float.rkt 7:0 (- 1.0+2.0i 2.0+4.0i 3.0) -- unboxed binary float complex
TR opt: float-complex-float.rkt 7:12 2.0+4.0i -- unboxed literal
TR opt: float-complex-float.rkt 7:21 3.0 -- float in complex ops
TR opt: float-complex-float.rkt 7:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float.rkt 8:0 (/ 0.0 +inf.0-1.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 8:3 0.0 -- float in complex ops
TR opt: float-complex-float.rkt 8:7 +inf.0-1.0i -- unboxed literal
TR opt: float-complex-float.rkt 9:0 (* -0.9263371220283309 3/2 (make-rectangular +inf.f 0.7692234292042541)) -- unboxed binary float complex
TR opt: float-complex-float.rkt 9:23 3/2 -- non float complex in complex ops
TR opt: float-complex-float.rkt 9:27 (make-rectangular +inf.f 0.7692234292042541) -- make-rectangular elimination
TR opt: float-complex-float.rkt 9:3 -0.9263371220283309 -- float in complex ops
END
#<<END
6.0+8.0i
-4.0-10.0i
-4.0-4.0i
-4.0-2.0i
+nan.0+0.0i
-inf.0-1.0688403264087485i
+nan.0+0.0i
-0.8414709848078965-4.5353337789114595e-57i

END
#lang typed/scheme
#:optimize
#reader typed-racket-test/optimizer/reset-port

(require racket/flonum)

(+ 1.0+2.0i 2.0 3.0+6.0i)
(- 1.0 2.0+4.0i 3.0+6.0i)
(- 1.0+2.0i 2.0 3.0+6.0i)
(- 1.0+2.0i 2.0+4.0i 3.0)
(/ 0.0 +inf.0-1.0i)
(* -0.9263371220283309 3/2 (make-rectangular +inf.f 0.7692234292042541))
(/ 2.3454025f0 (flmin (real->double-flonum 1.797693134862315e+308) (real->double-flonum -1.2848677f+32)) (make-rectangular +nan.0 0.0))
(+ (make-polar 4.8063810141303426e-57 -1.9082319f0) -0.8414709848078965)
