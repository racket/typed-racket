#;#;
#<<END
TR missed opt: float-complex-float.rkt 11:3 (make-polar 4.8063810141303426e-57 -1.9082319f0) -- non-complex value in complex arithmetic
TR missed opt: float-complex-float.rkt 12:63 (make-rectangular 1.4291365847030308e-64 -0.76987815f0) -- generic comparison -- caused by: 12:104 -0.76987815f0
TR missed opt: float-complex-float.rkt 14:17 (floor -2.2441852f0) -- all args float-arg-expr, result not Float -- caused by: 14:24 -2.2441852f0
TR opt: float-complex-float.rkt 10:0 (/ 2.3454025f0 (flmin (real->double-flonum 1.797693134862315e+308) (real->double-flonum -1.2848677f+32)) (make-rectangular +nan.0 0.0)) -- unboxed binary float complex
TR opt: float-complex-float.rkt 10:105 (make-rectangular +nan.0 0.0) -- make-rectangular elimination
TR opt: float-complex-float.rkt 10:15 (flmin (real->double-flonum 1.797693134862315e+308) (real->double-flonum -1.2848677f+32)) -- binary float
TR opt: float-complex-float.rkt 10:15 (flmin (real->double-flonum 1.797693134862315e+308) (real->double-flonum -1.2848677f+32)) -- float in complex ops
TR opt: float-complex-float.rkt 10:22 (real->double-flonum 1.797693134862315e+308) -- float to float
TR opt: float-complex-float.rkt 10:3 2.3454025f0 -- non float complex in complex ops
TR opt: float-complex-float.rkt 11:0 (+ (make-polar 4.8063810141303426e-57 -1.9082319f0) -0.8414709848078965) -- unboxed binary float complex
TR opt: float-complex-float.rkt 11:3 (make-polar 4.8063810141303426e-57 -1.9082319f0) -- non float complex in complex ops
TR opt: float-complex-float.rkt 11:52 -0.8414709848078965 -- float in complex ops
TR opt: float-complex-float.rkt 12:0 (+ 1.5245886f+12 (max (exact-round 2) (exact-round 5/4)) (tanh (make-rectangular 1.4291365847030308e-64 -0.76987815f0))) -- unboxed binary float complex
TR opt: float-complex-float.rkt 12:17 (max (exact-round 2) (exact-round 5/4)) -- non float complex in complex ops
TR opt: float-complex-float.rkt 12:3 1.5245886f+12 -- non float complex in complex ops
TR opt: float-complex-float.rkt 12:57 (tanh (make-rectangular 1.4291365847030308e-64 -0.76987815f0)) -- unbox float-complex
TR opt: float-complex-float.rkt 13:0 (* (min 3/4) 0.9845773f0 (make-rectangular 3 0.0)) -- unboxed binary float complex
TR opt: float-complex-float.rkt 13:13 0.9845773f0 -- non float complex in complex ops
TR opt: float-complex-float.rkt 13:25 (make-rectangular 3 0.0) -- make-rectangular elimination
TR opt: float-complex-float.rkt 13:3 (min 3/4) -- non float complex in complex ops
TR opt: float-complex-float.rkt 13:3 (min 3/4) -- unary number
TR opt: float-complex-float.rkt 14:0 (/ 3.2993203f+37 (floor -2.2441852f0) (make-polar 0.42484267570553375 4.940078147009648)) -- unboxed binary float complex
TR opt: float-complex-float.rkt 14:17 (floor -2.2441852f0) -- non float complex in complex ops
TR opt: float-complex-float.rkt 14:3 3.2993203f+37 -- non float complex in complex ops
TR opt: float-complex-float.rkt 14:38 (make-polar 0.42484267570553375 4.940078147009648) -- make-rectangular elimination
TR opt: float-complex-float.rkt 15:0 (/ -5 2/7 (make-polar -0.0 (fltan (real->double-flonum -3.833043f+21)))) -- unboxed binary float complex
TR opt: float-complex-float.rkt 15:10 (make-polar -0.0 (fltan (real->double-flonum -3.833043f+21))) -- make-rectangular elimination
TR opt: float-complex-float.rkt 15:27 (fltan (real->double-flonum -3.833043f+21)) -- unary float
TR opt: float-complex-float.rkt 15:3 -5 -- non float complex in complex ops
TR opt: float-complex-float.rkt 15:6 2/7 -- non float complex in complex ops
TR opt: float-complex-float.rkt 16:0 (/ (+ (exact-round 1.8655746f+35) (exact-round 1)) 2.0324421f-21 (make-rectangular 4 1.7976931348623157e+308)) -- unboxed binary float complex
TR opt: float-complex-float.rkt 16:3 (+ (exact-round 1.8655746f+35) (exact-round 1)) -- non float complex in complex ops
TR opt: float-complex-float.rkt 16:34 (exact-round 1) -- non float complex in complex ops
TR opt: float-complex-float.rkt 16:51 2.0324421f-21 -- non float complex in complex ops
TR opt: float-complex-float.rkt 16:6 (exact-round 1.8655746f+35) -- non float complex in complex ops
TR opt: float-complex-float.rkt 16:6 (exact-round 1.8655746f+35) -- non float complex in complex ops
TR opt: float-complex-float.rkt 16:65 (make-rectangular 4 1.7976931348623157e+308) -- make-rectangular elimination
TR opt: float-complex-float.rkt 17:0 (+ +inf.0-0.0i +nan.0) -- unboxed binary float complex
TR opt: float-complex-float.rkt 17:15 +nan.0 -- float in complex ops
TR opt: float-complex-float.rkt 17:3 +inf.0-0.0i -- unboxed literal
TR opt: float-complex-float.rkt 18:0 (+ (- 0.0 16 -inf.0+0.0i) +nan.0) -- unboxed binary float complex
TR opt: float-complex-float.rkt 18:10 16 -- non float complex in complex ops
TR opt: float-complex-float.rkt 18:13 -inf.0+0.0i -- unboxed literal
TR opt: float-complex-float.rkt 18:26 +nan.0 -- float in complex ops
TR opt: float-complex-float.rkt 18:3 (- 0.0 16 -inf.0+0.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 18:6 0.0 -- float in complex ops
TR opt: float-complex-float.rkt 19:0 (+ (floor (+ (exact-round -25.263502f0) (exact-round -1/2))) (- 0.0 16 (make-rectangular -inf.0 0.0)) +nan.0) -- unboxed binary float complex
TR opt: float-complex-float.rkt 19:102 +nan.0 -- float in complex ops
TR opt: float-complex-float.rkt 19:3 (floor (+ (exact-round -25.263502f0) (exact-round -1/2))) -- non float complex in complex ops
TR opt: float-complex-float.rkt 19:61 (- 0.0 16 (make-rectangular -inf.0 0.0)) -- unboxed binary float complex
TR opt: float-complex-float.rkt 19:64 0.0 -- float in complex ops
TR opt: float-complex-float.rkt 19:68 16 -- non float complex in complex ops
TR opt: float-complex-float.rkt 19:71 (make-rectangular -inf.0 0.0) -- make-rectangular elimination
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
1524588609536.0-0.9694319337396835i
2.2152990102767944+0.0i
-5.84330415295662e+36-2.521848811753627e+37i
-inf.0-0.0i
+nan.0+nan.0i
+nan.0-0.0i
+nan.0-0.0i
+nan.0-0.0i

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
(+ 1.5245886f+12 (max (exact-round 2) (exact-round 5/4)) (tanh (make-rectangular 1.4291365847030308e-64 -0.76987815f0)))
(* (min 3/4) 0.9845773f0 (make-rectangular 3 0.0))
(/ 3.2993203f+37 (floor -2.2441852f0) (make-polar 0.42484267570553375 4.940078147009648))
(/ -5 2/7 (make-polar -0.0 (fltan (real->double-flonum -3.833043f+21))))
(/ (+ (exact-round 1.8655746f+35) (exact-round 1)) 2.0324421f-21 (make-rectangular 4 1.7976931348623157e+308))
(+ +inf.0-0.0i +nan.0)
(+ (- 0.0 16 -inf.0+0.0i) +nan.0)
(+ (floor (+ (exact-round -25.263502f0) (exact-round -1/2))) (- 0.0 16 (make-rectangular -inf.0 0.0)) +nan.0)
