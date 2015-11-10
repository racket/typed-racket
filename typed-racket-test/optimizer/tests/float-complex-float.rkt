#;#;
#<<END
TR info: float-complex-float.rkt 7:3 printf -- hidden parameter
TR missed opt: float-complex-float.rkt 18:6 (make-polar 4.8063810141303426e-57 -1.9082319f0) -- non-complex value in complex arithmetic
TR missed opt: float-complex-float.rkt 19:66 (make-rectangular 1.4291365847030308e-64 -0.76987815f0) -- generic comparison -- caused by: 19:107 -0.76987815f0
TR missed opt: float-complex-float.rkt 21:20 (floor -2.2441852f0) -- all args float-arg-expr, result not Float -- caused by: 21:27 -2.2441852f0
TR missed opt: float-complex-float.rkt 28:6 (+ 1 -0.17853218f0) -- all args float-arg-expr, result not Float -- caused by: 28:9 1, 28:11 -0.17853218f0
TR opt: float-complex-float.rkt 11:0 (+ 1.0+2.0i 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 11:12 2.0 -- float in complex ops
TR opt: float-complex-float.rkt 11:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float.rkt 11:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float.rkt 12:0 (- 1.0 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 12:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float.rkt 12:3 1.0 -- float in complex ops
TR opt: float-complex-float.rkt 12:7 2.0+4.0i -- unboxed literal
TR opt: float-complex-float.rkt 13:0 (- 1.0+2.0i 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 13:12 2.0 -- float in complex ops
TR opt: float-complex-float.rkt 13:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float.rkt 13:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float.rkt 14:0 (- 1.0+2.0i 2.0+4.0i 3.0) -- unboxed binary float complex
TR opt: float-complex-float.rkt 14:12 2.0+4.0i -- unboxed literal
TR opt: float-complex-float.rkt 14:21 3.0 -- float in complex ops
TR opt: float-complex-float.rkt 14:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float.rkt 15:0 (/ 0.0 +inf.0-1.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 15:3 0.0 -- float in complex ops
TR opt: float-complex-float.rkt 15:7 +inf.0-1.0i -- unboxed literal
TR opt: float-complex-float.rkt 16:26 3/2 -- non float complex in complex ops
TR opt: float-complex-float.rkt 16:3 (* -0.9263371220283309 3/2 (make-rectangular +inf.f 0.7692234292042541)) -- unboxed binary float complex
TR opt: float-complex-float.rkt 16:30 (make-rectangular +inf.f 0.7692234292042541) -- make-rectangular elimination
TR opt: float-complex-float.rkt 16:6 -0.9263371220283309 -- float in complex ops
TR opt: float-complex-float.rkt 17:0 (/ 2.3454025f0 (flmin (real->double-flonum 1.797693134862315e+308) (real->double-flonum -1.2848677f+32)) (make-rectangular +nan.0 0.0)) -- unboxed binary float complex
TR opt: float-complex-float.rkt 17:105 (make-rectangular +nan.0 0.0) -- make-rectangular elimination
TR opt: float-complex-float.rkt 17:15 (flmin (real->double-flonum 1.797693134862315e+308) (real->double-flonum -1.2848677f+32)) -- binary float
TR opt: float-complex-float.rkt 17:15 (flmin (real->double-flonum 1.797693134862315e+308) (real->double-flonum -1.2848677f+32)) -- float in complex ops
TR opt: float-complex-float.rkt 17:22 (real->double-flonum 1.797693134862315e+308) -- float to float
TR opt: float-complex-float.rkt 17:3 2.3454025f0 -- non float complex in complex ops
TR opt: float-complex-float.rkt 18:3 (+ (make-polar 4.8063810141303426e-57 -1.9082319f0) -0.8414709848078965) -- unboxed binary float complex
TR opt: float-complex-float.rkt 18:55 -0.8414709848078965 -- float in complex ops
TR opt: float-complex-float.rkt 18:6 (make-polar 4.8063810141303426e-57 -1.9082319f0) -- non float complex in complex ops
TR opt: float-complex-float.rkt 19:20 (max (exact-round 2) (exact-round 5/4)) -- non float complex in complex ops
TR opt: float-complex-float.rkt 19:3 (+ 1.5245886f+12 (max (exact-round 2) (exact-round 5/4)) (tanh (make-rectangular 1.4291365847030308e-64 -0.76987815f0))) -- unboxed binary float complex
TR opt: float-complex-float.rkt 19:6 1.5245886f+12 -- non float complex in complex ops
TR opt: float-complex-float.rkt 19:60 (tanh (make-rectangular 1.4291365847030308e-64 -0.76987815f0)) -- unbox float-complex
TR opt: float-complex-float.rkt 20:16 0.9845773f0 -- non float complex in complex ops
TR opt: float-complex-float.rkt 20:28 (make-rectangular 3 0.0) -- make-rectangular elimination
TR opt: float-complex-float.rkt 20:3 (* (min 3/4) 0.9845773f0 (make-rectangular 3 0.0)) -- unboxed binary float complex
TR opt: float-complex-float.rkt 20:6 (min 3/4) -- non float complex in complex ops
TR opt: float-complex-float.rkt 20:6 (min 3/4) -- unary number
TR opt: float-complex-float.rkt 21:20 (floor -2.2441852f0) -- non float complex in complex ops
TR opt: float-complex-float.rkt 21:3 (/ 3.2993203f+37 (floor -2.2441852f0) (make-polar 0.42484267570553375 4.940078147009648)) -- unboxed binary float complex
TR opt: float-complex-float.rkt 21:41 (make-polar 0.42484267570553375 4.940078147009648) -- make-rectangular elimination
TR opt: float-complex-float.rkt 21:6 3.2993203f+37 -- non float complex in complex ops
TR opt: float-complex-float.rkt 22:0 (/ -5 2/7 (make-polar -0.0 (fltan (real->double-flonum -3.833043f+21)))) -- unboxed binary float complex
TR opt: float-complex-float.rkt 22:10 (make-polar -0.0 (fltan (real->double-flonum -3.833043f+21))) -- make-rectangular elimination
TR opt: float-complex-float.rkt 22:27 (fltan (real->double-flonum -3.833043f+21)) -- unary float
TR opt: float-complex-float.rkt 22:3 -5 -- non float complex in complex ops
TR opt: float-complex-float.rkt 22:6 2/7 -- non float complex in complex ops
TR opt: float-complex-float.rkt 23:0 (/ (+ (exact-round 1.8655746f+35) (exact-round 1)) 2.0324421f-21 (make-rectangular 4 1.7976931348623157e+308)) -- unboxed binary float complex
TR opt: float-complex-float.rkt 23:3 (+ (exact-round 1.8655746f+35) (exact-round 1)) -- non float complex in complex ops
TR opt: float-complex-float.rkt 23:34 (exact-round 1) -- non float complex in complex ops
TR opt: float-complex-float.rkt 23:51 2.0324421f-21 -- non float complex in complex ops
TR opt: float-complex-float.rkt 23:6 (exact-round 1.8655746f+35) -- non float complex in complex ops
TR opt: float-complex-float.rkt 23:6 (exact-round 1.8655746f+35) -- non float complex in complex ops
TR opt: float-complex-float.rkt 23:65 (make-rectangular 4 1.7976931348623157e+308) -- make-rectangular elimination
TR opt: float-complex-float.rkt 24:0 (+ +inf.0-0.0i +nan.0) -- unboxed binary float complex
TR opt: float-complex-float.rkt 24:15 +nan.0 -- float in complex ops
TR opt: float-complex-float.rkt 24:3 +inf.0-0.0i -- unboxed literal
TR opt: float-complex-float.rkt 25:0 (+ (- 0.0 16 -inf.0+0.0i) +nan.0) -- unboxed binary float complex
TR opt: float-complex-float.rkt 25:10 16 -- non float complex in complex ops
TR opt: float-complex-float.rkt 25:13 -inf.0+0.0i -- unboxed literal
TR opt: float-complex-float.rkt 25:26 +nan.0 -- float in complex ops
TR opt: float-complex-float.rkt 25:3 (- 0.0 16 -inf.0+0.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 25:6 0.0 -- float in complex ops
TR opt: float-complex-float.rkt 26:0 (+ (floor (+ (exact-round -25.263502f0) (exact-round -1/2))) (- 0.0 16 (make-rectangular -inf.0 0.0)) +nan.0) -- unboxed binary float complex
TR opt: float-complex-float.rkt 26:102 +nan.0 -- float in complex ops
TR opt: float-complex-float.rkt 26:3 (floor (+ (exact-round -25.263502f0) (exact-round -1/2))) -- non float complex in complex ops
TR opt: float-complex-float.rkt 26:61 (- 0.0 16 (make-rectangular -inf.0 0.0)) -- unboxed binary float complex
TR opt: float-complex-float.rkt 26:64 0.0 -- float in complex ops
TR opt: float-complex-float.rkt 26:68 16 -- non float complex in complex ops
TR opt: float-complex-float.rkt 26:71 (make-rectangular -inf.0 0.0) -- make-rectangular elimination
TR opt: float-complex-float.rkt 27:18 3 -- non float complex in complex ops
TR opt: float-complex-float.rkt 27:20 -741.732021720279+inf.0i -- unboxed literal
TR opt: float-complex-float.rkt 27:3 (- 0.8214678f0 3 -741.732021720279+inf.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 27:6 0.8214678f0 -- non float complex in complex ops
TR opt: float-complex-float.rkt 28:100 (log 7.4109846876187e-323) -- unary float
TR opt: float-complex-float.rkt 28:11 -0.17853218f0 -- non float complex in complex ops
TR opt: float-complex-float.rkt 28:127 (abs -inf.0) -- unary float
TR opt: float-complex-float.rkt 28:26 (bitwise-ior (max (exact-round 3)) (exact-round 0.0f0)) -- binary fixnum
TR opt: float-complex-float.rkt 28:26 (bitwise-ior (max (exact-round 3)) (exact-round 0.0f0)) -- non float complex in complex ops
TR opt: float-complex-float.rkt 28:3 (- (+ 1 -0.17853218f0) (bitwise-ior (max (exact-round 3)) (exact-round 0.0f0)) (make-rectangular (log 7.4109846876187e-323) (abs -inf.0))) -- unboxed binary float complex
TR opt: float-complex-float.rkt 28:39 (max (exact-round 3)) -- unary number
TR opt: float-complex-float.rkt 28:6 (+ 1 -0.17853218f0) -- non float complex in complex ops
TR opt: float-complex-float.rkt 28:82 (make-rectangular (log 7.4109846876187e-323) (abs -inf.0)) -- make-rectangular elimination
TR opt: float-complex-float.rkt 28:9 1 -- non float complex in complex ops
TR opt: float-complex-float.rkt 28:9 1 -- non float complex in complex ops
END
#<<END
6.0+8.0i
-4.0-10.0i
-4.0-4.0i
-4.0-2.0i
+nan.0+0.0i
-inf.0 -1.06884033
+nan.0+0.0i
-0.84147098 -0.00000000
1524588609536.00000000 -0.96943193
2.21529901 0.00000000
-5843304152956620201721379414591143936.00000000 -25218488117536270901301864345842483200.00000000
-inf.0-0.0i
+nan.0+nan.0i
+nan.0-0.0i
+nan.0-0.0i
+nan.0-0.0i
739.55348960 -inf.0
739.55348960 -inf.0

END
#lang typed/scheme
#:optimize
#reader typed-racket-test/optimizer/reset-port

(require racket/flonum)

(define (p [n : Number])
  (define r (real-part n))
  (define i (imag-part n))
  (printf "~a ~a\n"
          (if (infinite? r) r (real->decimal-string r 8))
          (if (infinite? i) i (real->decimal-string i 8))))

(+ 1.0+2.0i 2.0 3.0+6.0i)
(- 1.0 2.0+4.0i 3.0+6.0i)
(- 1.0+2.0i 2.0 3.0+6.0i)
(- 1.0+2.0i 2.0+4.0i 3.0)
(/ 0.0 +inf.0-1.0i)
(p (* -0.9263371220283309 3/2 (make-rectangular +inf.f 0.7692234292042541)))
(/ 2.3454025f0 (flmin (real->double-flonum 1.797693134862315e+308) (real->double-flonum -1.2848677f+32)) (make-rectangular +nan.0 0.0))
(p (+ (make-polar 4.8063810141303426e-57 -1.9082319f0) -0.8414709848078965))
(p (+ 1.5245886f+12 (max (exact-round 2) (exact-round 5/4)) (tanh (make-rectangular 1.4291365847030308e-64 -0.76987815f0))))
(p (* (min 3/4) 0.9845773f0 (make-rectangular 3 0.0)))
(p (/ 3.2993203f+37 (floor -2.2441852f0) (make-polar 0.42484267570553375 4.940078147009648)))
(/ -5 2/7 (make-polar -0.0 (fltan (real->double-flonum -3.833043f+21))))
(/ (+ (exact-round 1.8655746f+35) (exact-round 1)) 2.0324421f-21 (make-rectangular 4 1.7976931348623157e+308))
(+ +inf.0-0.0i +nan.0)
(+ (- 0.0 16 -inf.0+0.0i) +nan.0)
(+ (floor (+ (exact-round -25.263502f0) (exact-round -1/2))) (- 0.0 16 (make-rectangular -inf.0 0.0)) +nan.0)
(p (- 0.8214678f0 3 -741.732021720279+inf.0i))
(p (- (+ 1 -0.17853218f0) (bitwise-ior (max (exact-round 3)) (exact-round 0.0f0)) (make-rectangular (log 7.4109846876187e-323) (abs -inf.0))))
