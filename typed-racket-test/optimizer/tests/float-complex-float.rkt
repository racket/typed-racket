#;#;
#<<END
TR info: float-complex-float.rkt 7:3 printf -- hidden parameter
TR missed opt: float-complex-float.rkt 22:6 (make-polar 4.8063810141303426e-57 -1.9082319f0) -- non-complex value in complex arithmetic
TR missed opt: float-complex-float.rkt 23:66 (make-rectangular 1.4291365847030308e-64 -0.76987815f0) -- generic comparison -- caused by: 23:107 -0.76987815f0
TR missed opt: float-complex-float.rkt 25:20 (floor -2.2441852f0) -- all args float-arg-expr, result not Float -- caused by: 25:27 -2.2441852f0
TR missed opt: float-complex-float.rkt 32:6 (+ 1 -0.17853218f0) -- all args float-arg-expr, result not Float -- caused by: 32:9 1, 32:11 -0.17853218f0
TR opt: float-complex-float.rkt 15:0 (+ 1.0+2.0i 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 15:12 2.0 -- float in complex ops
TR opt: float-complex-float.rkt 15:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float.rkt 15:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float.rkt 16:0 (- 1.0 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 16:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float.rkt 16:3 1.0 -- float in complex ops
TR opt: float-complex-float.rkt 16:7 2.0+4.0i -- unboxed literal
TR opt: float-complex-float.rkt 17:0 (- 1.0+2.0i 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 17:12 2.0 -- float in complex ops
TR opt: float-complex-float.rkt 17:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float.rkt 17:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float.rkt 18:0 (- 1.0+2.0i 2.0+4.0i 3.0) -- unboxed binary float complex
TR opt: float-complex-float.rkt 18:12 2.0+4.0i -- unboxed literal
TR opt: float-complex-float.rkt 18:21 3.0 -- float in complex ops
TR opt: float-complex-float.rkt 18:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float.rkt 19:0 (/ 0.0 +inf.0-1.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 19:3 0.0 -- float in complex ops
TR opt: float-complex-float.rkt 19:7 +inf.0-1.0i -- unboxed literal
TR opt: float-complex-float.rkt 20:26 3/2 -- non float complex in complex ops
TR opt: float-complex-float.rkt 20:3 (* -0.9263371220283309 3/2 (make-rectangular +inf.f 0.7692234292042541)) -- unboxed binary float complex
TR opt: float-complex-float.rkt 20:30 (make-rectangular +inf.f 0.7692234292042541) -- make-rectangular elimination
TR opt: float-complex-float.rkt 20:6 -0.9263371220283309 -- float in complex ops
TR opt: float-complex-float.rkt 21:0 (/ 2.3454025f0 (flmin (real->double-flonum 1.797693134862315e+308) (real->double-flonum -1.2848677f+32)) (make-rectangular +nan.0 0.0)) -- unboxed binary float complex
TR opt: float-complex-float.rkt 21:105 (make-rectangular +nan.0 0.0) -- make-rectangular elimination
TR opt: float-complex-float.rkt 21:15 (flmin (real->double-flonum 1.797693134862315e+308) (real->double-flonum -1.2848677f+32)) -- binary float
TR opt: float-complex-float.rkt 21:15 (flmin (real->double-flonum 1.797693134862315e+308) (real->double-flonum -1.2848677f+32)) -- float in complex ops
TR opt: float-complex-float.rkt 21:22 (real->double-flonum 1.797693134862315e+308) -- float to float
TR opt: float-complex-float.rkt 21:3 2.3454025f0 -- non float complex in complex ops
TR opt: float-complex-float.rkt 22:3 (+ (make-polar 4.8063810141303426e-57 -1.9082319f0) -0.8414709848078965) -- unboxed binary float complex
TR opt: float-complex-float.rkt 22:55 -0.8414709848078965 -- float in complex ops
TR opt: float-complex-float.rkt 22:6 (make-polar 4.8063810141303426e-57 -1.9082319f0) -- non float complex in complex ops
TR opt: float-complex-float.rkt 23:20 (max (exact-round 2) (exact-round 5/4)) -- non float complex in complex ops
TR opt: float-complex-float.rkt 23:3 (+ 1.5245886f+12 (max (exact-round 2) (exact-round 5/4)) (tanh (make-rectangular 1.4291365847030308e-64 -0.76987815f0))) -- unboxed binary float complex
TR opt: float-complex-float.rkt 23:6 1.5245886f+12 -- non float complex in complex ops
TR opt: float-complex-float.rkt 23:60 (tanh (make-rectangular 1.4291365847030308e-64 -0.76987815f0)) -- unbox float-complex
TR opt: float-complex-float.rkt 24:16 0.9845773f0 -- non float complex in complex ops
TR opt: float-complex-float.rkt 24:28 (make-rectangular 3 0.0) -- make-rectangular elimination
TR opt: float-complex-float.rkt 24:3 (* (min 3/4) 0.9845773f0 (make-rectangular 3 0.0)) -- unboxed binary float complex
TR opt: float-complex-float.rkt 24:6 (min 3/4) -- non float complex in complex ops
TR opt: float-complex-float.rkt 24:6 (min 3/4) -- unary number
TR opt: float-complex-float.rkt 25:20 (floor -2.2441852f0) -- non float complex in complex ops
TR opt: float-complex-float.rkt 25:3 (/ 3.2993203f+37 (floor -2.2441852f0) (make-polar 0.42484267570553375 4.940078147009648)) -- unboxed binary float complex
TR opt: float-complex-float.rkt 25:41 (make-polar 0.42484267570553375 4.940078147009648) -- make-rectangular elimination
TR opt: float-complex-float.rkt 25:6 3.2993203f+37 -- non float complex in complex ops
TR opt: float-complex-float.rkt 26:0 (/ -5 2/7 (make-polar -0.0 (fltan (real->double-flonum -3.833043f+21)))) -- unboxed binary float complex
TR opt: float-complex-float.rkt 26:10 (make-polar -0.0 (fltan (real->double-flonum -3.833043f+21))) -- make-rectangular elimination
TR opt: float-complex-float.rkt 26:27 (fltan (real->double-flonum -3.833043f+21)) -- unary float
TR opt: float-complex-float.rkt 26:3 -5 -- non float complex in complex ops
TR opt: float-complex-float.rkt 26:6 2/7 -- non float complex in complex ops
TR opt: float-complex-float.rkt 27:0 (/ (+ (exact-round 1.8655746f+35) (exact-round 1)) 2.0324421f-21 (make-rectangular 4 1.7976931348623157e+308)) -- unboxed binary float complex
TR opt: float-complex-float.rkt 27:3 (+ (exact-round 1.8655746f+35) (exact-round 1)) -- non float complex in complex ops
TR opt: float-complex-float.rkt 27:34 (exact-round 1) -- non float complex in complex ops
TR opt: float-complex-float.rkt 27:51 2.0324421f-21 -- non float complex in complex ops
TR opt: float-complex-float.rkt 27:6 (exact-round 1.8655746f+35) -- non float complex in complex ops
TR opt: float-complex-float.rkt 27:6 (exact-round 1.8655746f+35) -- non float complex in complex ops
TR opt: float-complex-float.rkt 27:65 (make-rectangular 4 1.7976931348623157e+308) -- make-rectangular elimination
TR opt: float-complex-float.rkt 28:0 (+ +inf.0-0.0i +nan.0) -- unboxed binary float complex
TR opt: float-complex-float.rkt 28:15 +nan.0 -- float in complex ops
TR opt: float-complex-float.rkt 28:3 +inf.0-0.0i -- unboxed literal
TR opt: float-complex-float.rkt 29:0 (+ (- 0.0 16 -inf.0+0.0i) +nan.0) -- unboxed binary float complex
TR opt: float-complex-float.rkt 29:10 16 -- non float complex in complex ops
TR opt: float-complex-float.rkt 29:13 -inf.0+0.0i -- unboxed literal
TR opt: float-complex-float.rkt 29:26 +nan.0 -- float in complex ops
TR opt: float-complex-float.rkt 29:3 (- 0.0 16 -inf.0+0.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 29:6 0.0 -- float in complex ops
TR opt: float-complex-float.rkt 30:0 (+ (floor (+ (exact-round -25.263502f0) (exact-round -1/2))) (- 0.0 16 (make-rectangular -inf.0 0.0)) +nan.0) -- unboxed binary float complex
TR opt: float-complex-float.rkt 30:102 +nan.0 -- float in complex ops
TR opt: float-complex-float.rkt 30:3 (floor (+ (exact-round -25.263502f0) (exact-round -1/2))) -- non float complex in complex ops
TR opt: float-complex-float.rkt 30:61 (- 0.0 16 (make-rectangular -inf.0 0.0)) -- unboxed binary float complex
TR opt: float-complex-float.rkt 30:64 0.0 -- float in complex ops
TR opt: float-complex-float.rkt 30:68 16 -- non float complex in complex ops
TR opt: float-complex-float.rkt 30:71 (make-rectangular -inf.0 0.0) -- make-rectangular elimination
TR opt: float-complex-float.rkt 31:18 3 -- non float complex in complex ops
TR opt: float-complex-float.rkt 31:20 -741.732021720279+inf.0i -- unboxed literal
TR opt: float-complex-float.rkt 31:3 (- 0.8214678f0 3 -741.732021720279+inf.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 31:6 0.8214678f0 -- non float complex in complex ops
TR opt: float-complex-float.rkt 32:100 (log 7.4109846876187e-323) -- unary float
TR opt: float-complex-float.rkt 32:11 -0.17853218f0 -- non float complex in complex ops
TR opt: float-complex-float.rkt 32:127 (abs -inf.0) -- unary float
TR opt: float-complex-float.rkt 32:26 (bitwise-ior (max (exact-round 3)) (exact-round 0.0f0)) -- binary fixnum
TR opt: float-complex-float.rkt 32:26 (bitwise-ior (max (exact-round 3)) (exact-round 0.0f0)) -- non float complex in complex ops
TR opt: float-complex-float.rkt 32:3 (- (+ 1 -0.17853218f0) (bitwise-ior (max (exact-round 3)) (exact-round 0.0f0)) (make-rectangular (log 7.4109846876187e-323) (abs -inf.0))) -- unboxed binary float complex
TR opt: float-complex-float.rkt 32:39 (max (exact-round 3)) -- unary number
TR opt: float-complex-float.rkt 32:6 (+ 1 -0.17853218f0) -- non float complex in complex ops
TR opt: float-complex-float.rkt 32:82 (make-rectangular (log 7.4109846876187e-323) (abs -inf.0)) -- make-rectangular elimination
TR opt: float-complex-float.rkt 32:9 1 -- non float complex in complex ops
TR opt: float-complex-float.rkt 32:9 1 -- non float complex in complex ops
END
#<<END
6.0+8.0i
-4.0-10.0i
-4.0-4.0i
-4.0-2.0i
+nan.0+0.0i
-inf.0 -1.06884033e+00
+nan.0+0.0i
-8.41470985e-01 -4.53533378e-57
1.52458861e+12 -9.69431934e-01
2.21529901e+00 0.0
-5.84330415e+36 -2.52184881e+37
-inf.0-0.0i
+nan.0+nan.0i
+nan.0-0.0i
+nan.0-0.0i
+nan.0-0.0i
7.3955349e+02 -inf.0
7.3955349e+02 -inf.0

END
#lang typed/scheme
#:optimize
#reader typed-racket-test/optimizer/reset-port

(require racket/flonum racket/format)

(define (p [n : Number])
  (define r (real-part n))
  (define i (imag-part n))
  (printf "~a ~a\n"
          (if (or (infinite? r) (nan? r) (zero? r))
              r
              (~r r #:precision 8 #:notation 'exponential))
          (if (or (infinite? i) (nan? i) (zero? i))
              i
              (~r i #:precision 8 #:notation 'exponential))))

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
