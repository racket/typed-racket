#;#;
#<<END
TR opt: unboxed-make-rectangular.rkt 10:6 (x (make-flrectangular 1.0 2.0)) -- unboxed let bindings
TR opt: unboxed-make-rectangular.rkt 10:9 (make-flrectangular 1.0 2.0) -- make-rectangular elimination
TR opt: unboxed-make-rectangular.rkt 11:2 (+ x 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-make-rectangular.rkt 11:5 x -- leave var unboxed
TR opt: unboxed-make-rectangular.rkt 11:7 2.0+4.0i -- unboxed literal
TR opt: unboxed-make-rectangular.rkt 12:0 (real-part (make-flrectangular -0.0 2.0)) -- complex accessor elimination
TR opt: unboxed-make-rectangular.rkt 12:11 (make-flrectangular -0.0 2.0) -- make-rectangular elimination
TR opt: unboxed-make-rectangular.rkt 13:0 (imag-part (make-flrectangular 1.0 -0.0)) -- complex accessor elimination
TR opt: unboxed-make-rectangular.rkt 13:11 (make-flrectangular 1.0 -0.0) -- make-rectangular elimination
TR opt: unboxed-make-rectangular.rkt 5:6 (x (make-rectangular 1.0 2.0)) -- unboxed let bindings
TR opt: unboxed-make-rectangular.rkt 5:9 (make-rectangular 1.0 2.0) -- make-rectangular elimination
TR opt: unboxed-make-rectangular.rkt 6:2 (+ x 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-make-rectangular.rkt 6:5 x -- leave var unboxed
TR opt: unboxed-make-rectangular.rkt 6:7 2.0+4.0i -- unboxed literal
TR opt: unboxed-make-rectangular.rkt 7:6 (x (unsafe-make-flrectangular 1.0 2.0)) -- unboxed let bindings
TR opt: unboxed-make-rectangular.rkt 7:9 (unsafe-make-flrectangular 1.0 2.0) -- make-rectangular elimination
TR opt: unboxed-make-rectangular.rkt 8:2 (+ x 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-make-rectangular.rkt 8:5 x -- leave var unboxed
TR opt: unboxed-make-rectangular.rkt 8:7 2.0+4.0i -- unboxed literal
END
#<<END
3.0+6.0i
3.0+6.0i
3.0+6.0i
-0.0
-0.0

END
#lang typed/scheme
#:optimize
#reader typed-racket-test/optimizer/reset-port


(require racket/unsafe/ops racket/flonum)

(let ((x (make-rectangular 1.0 2.0)))
  (+ x 2.0+4.0i))
(let ((x (unsafe-make-flrectangular 1.0 2.0)))
  (+ x 2.0+4.0i))

(let ((x (make-flrectangular 1.0 2.0)))
  (+ x 2.0+4.0i))
(real-part (make-flrectangular -0.0 2.0))
(imag-part (make-flrectangular 1.0 -0.0))
