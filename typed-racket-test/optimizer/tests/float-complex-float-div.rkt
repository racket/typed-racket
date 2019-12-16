#;#;
#<<END
TR opt: float-complex-float-div.rkt 4:62 x -- unbox float-complex
TR opt: float-complex-float-div.rkt 4:51 (imag-part x) -- complex accessor elimination
TR opt: float-complex-float-div.rkt 3:62 x -- unbox float-complex
TR opt: float-complex-float-div.rkt 3:51 (real-part x) -- complex accessor elimination
END
#<<END
'("0.1000000000-0.2000000000" "0.50000000001.0000000000" "-0.0200000000-0.0266666667" "0.16666666670.0000000000" "0.16666666670.0000000000" "0.16666666670.3333333333" "0.0333333333-0.0666666667")

END
#lang typed/scheme
#:optimize
#reader typed-racket-test/optimizer/reset-port

(map (lambda: ((x : Float-Complex))
              (string-append (real->decimal-string (real-part x) 10)
                             (real->decimal-string (imag-part x) 10)))
     (list
      (/ 1.0 2.0+4.0i)
      (/ 1.0+2.0i 2.0)
      (/ 1.0 2.0+4.0i 3.0+6.0i)
      (/ 1.0+2.0i 2.0 3.0+6.0i)
      (/ 1.0+2.0i 2.0+4.0i 3.0)
      (/ 1.0+2.0i 2.0 3.0)
      (/ 1.0 2.0 3.0+6.0i)))
