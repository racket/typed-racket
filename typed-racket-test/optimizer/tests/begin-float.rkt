#;#;
#<<END
TR opt: begin-float.rkt 2:7 (- 2.0 3.0) -- binary float
TR opt: begin-float.rkt 3:7 (* 2.0 3.0) -- binary float
END
#<<END
-1.0
6.0

END

#lang typed/scheme
#:optimize
#reader typed-racket-test/optimizer/reset-port

(begin (- 2.0 3.0)
       (* 2.0 3.0))
