#;#;
#<<END
TR opt: known-length-vector-op.rkt 3:0 (vector-ref (vector (set! x (+ x 1))) 0) -- vector
END
#<<END
1

END
#lang typed/racket
#:optimize
#reader typed-racket-test/optimizer/reset-port

(define x 0)
(vector-ref (vector (set! x (+ x 1))) 0)
x
