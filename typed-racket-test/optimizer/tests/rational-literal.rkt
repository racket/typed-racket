#;#;
#<<END
TR opt: rational-literal.rkt 2:0 (+ 3/4 1.2) -- binary float
END
#<<END
1.95

END
#lang typed/racket #:optimize
#reader typed-racket-test/optimizer/reset-port
;; rational literals should be promoted to floats at compile time
(+ 3/4 1.2)
