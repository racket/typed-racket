#;#;
#<<END
TR opt: simple-pair.rkt 2:0 (car (cons 1 2)) -- pair
END
#<<END
1

END
#lang typed/scheme
#:optimize
#reader typed-racket-test/optimizer/reset-port

(car (cons 1 2))
