#;#;
#<<END
TR opt: box.rkt 4:0 (unbox x) -- box
TR opt: box.rkt 6:0 (unbox x) -- box
END
#<<END
1
2

END

#lang typed/scheme
#:optimize
#reader typed-racket-test/optimizer/reset-port

(: x (Boxof Integer))
(define x (box 1))
(unbox x)
(set-box! x 2)
(unbox x)
