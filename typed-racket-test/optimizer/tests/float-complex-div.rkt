#;#;
#<<END
END
#<<END
0.03333333333333333-0.06666666666666667i

END
#lang typed/scheme
#:optimize
#reader typed-racket-test/optimizer/reset-port

(/ 1.0+2.0i 2.0+4.0i 3.0+6.0i)
