#;#;
#<<END
TR missed opt: invalid-sqrt.rkt 1:11 (sqrt -2.0) -- unexpected complex type
END
#<<END
1.4142135623730951

END
#lang typed/scheme
#:optimize
#reader typed-racket-test/optimizer/reset-port
(imag-part (sqrt -2.0)) ; not a nonnegative flonum, can't optimize
