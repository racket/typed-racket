#;#;
#<<END
TR opt: flhypot.rkt 3:0 (flhypot 3.0 4.0) -- binary float
TR opt: flhypot.rkt 4:0 (magnitude 3.0+4.0i) -- unboxed unary float complex
TR opt: flhypot.rkt 4:11 3.0+4.0i -- unboxed literal
END
#<<END
5.0
5.0

END
#lang typed/racket/base
#:optimize
#reader typed-racket-test/optimizer/reset-port

(require racket/flonum)
(flhypot 3.0 4.0)
(magnitude 3.0+4.0i)
