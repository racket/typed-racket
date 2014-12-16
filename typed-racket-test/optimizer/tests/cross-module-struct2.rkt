#;#;
#<<END
TR opt: cross-module-struct2.rkt 4:0 (x-x a) -- struct ref
END
#<<END
1

END
#lang typed/scheme #:optimize
#reader typed-racket-test/optimizer/reset-port

(require (file "cross-module-struct.rkt"))
(define a (make-x 1))
(x-x a)
