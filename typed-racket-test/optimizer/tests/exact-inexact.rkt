#;#;
#<<END
TR opt: exact-inexact.rkt 2:0 (exact->inexact (expt 10 100)) -- int to float
TR opt: exact-inexact.rkt 3:0 (round (exact->inexact (expt 2.3 3.2))) -- unary float
TR opt: exact-inexact.rkt 3:23 (expt 2.3 3.2) -- binary float
TR opt: exact-inexact.rkt 3:7 (exact->inexact (expt 2.3 3.2)) -- float to float
TR opt: exact-inexact.rkt 4:0 (real->double-flonum (expt 10 100)) -- int to float
TR opt: exact-inexact.rkt 5:0 (round (real->double-flonum (expt 2.3 3.2))) -- unary float
TR opt: exact-inexact.rkt 5:28 (expt 2.3 3.2) -- binary float
TR opt: exact-inexact.rkt 5:7 (real->double-flonum (expt 2.3 3.2)) -- float to float
END
#<<END
1e+100
14.0
1e+100
14.0

END
#lang typed/scheme
#:optimize
#reader typed-racket-test/optimizer/reset-port

(exact->inexact (expt 10 100)) ; must not be a fixnum
(round (exact->inexact (expt 2.3 3.2))) ; already a float
(real->double-flonum (expt 10 100)) ; must not be a fixnum
(round (real->double-flonum (expt 2.3 3.2))) ; already a float
