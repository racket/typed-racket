#;#;
"TR info: fixnum-real-to-double-flonum.rkt 6:1 displayln -- hidden parameter\nTR info: fixnum-real-to-double-flonum.rkt 7:1 displayln -- hidden parameter\nTR opt: fixnum-real-to-double-flonum.rkt 4:2 (real->double-flonum n) -- fixnum to float\n"
"42.0\n-42.0\n"
#lang typed/racket
#:optimize
#reader typed-racket-test/optimizer/reset-port

(: convert (-> Fixnum Flonum))
(define (convert n)
  (real->double-flonum n))

(displayln (convert 42))
(displayln (convert -42))
