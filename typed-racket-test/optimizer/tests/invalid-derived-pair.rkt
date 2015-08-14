#;#;
#<<END
TR missed opt: invalid-derived-pair.rkt 10:7 cadr -- car/cdr on a potentially empty list -- caused by: 10:12 x
TR missed opt: invalid-derived-pair.rkt 5:3 cadr -- car/cdr on a potentially empty list -- caused by: 5:8 x
TR missed opt: invalid-derived-pair.rkt 5:3 cadr -- car/cdr on a potentially empty list -- caused by: 5:8 x
TR opt: invalid-derived-pair.rkt 10:7 cadr -- pair
END
""
#lang typed/racket #:optimize
#reader typed-racket-test/optimizer/reset-port

;; can't optimize, the lists may not be long enough
(: f ((Listof Integer) -> Integer))
(define (f x)
  (cadr x))
(: g ((Listof Integer) -> Integer))
(define (g x)
  (if (null? x)
      0
      (cadr x)))
