#;#;
#<<END
TR missed opt: derived-pair-open-terms.rkt 12:3 first -- car/cdr on a potentially empty list -- caused by: 12:9 x
TR missed opt: derived-pair-open-terms.rkt 16:3 rest -- car/cdr on a potentially empty list -- caused by: 16:8 x
TR missed opt: derived-pair-open-terms.rkt 20:3 cddr -- car/cdr on a potentially empty list -- caused by: 20:8 x
TR opt: derived-pair-open-terms.rkt 20:3 cddr -- pair
TR opt: derived-pair-open-terms.rkt 4:3 cadr -- pair
TR opt: derived-pair-open-terms.rkt 4:3 cadr -- pair
TR opt: derived-pair-open-terms.rkt 8:3 caddr -- pair
TR opt: derived-pair-open-terms.rkt 8:3 caddr -- pair
TR opt: derived-pair-open-terms.rkt 8:3 caddr -- pair
END
""

#lang typed/racket #:optimize
#reader typed-racket-test/optimizer/reset-port

(: f ((List Integer Integer Integer) -> Integer))
(define (f x)
  (cadr x))

(: g ((List Integer Integer Integer) -> Integer))
(define (g x)
  (caddr x))

(: h ((Listof Integer) -> Integer))
(define (h x)
  (first x)) ; unsafe

(: i ((Listof Integer) -> (Listof Integer)))
(define (i x)
  (rest x)) ; unsafe

(: j ((cons Integer (Listof Integer)) -> (Listof Integer)))
(define (j x)
  (cddr x)) ; partially safe
