#;#;
#<<END
TR opt: gh60.rkt 13:2 (car x) -- pair
TR opt: gh60.rkt 20:2 (car x) -- pair
TR opt: gh60.rkt 28:6 (car x) -- pair
TR opt: gh60.rkt 29:6 (quote error) -- dead else branch
TR opt: gh60.rkt 6:2 (car x) -- pair
END
""

#lang typed/racket #:optimize
#reader typed-racket-test/optimizer/reset-port

(define-type foo (List Symbol Any))

(: foo-car (foo . -> . Symbol))
(define (foo-car x)
  (car x))


(define-type bar (List Symbol (U String bar)))

(: bar-car (bar . -> . Symbol))
(define (bar-car x)
  (car x))


(define-type zam (Rec z (List Symbol (U String z))))

(: zam-car (zam . -> . Symbol))
(define (zam-car x)
  (car x))


(define-type bar2 (List Symbol (U String bar)))

(: bar2-car (bar2 . -> . Symbol))
(define (bar2-car x)
  (if (not (null? x))
      (car x)
      'error))
