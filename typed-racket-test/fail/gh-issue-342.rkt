#;
(exn-pred 2)
#lang typed/racket

(: f (case→ (→ (→ (Values Any Any)) Any)
            (→ 'sym Any)))
(define (f g) (g))
