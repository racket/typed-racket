#;
(exn-pred exn:fail:syntax? "type mismatch")
#lang typed/racket
(provide
 (contract-out
  [doubler (->/c string? string?)]))

(: doubler : Integer -> Integer)
(define (doubler x)
  (+ x x))
