#;
(exn-pred exn:fail:syntax?
          #rx"Type \\(List X\\) could not be converted to a contract"
          #rx"contains free variables"
          #rx"10.8")

#lang typed/racket
(: f : (All (X) (-> X X)))
(define (f x)
  (cast (list x) Any)
  x)
