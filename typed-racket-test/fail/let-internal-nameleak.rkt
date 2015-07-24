#;
(exn-pred exn:fail:syntax? #rx"let: expected identifier")
#lang typed/racket
(let ([() 5]) (void))
