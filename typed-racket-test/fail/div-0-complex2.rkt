#;
(exn-pred "division by zero")
#lang typed/racket

;; float-complex opts should not make div-by-0 errors go away
(* (/ 1.0+1.0i (ann 0 Integer) (ann 0 Integer)) -4.880003479031522e-08)
