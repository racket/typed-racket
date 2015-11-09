#;
(exn-pred "division by zero")
#lang typed/racket

;; float-complex opts should not make div-by-0 errors go away
(* (/ (make-rectangular +inf.f -1.3086925781062948e-124)
      (exp 2)
      (bitwise-and (exact-round -3/11)))
   -4.880003479031522e-08)
