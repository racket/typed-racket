#;
(exn-pred exn:fail:contract?)
#lang scheme
(require typed/racket/shallow)

((with-type #:result (Number -> Number)
   (lambda: ([x : Number]) (add1 x)))
 #f)
