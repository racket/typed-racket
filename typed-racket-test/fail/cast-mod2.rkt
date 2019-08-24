#;
(exn-pred exn:fail:contract? #rx".*produced: 3" #rx".*promised: string?.*" )

#lang typed/racket/base

((cast (lambda () 3) (-> String)))

