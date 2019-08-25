#;
(exn-pred exn:fail:contract? #rx".*produced: 3" #rx".*promised: string?.*" #rx"6\\.0")

#lang typed/racket/base

(cast 3 String)
