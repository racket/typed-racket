#;
(exn-pred exn:fail:contract? #rx"shape-check")

#lang typed/racket/shallow

;; Bad cast, expect shape-check error

(cast 42 String)
