#;
(exn-pred exn:fail:contract? #rx"shape-check")
#lang typed/racket/shallow
(require/typed racket/base [list (All (a) Float)])
(* 3.3 list)

