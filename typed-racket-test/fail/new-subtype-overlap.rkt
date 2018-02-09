#;
(exn-pred 2)
#lang typed/racket/base

(define-new-subtype New-Int (new-int Integer))

;; checks that the intersection of Int and New-Int is non-empty
;; see https://github.com/racket/typed-racket/issues/662
(ann (integer? (new-int 42)) Nothing)
(ann (exact-integer? (new-int 42)) Nothing)
 