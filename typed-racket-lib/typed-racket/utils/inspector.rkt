#lang racket/base

(provide old-inspector new-inspector)

;; Defines a new inspector that typed racket structs will use,
;; and make this new inspector a sub-inspector of the old one.
;; The old one is more powerfull than the new one, so any-wrap/c
;; can use the old one to inspect opaque structs created by
;; typed racket.
(define old-inspector (current-inspector))
(define new-inspector (make-inspector old-inspector))

