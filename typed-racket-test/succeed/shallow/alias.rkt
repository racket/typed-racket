#lang typed/racket/base/shallow

;; Test trusted vs. untrusted primitives
;; - + is a trusted identifier, its result is not checked
;; - (begin +) is not a trusted identifier --- even though it evaluates to
;;   a function that could be trusted --- its result is checked

(+ 2 2)
((begin +) 2 2)
