#lang racket/load

;; Test for PR 13878, ensure that this doesn't produce
;; an internal type-checker error

(require typed/racket/shallow)
(begin-for-syntax 3)

