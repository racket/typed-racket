#;
(exn-pred #rx"missing type for top-level")
#lang racket/load

;; Test that built-in struct fields don't have types for setters

(require typed/racket/base)
set-date-second!
