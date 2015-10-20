#lang racket/load

;; A test for GH issue #220

(require typed/racket)

(class object% (super-new) (+ 1 "foo"))
(unit (import) (export) (+ 1 "foo"))
