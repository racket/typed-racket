#lang racket/base

;; (type-out (struct ... #:omit-constructor))
;; Makes the struct constructor invisible

(module omit-constructor typed/racket/base
  (provide
    (type-out (struct foo () #:omit-constructor))))

(require 'omit-constructor)
foo
