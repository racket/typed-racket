#lang racket/base

(provide prop:typed-method
         typed-method?)

(define-values (prop:typed-method
                typed-method?
                ;; the payload doesn't matter since this
                ;; property is only used to detect a boolean
                ;; property of the method
                _)
  (make-impersonator-property 'typed-method))
