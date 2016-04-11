#lang typed/racket/base #:no-optimize
(require typed/racket/class)

(define c%
  (class object%
    (init-field val)
    (super-new)))

(instantiate c% (3))
