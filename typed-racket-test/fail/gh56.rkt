#lang typed/racket

(: f (Number [#:y Boolean] -> Number))
(define (f x #:y [y #f] #:z [z 'this-can-be-anything])
   (if y "y is truthy" x))
