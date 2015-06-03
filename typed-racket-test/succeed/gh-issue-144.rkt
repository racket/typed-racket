#lang typed/racket

(define-type F (-> String F String))

(: f F)
(define (f x g) (string-append x))
