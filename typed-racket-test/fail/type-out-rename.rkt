#lang racket/base

;; (type-out (rename ...))
;; Does not provide original identifier

(module rename typed/racket/base
  (provide
    (type-out (rename f g (-> Natural Natural))))
  (define (f n)
    (let ([n-1 (- n 1)])
      (if (positive? n-1) (* n (f n-1)) 1))))

(require 'rename)
(f 4)

