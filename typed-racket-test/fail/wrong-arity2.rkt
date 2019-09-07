#;
(exn-pred 2)
#lang typed/racket

;; https://github.com/racket/typed-racket/issues/58

(: f1 ([] [String] #:rest Symbol . ->* . Any))
(define (f1 [bool #t] . syms) bool syms)

(ann (λ ([bool #f] . syms) bool syms)
     ([] [String] #:rest Symbol . ->* . Any))
