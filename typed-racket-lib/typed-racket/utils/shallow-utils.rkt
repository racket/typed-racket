#lang racket/base

(provide
  shallow-trusted-positive?
  (all-from-out (submod typed-racket/rep/type-rep shallow-exports)))

(require
  racket/match
  typed-racket/types/match-expanders
  typed-racket/rep/type-rep
  (submod typed-racket/rep/type-rep shallow-exports)
  typed-racket/types/tc-result
  typed-racket/types/type-table
  typed-racket/types/union
  typed-racket/utils/tc-utils)


(define (ArrowT+? arr)
  (and (Arrow? arr) (Arrow-rng-shallow-safe? arr)))

(define (shallow-trusted-positive? orig-ty)
  (define in-shallow? (eq? shallow (current-type-enforcement-mode)))
  ;; 2022-12-12: normalize only in shallow mode to save time
  (let loop ((ty ((if in-shallow? normalize-type values) orig-ty)))
    (match ty
      [(Fun: arr*)
       (andmap ArrowT+? arr*)]
      [(Union: _ ts)
       (andmap loop ts)]
      [(or (Poly: _ b)
           (PolyDots-unsafe: _ b)
           (PolyRow-unsafe: b _))
       (loop b)]
      [_
        #false])))

