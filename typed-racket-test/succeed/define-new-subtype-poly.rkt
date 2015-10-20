#lang typed/racket/base

(define-new-subtype (Meters a) (meters a))

(: m+ : (case-> [(Meters Real) (Meters Real) -> (Meters Real)]
                [(Meters Number) (Meters Number) -> (Meters Number)]))
(define (m+ a b)
  (meters (+ a b)))

(define x1 (m+ (meters 1) (meters 2)))
(ann x1 (Meters Real))
(define x2 (m+ (meters (ann 1 Number)) (meters 2)))
(ann x2 (Meters Number))
