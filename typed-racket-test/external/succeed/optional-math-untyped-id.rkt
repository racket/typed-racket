#lang typed/racket/optional

;; TODO uncomment after merging https://github.com/racket/math/pull/68

;;;; Test math-lib identifiers created with `define-typed/untyped-id`
;;
;;(require
;;  typed/rackunit
;;  (only-in math/array
;;           array?
;;           list*->array
;;           vector*->array
;;           array-map)
;;  (only-in math/matrix
;;           matrix?
;;           list->matrix
;;           matrix-transpose
;;           matrix-map
;;           matrix*
;;           matrix+
;;           matrix-
;;           matrix-scale
;;           matrix-sum))
;;
;;(void
;;  list*->array
;;  vector*->array
;;  array-map
;;  matrix-map
;;  matrix*
;;  matrix+
;;  matrix-
;;  matrix-scale
;;  matrix-sum)
;;
;;(define a0 (list*->array '((5 2 3) (4 1 0)) natural?))
;;(define a1 (vector*->array '#(#(5 2 3) #(4 1 0)) natural?))
;;
;;(void
;;  (array-map number->string a0)
;;  (array-map number->string a1))
;;
;;(define m0 (list->matrix 2 3 '(5 2 3 4 1 0)))
;;
;;(void
;;  (matrix-map number->string m0)
;;  (matrix* m0 (matrix-transpose m0))
;;  (matrix+ m0)
;;  (matrix- m0 m0)
;;  (matrix-scale m0 42)
;;  (matrix-sum (list m0 m0)))
;;
