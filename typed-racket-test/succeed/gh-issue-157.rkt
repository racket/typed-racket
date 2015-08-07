#lang typed/racket

;; These aliases should be allowed because there's no
;; polymorphic recursion
;;
;; Tests for GH issue #157

(define-type (a T U) (-> (b U)))
(define-type (b T)   (-> (a T Integer)))
(define-type (c T)   (a (c T) Number))
