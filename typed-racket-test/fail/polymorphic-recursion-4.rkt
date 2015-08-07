#;
(exn-pred #rx"applied at a different type")
#lang typed/racket

;; These aliases should be not allowed because there's
;; polymorphic recursion
;;
;; Related to GH issue #157

(define-type (a T U) (-> (b U)))
(define-type (b T)   (-> (a (Listof T) (Listof T))))

;; Because the check is conservative, there are some types
;; that are disallowed even if they are ok. Here is one
;; example:

(define-type (c T U) (-> (d U)))
;; ok because (Listof T) is thrown away
(define-type (d T)   (-> (c (Listof T) Integer)))
