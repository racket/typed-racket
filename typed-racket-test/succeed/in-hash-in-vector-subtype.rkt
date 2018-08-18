#lang typed/racket/base

;; Adapted from SCV:
;;   https://github.com/philnguyen/soft-contract

;; This program should type-check.
;; (At one point, it didn't typecheck because of the way Typed Racket's
;;  constraint generation worked with subtypes and unions.)

(define-new-subtype VB (make-VB (Vectorof Boolean)))
(let ()
  (define x : VB (make-VB '#(#f #f #t)))
  (for ([b : Boolean (in-vector x)]) b))

(define-new-subtype HB (make-HB (HashTable Boolean Boolean)))
(let ()
  (define x : HB (make-HB (hash #t #f #f #t)))
  (for ([(k v) (in-hash x)]) (ann k Boolean) (ann v Boolean)))
