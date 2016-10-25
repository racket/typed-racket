#lang typed/racket/base

;; verify the casts in this file succeed
;; i.e. the calls to 'intersect' shouldn't not
;; generate intersection types

(define-type Json1 (Rec Json1 (U (Listof Json1) (HashTable Symbol Json1))))

(: scan1 (Json1 -> (Listof (HashTable Symbol Json1))))
(define (scan1 items-js)
  (if (and (list? items-js) (andmap hash? items-js))
      (cast items-js (Listof (HashTable Symbol Json1)))
      (list)))

(define-type Json2 (Rec Json2 (U Null (Pairof Json2 (Listof Json2)) (HashTable Symbol Json2))))

(: scan2 (Json2 -> (Listof (HashTable Symbol Json2))))
(define (scan2 items-js)
  (if (and (list? items-js) (andmap hash? items-js))
      (cast items-js (Listof (HashTable Symbol Json2)))
      (list)))