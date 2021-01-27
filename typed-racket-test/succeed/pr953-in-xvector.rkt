#lang typed/racket/base

(require racket/sequence
         racket/flonum
         racket/fixnum
         racket/extflonum
         typed/rackunit)

(check-equal? (sequence->list (in-flvector (flvector 1.0 2.0 3.0 4.0))) (list 1.0 2.0 3.0 4.0))
(check-equal? (sequence->list (in-flvector (flvector 1.0 2.0 3.0 4.0) 1)) (list 2.0 3.0 4.0))
(check-equal? (sequence->list (in-flvector (flvector 1.0 2.0 3.0 4.0) 1 2)) (list 2.0))
(check-equal? (sequence->list (in-flvector (flvector 1.0 2.0 3.0 4.0) 0 #f 2)) (list 1.0 3.0))
(check-equal? (for/list : (Listof Flonum) ([i (in-flvector (flvector 1.0 2.0 3.0 4.0) 3 -1 -2)]) i)
              (list 4.0 2.0))

(check-equal? (sequence->list (in-fxvector (fxvector 1 2 3 4) 0 #f 2)) (list 1 3))
(check-equal? (for/list : (Listof Fixnum) ([i : Fixnum (ann (in-fxvector (fxvector 1 2 3 4) 3 -1 -2) (Sequenceof Fixnum))]) i)
              (list 4 2))
;; I don't understand why this fails...
#;(check-equal? (for/list : (Listof Fixnum) ([i (in-fxvector (fxvector 1 2 3 4) 3 -1 -2)]) i)
              (list 4 2))

(when (extflonum-available?)
  (check-equal? (for/list : (Listof ExtFlonum) ([i (in-extflvector (extflvector 1.0t0 2.0t0 3.0t0 4.0t0) 3 -1 -2)]) i)
                (list 4.0t0 2.0t0)))
