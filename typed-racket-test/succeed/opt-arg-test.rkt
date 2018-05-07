#lang typed/racket

(: f (case-> (-> Integer)
             (Integer -> Integer)))
(define (f [#{z : Integer} 0]) z)
#;
(define-values
  (f)
  (let-values (((#{core3 : (case-> (Integer True -> Integer)
                                   (Univ False -> Integer))})
                (lambda (z1 z2) (let-values (((#{z : Integer}) (if z2 z1 '0)))
                                  (let-values () z)))))
    (case-lambda (() (#%app core3 '#f '#f)) 
                 ((z1) (#%app core3 z1 '#t)))))


(add1 (f 0))
(add1 (f))

;; ----------------------------------------

;; Can forget an optional argument:
(: f2 (-> Integer Integer))
(define (f2 x [y 0])
  (+ x y))

;; Same, with non-immediate default:
(: f2/n (-> Integer Integer))
(define (f2/n x [y (+ 1 0)])
  (+ x y))

;; Can forget an optional keyword argument:
(: f3 (-> Integer Integer))
(define (f3 x #:y [y 0])
  (+ x y))

;; Same, with non-immediate default:
(: f3/n (-> Integer Integer))
(define (f3/n x #:y [y (+ 1 0)])
  (+ x y))

;; Mix by-position, by-keyword, and [non-]immediate:
(: f4 (-> Integer Integer))
(define (f4 x [y1 0] [y2 (+ 1 0)] #:z1 [z1 0] #:z2 [z2 (+ 1 0)])
  (+ x y1 y2 z1 z2))
