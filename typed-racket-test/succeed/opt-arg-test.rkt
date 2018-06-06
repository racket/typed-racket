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


(define b (box 42))
;; make sure Unsafe-Undefined is added to the core-lambda
;; argument type when a partial type annotation exists
(define (foo [n : Integer (unbox b)])
  (+ n 0))

(unless (equal? 42 (foo))
  (error "oops! a bug!"))

(define (foo2 #:kw [n : Integer (unbox b)])
  (+ n 0))

(unless (equal? 42 (foo2))
  (error "oops! a bug!"))


(require (prefix-in r: racket/base))
;; This expression below either needs to fail to type check
;; (because the λ is from racket/base and thus we fail
;; to add (i.e. union) Unsafe-Undefined to `n`'s type),
;; or it needs to type check but not produce a runtime error.
;; Currently (Racket v 6.90.0.29) it fails to type check
;; (and is therefore wrapped in `assert-typecheck-fail`).
;; If at some point in the future it does type check,
;; then `assert-typecheck-fail` can be removed and
;; the expression should not produce a runtime error.
(assert-typecheck-fail
 (unless (equal? 42 ((r:λ ([#{n : Integer} (unbox b)])
                          (+ n 0))))
   (error "aah!")))

