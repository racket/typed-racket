#lang typed/racket #:with-refinements

 
(define-type (V8 A) (Refine [v : (Vectorof A)] (= 8 (vector-length v))))
(define-type Nat<8 (Refine [i : Natural] (< i 8)))
(: v8ref (-> (V8 Any) Nat<8 Any))
(define (v8ref v i)
  (vector-ref v i))

(: poly-v8ref (All (A) (-> (V8 A) Nat<8 A)))
(define (poly-v8ref v i)
  (vector-ref v i))


(ann (vector 0 1 2 3 4 5 6 7) (V8 Any))
(ann (vector 0 1 2 3 4 5 6 7) (V8 Byte))
(ann '#(0 1 2 3 4 5 6 7) (V8 Any))
(ann '#(0 1 2 3 4 5 6 7) (Refine (y : (Immutable-Vector Byte Byte Byte Byte Byte Byte Byte Byte)) (= (vector-length y) 8)))
(ann '#(0 1 2 3 4 5 6 7) (V8 Byte))
 
;;(v8ref (vector 0 1 2 3 4 5 6) 4) ;; should fail
(v8ref (vector 0 1 2 3 4 5 6 7) 4)
(poly-v8ref (vector 0 1 2 3 4 5 6 7) 4)
;(v8ref (vector 0 1 2 3 4 5 6 7) 9) ;; should fail
;(v8ref (vector 0 1 2 3 4 5 6 7 8) 4) ;; should fail
;(poly-v8ref (vector 0 1 2 3 4 5 6 7) 9) ;; should fail

(define v0 : (V8 Any) (vector 0 1 2 3 4 5 6 7))
(define v1 : (Vectorof Any) (vector 0 1 2 3 4 5 6 7))


(if (= 8 (vector-length v0))
    (v8ref v0 3)
    (ann "hello" Number))

;; TODO
;; this doesn't work right now because aliasing
;; vector-length makes the type un-updatable (i.e. silly
;; implementation bug that needs a minor refactoring of
;; our object representations)
;; (define (byte->byte [b : Byte]) b)
;;(let ([len (vector-length v0)])
;;    (when (byte? len)
;;      (byte->byte len)))


(when (and (<= 8 (vector-length v1))
           (<= (vector-length v1) 8))
  (v8ref v1 3)) 

(define zero 0)
(define one 1)
(define two 2)
(define three 3)

;; cute syntax that forces an integer equality/inequality/etc
;; else typechecking will fail -- also the type check
;; error highlights the test case
(define-syntax (assert= stx)
  (syntax-case stx ()
    [(_ expr1 expr2)
     #`(unless (= expr1 expr2)
         #,(quasisyntax/loc stx
             (add1 #,(syntax/loc stx "undead = code"))))]))

(define-syntax (assert< stx)
  (syntax-case stx ()
    [(_ expr1 expr2)
     #`(unless (< expr1 expr2)
         #,(quasisyntax/loc stx
             (add1 #,(syntax/loc stx "undead < code"))))]))

(define-syntax (assert<= stx)
  (syntax-case stx ()
    [(_ expr1 expr2)
     #`(unless (<= expr1 expr2)
         #,(quasisyntax/loc stx
             (add1 #,(syntax/loc stx "undead <= code"))))]))

(assert= (- three zero) three)
(assert= (- 3 zero) three)
(assert= (- three 0) three)
(assert= (- three zero) 3)
(assert= (- three two) one)
(assert= (+ one two) three)
(assert= (+ one two) three)
(assert= (- (+ one two) one) two)
(assert= (* 4 (+ one two)) 12)



