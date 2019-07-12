#;#;
#<<END
TR info: bounds-check.rkt 18:1 displayln -- hidden parameter
TR info: bounds-check.rkt 20:1 displayln -- hidden parameter
TR info: bounds-check.rkt 22:1 displayln -- hidden parameter
TR info: bounds-check.rkt 39:1 displayln -- hidden parameter
TR info: bounds-check.rkt 41:1 displayln -- hidden parameter
TR info: bounds-check.rkt 43:1 displayln -- hidden parameter
TR opt: bounds-check.rkt 12:2 (vector-ref v i) -- vector partial bounds checking elimination
TR opt: bounds-check.rkt 15:2 (vector-set! v i n) -- vector partial bounds checking elimination
TR opt: bounds-check.rkt 27:2 (flvector-ref v i) -- flvector partial bounds checking elimination
TR opt: bounds-check.rkt 30:2 (flvector-set! v i n) -- flvector partial bounds checking elimination
TR opt: bounds-check.rkt 33:2 (flvector-ref v i) -- flvector partial bounds checking elimination
TR opt: bounds-check.rkt 36:2 (flvector-set! v i n) -- flvector partial bounds checking elimination
TR opt: bounds-check.rkt 6:2 (vector-ref v i) -- vector partial bounds checking elimination
TR opt: bounds-check.rkt 9:2 (vector-set! v i n) -- vector partial bounds checking elimination
END
#<<END
3
4
5
3.0
4.0
5.0

END

#lang typed/racket
#reader typed-racket-test/optimizer/reset-port

(require racket/flonum racket/extflonum)

(: f (All (X) ((Vectorof X) Fixnum -> X)))
(define (f v i)
  (vector-ref v i))
(: g (All (X) ((Vectorof X) Fixnum X -> Void)))
(define (g v i n)
  (vector-set! v i n))
(: h (All (X) ((Vectorof X) Index -> X)))
(define (h v i)
  (vector-ref v i))
(: a (All (X) ((Vectorof X) Index X -> Void)))
(define (a v i n)
  (vector-set! v i n))

(define: v : (Vectorof Integer) (vector 1 2 3 4))
(displayln (f v 2))
(g v 2 4)
(displayln (h v 2))
(a v 2 5)
(displayln (f v 2))


(: ff (FlVector Fixnum -> Float))
(define (ff v i)
  (flvector-ref v i))
(: fg (FlVector Fixnum Float -> Void))
(define (fg v i n)
  (flvector-set! v i n))
(: fh (FlVector Index -> Float))
(define (fh v i)
  (flvector-ref v i))
(: fa (FlVector Index Float -> Void))
(define (fa v i n)
  (flvector-set! v i n))

(define: fv : FlVector (flvector 1.0 2.0 3.0 4.0))
(displayln (ff fv 2))
(fg fv 2 4.0)
(displayln (fh fv 2))
(fa fv 2 5.0)
(displayln (ff fv 2))
