#;#;
#<<END
TR info: vector-bounds-check.rkt 11:6 (error "make-my-flvector should only be called once!") -- vector of floats
TR info: vector-bounds-check.rkt 15:12 (make-my-vector) -- vector of floats
TR info: vector-bounds-check.rkt 6:6 (error "make-my-vector should only be called once!") -- vector of floats
TR info: vector-bounds-check.rkt 8:7 (vector 1.0 2.0 3.0) -- vector of floats
TR opt: vector-bounds-check.rkt 15:0 (vector-ref (make-my-vector) 0) -- vector partial bounds checking elimination
TR opt: vector-bounds-check.rkt 16:0 (flvector-ref (make-my-flvector) (ann 0 Fixnum)) -- flvector partial bounds checking elimination
END
#<<END
1.0
1.0

END
#lang typed/racket
(require racket/flonum)
#reader typed-racket-test/optimizer/reset-port

(define: been-there-vector?   : Boolean #f)
(define: been-there-flvector? : Boolean #f)
(define (make-my-vector)
  (if been-there-vector?
      (error "make-my-vector should only be called once!")
      (set! been-there-vector? #t))
  (ann (vector 1.0 2.0 3.0) (Vectorof Flonum)))
(define (make-my-flvector)
  (if been-there-flvector?
      (error "make-my-flvector should only be called once!")
      (set! been-there-flvector? #t))
  (flvector 1.0 2.0 3.0))

(vector-ref (make-my-vector) 0)
(flvector-ref (make-my-flvector) (ann 0 Fixnum))
