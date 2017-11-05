#lang typed/racket/base #:with-refinements

(define-type Pear (Pair Integer Integer))
(define-type SomeVectorsInAPair (Pair (Vectorof String)
                                      (Vectorof String)))

(define-type Pear1
  (Refine [p : Pear] (= (car p) 5)))

(define-type Pear2
  (Refine [p : Pear] (= (cdr p) 5)))

(define-type Vec
  (Refine [p : SomeVectorsInAPair]
          (= (vector-length (car p))
             (vector-length (cdr p)))))