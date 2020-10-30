#lang typed/racket/base
(define-type Type1 (Parameterof Number))
(define-type Type2 (U Type1 Symbol))
(: x Type1)
(define x (make-parameter 123))
(ann x Type2)

(define-type Type11 (U Number (Parameter Type11)))
(define-type Type12 (U Type11 Symbol))
(ann (ann 123 Type11) Type12)

(define-type Type21 (U Number (Parameter Type22)))
(define-type Type22 (U Type21 Symbol))
(ann (ann 123 Type21) Type22)
