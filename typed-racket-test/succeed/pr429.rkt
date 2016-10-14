#lang typed/racket

(: stx-car/c (∀ (Result Pos Neg) (→ (→ Any Result : #:+ Pos #:- Neg)
                                    (→ Any (U #f Result)
                                       : #:+ (Syntaxof (Pairof Pos Any))))))
(define ((stx-car/c nested-c) v)
  (if (syntax? v)
      (if (pair? (syntax-e v))
          (let ([result (nested-c (car (syntax-e v)))])
            (if result
                (begin (ann v (Syntaxof (Pairof Pos Any)))
                       result)
                #f))
          #f)
      #f))