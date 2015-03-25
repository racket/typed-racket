#lang racket/base

(require "test-utils.rkt"
         rackunit racket/format racket/match
         (typecheck tc-metafunctions tc-subst)
         (rep filter-rep type-rep object-rep)
         (types abbrev union filter-ops tc-result numeric-tower)
         (logic proves)
         (for-syntax racket/base syntax/parse))

(provide tests)
(gen-test-main)

(define-syntax-rule (id-abstract-SLI slis vars expected-slis)
  (test-true (format "~a" '(img tgt)) (let ([actual (abstract-idents vars (apply -and result))]
                                            [expected (apply -and expected-slis)])
                                        (or (filter-equal? actual expected)
                                            (list 'expected: expected 'actual: actual)))))

;; some propositions w/ no logical overlap
;; and no interesting types


(define 
  tests
  (test-suite 
   "Abstraction / Instantiation"
   
   #;(test-suite
      "Mu / Type Variables"
      
      )
   
   (test-suite
    "Id / DeBruijn variables"
    (id-abstract-SLI (list #'x #'y) 
                     (-sli (-leq (-id-lexp (2 x))
                                 (-id-lexp (3 y)))
                           (-leq (-id-lexp (4 y))
                                 (-id-lexp (5 z))))
                     (-sli (-leq (-lexp-obj (list 2 (-id-path (list 0 1))))
                                 (-id-lexp (list 3 (-id-path (list 0 0))) ))
                           (-leq (-id-lexp (list 4 (-id-path (list 0 0))))
                                 (-id-lexp (5 z)))))
    )
   
   
   
   ))
