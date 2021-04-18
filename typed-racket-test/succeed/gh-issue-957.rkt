#lang typed/racket
(define-type Func (All (A) [-> Func A]))
(: test-func [-> Number Number])
(define test-func
  (λ (num)
    ((λ (func) num)
     (λ ([func : Func])
       (inst func Number)))))

(define-type Func2 (Rec X (All (A) [-> X A])))
(: test-func2 [-> Number Number])
(define test-func2
  (λ (num)
    ((λ (func) num)
     (λ ([func : Func2])
       (inst func Number)))))
