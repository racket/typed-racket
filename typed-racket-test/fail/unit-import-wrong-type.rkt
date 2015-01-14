#lang typed/racket/base
(require typed/racket/unit)


(define-signature x^ ([x : String]))

(unit (import x^)
      (export )
      (: y Integer)
      (define y x)
      y)
