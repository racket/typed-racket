#lang typed/racket/shallow

;; Expect no shape-check in domain,
;;  but yes in the body


(define c%
  (class object%
    (super-new)
    (define/private (g (x : (Listof Symbol))) : Symbol
      (car x))))
