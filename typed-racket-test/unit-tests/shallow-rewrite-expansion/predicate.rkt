#lang typed/racket/base/shallow

;; List? should expand to a check for `(and/c list? len=2)`
;;
;; search for `List?`, looking for a define-values

(define-type Big-T (List (Listof Integer) (List Symbol Integer Integer)))
(define-predicate List? Big-T)

(: g (-> Any (U #f Big-T)))
(define (g y)
  (if (List? y)
    y
    #f))

