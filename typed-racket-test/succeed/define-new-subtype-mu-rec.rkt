#lang typed/racket/base

(define-new-subtype Lst (make-Lst (U Null (Pairof Elt Lst))))
(define-new-subtype Elt (make-Elt (U Symbol Lst)))

(: lst : [Elt * -> Lst])
(define (lst . args)
  (for/fold ([lst (make-Lst '())]) ([elt (in-list (reverse args))])
    (make-Lst (cons elt lst))))

