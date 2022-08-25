#lang typed/racket/base/shallow

;; Originally raised error in optimizer,
;;  expected kernel-literal (or something) got gensym var

(require racket/match)

(define-type result Symbol)
(define-type zo Symbol)
(define zo? symbol?)

(define-type Context (U zo (Listof zo) (Listof result)))
(define-type History (Listof Context))
(define-type History* (Listof History))

(: find (-> String Context History History* (Values Context History History*)))
(define (find raw ctx hist pre-hist)
  (define arg "hello")
  (cond [(and arg (zo? ctx))
         (define results : (Listof result) '())
         (match results
           ['()
            (values ctx hist pre-hist)]
           [_
            (values ctx '() pre-hist)])]
        [else
         (values ctx hist pre-hist)]))

