#lang racket

;; Regression test for PR 267
;;   https://github.com/racket/typed-racket/pull/267
;; Issue reported separately as:
;;   https://github.com/racket/racket/issues/1198
;; becuase it was really a racket/contract issue
;; (impersonate-vector contracts were using chaperone-contract)

(define con
  (object/c (field [a* any/c])
   (regenerate (->m (object/c (field [a* (vectorof (object/c))]))))))

(define population%
  (class object%
   (super-new)
   (init-field a*)
   (define/public (regenerate)
    (vector-ref (get-field a* this) 0)
    this)))

(define pop
  (contract con (new population% [a* (vector (new (class object% (super-new))))]) 'p 'n))

(void (send (send pop regenerate) regenerate))

