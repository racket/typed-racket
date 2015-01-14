#lang typed/racket/base

;; ensures updates to a struct's field
;; which results in Bottom
;; properly propogates Bottom up

(define-type ABC (U 'a 'b 'c))

(define-struct ABCWrapper ([abc : ABC]))

(: abc-123-let (ABCWrapper -> (U 1 2 3)))
(define (abc-123-let wrapper)
  (let ([abc (ABCWrapper-abc wrapper)])
    (cond
      [(eq? abc 'a) 1]
      [(eq? abc 'b) 2]
      [(eq? abc 'c) 3])))