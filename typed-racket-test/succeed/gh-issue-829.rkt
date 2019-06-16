#lang typed/racket

;; Test for GH issue 829, this isn't in the class unit tests
;; because of limitations with struct declarations

(struct circle ({x : Integer} {y : Integer}) #:transparent)

(define-type ClassType (Class (init-field {c circle})))

(define c : ClassType
  (class object% (init-field c)
    (match-define (circle #{x : Integer} #{y : Integer}) c)
    (displayln `(,x + ,y = ,(+ x y)))
    (super-new)))
