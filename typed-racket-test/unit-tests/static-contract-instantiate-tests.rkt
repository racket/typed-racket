#lang racket/base

;; Instantiate static contracts to contracts,
;;  check that the contracts accept/reject the right values.

(require "test-utils.rkt" "evaluator.rkt"
         rackunit
         (except-in racket/class private)
         (for-syntax
           syntax/parse
           racket/base
           (static-contracts instantiate optimize combinators)))

(provide tests)
(gen-test-main)

(define-syntax sc->contract
  (syntax-parser
    [(_ sc:expr)
     (syntax/loc #'e
       (phase1-phase0-eval
         (define defs+ctc (instantiate sc (lambda (#:reason _) (error "static-contract could not be converted to a contract"))))
         #`(let () #,@(car defs+ctc) #,(cadr defs+ctc))))]))

(define tests
  (test-suite "Instantiate Tests"
    (let ([nat-ctc (sc->contract (flat/sc #'exact-nonnegative-integer?))])
      (check-true (nat-ctc 4))
      (check-false (nat-ctc -4)))
    (let ([list-0 (sc->contract (list-length/sc 0))])
      (check-true (list-0 '()))
      (check-false (list-0 '#()))
      (check-false (list-0 '(1))))
    (let ([list-1 (sc->contract (list-length/sc 1))])
      (check-true (list-1 '(1)))
      (check-false (list-1 '#()))
      (check-false (list-1 '())))
    (let ([vector-0 (sc->contract (vector-length/sc 0))])
      (check-true (vector-0 '#()))
      (check-false (vector-0 '()))
      (check-false (vector-0 '#(1))))
    (let ([vector-1 (sc->contract (vector-length/sc 1))])
      (check-true (vector-1 '#(1)))
      (check-false (vector-1 '#()))
      (check-false (vector-1 '())))
    (let* ((chk-cls-0 (sc->contract (make-class-shape/sc '((i . t)) '((f . t)) '((p . t)) '((a . t)))))
           (pre-cls (class object% (super-new) (define/pubment (a x) 42)))
           (cls-0 (class pre-cls (super-new) (init i) (field (f 0)) (define/public (p x) (void)) (define/augment (a x) (void)))))
      (check-true (chk-cls-0 cls-0))
      (check-false (chk-cls-0 (class object% (super-new)))))
    (let ((obj-0 (sc->contract (make-object-shape/sc '(f) '(m)))))
      (check-true (obj-0 (new (class object% (super-new) (field (f 0)) (define/public (m x) (void))))))
      (check-false (obj-0 (new (class object% (super-new) (field (f 0)))))))
    ))
