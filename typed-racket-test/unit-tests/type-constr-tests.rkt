#lang racket/base

(require "test-utils.rkt"
         typed-racket/types/subtype
         typed-racket/types/numeric-tower
         typed-racket/types/base-abbrev
         typed-racket/types/abbrev
         typed-racket/typecheck/tc-structs
         typed-racket/types/prop-ops
         typed-racket/rep/type-rep
         typed-racket/rep/values-rep
         typed-racket/env/lexical-env
         typed-racket/env/init-envs
         typed-racket/env/type-env-structs
         typed-racket/env/tvar-env
         typed-racket/env/type-constr-env
         typed-racket/rep/type-rep
         typed-racket/rep/type-constr
         rackunit
         syntax/id-set
         (for-syntax racket/base))

;; (struct hello ([a : Number]))
;; (make-Struct #'hello #f (list (make-fld -Number #'a #f)) (box #f) #f #'hello? (immutable-free-id-set))
(define-syntax-rule (check-print-kind constructor-name expected-str)
  (test-begin (check-equal? (print-kind (lookup-type-constructor constructor-name))
                            expected-str)))
(define tests
  (test-suite
   "suite"
   (test-begin (check-equal? (print-kind (-pair -Number -Number))
                             "*"))
   (test-begin (check-equal? (print-kind (-mpair -Number -Number))
                             "*"))
   (test-begin (check-equal? (print-kind (make-Immutable-Vector -Number))
                             "*"))
   (test-begin (check-equal? (print-kind (make-Mutable-Vector -Number))
                             "*"))
   (test-begin (check-equal? (print-kind (make-Box -Number))
                             "*"))
   (test-begin (check-equal? (print-kind (make-Struct #'hello #f
                                                      (list (make-fld -Number #'a #f))
                                                      (box #f)
                                                      #f
                                                      #'hello?
                                                      (immutable-free-id-set)))
                             "*"))
   (test-begin (check-equal? (print-kind (make-type-constr -pair 2)) "(-> * * *)"))
   (test-begin (check-equal? (print-kind Un) "(-o * ... *)"))
   (test-begin (check-exn exn:fail:contract:arity:type-constructor?
                          (lambda ()
                            ((make-type-constr -pair 2) -Number))))
   (begin
     (register-parsed-struct-sty! (tc/struct (list #'X #'Y) #'foo #'foo (list #'a) (list #'X)))
     (test-begin (check-equal? (print-kind (lookup-type-constructor #'foo))
                               "(-> * * *)")))))

(provide tests)
(gen-test-main)
