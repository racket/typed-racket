#lang racket

;; Make sure that the type printer expands only a single
;; level for (:type ...)

(require rackunit
         racket/sandbox)

(define out (open-output-string))

(define tr-eval
  (parameterize ([sandbox-output out])
    (call-with-trusted-sandbox-configuration
     (thunk (make-evaluator 'typed/racket/shallow)))))

(tr-eval '(require typed/racket/shallow))
(tr-eval '(define-type Foo (U String Integer)))
(tr-eval '(define-type Bar (Foo -> Foo)))

(tr-eval '(:type Foo))
(tr-eval '(:type Bar))
(tr-eval '(:type (Number -> Integer)))

;; if #:verbose, make sure it's the full type
(tr-eval '(:type #:verbose Bar))

(check-equal? (get-output-string out)
              (string-append "(U Integer String)\n[can expand further: Integer]"
                             "(-> Foo Foo)\n[can expand further: Foo]"
                             "(-> Number Integer)\n[can expand further: Integer Number]"
                             "(-> (U 0\n"
                             "       1\n"
                             "       Byte-Larger-Than-One\n"
                             "       Negative-Fixnum\n"
                             "       Negative-Integer-Not-Fixnum\n"
                             "       Positive-Fixnum-Not-Index\n"
                             "       Positive-Index-Not-Byte\n"
                             "       Positive-Integer-Not-Fixnum\n"
                             "       String)\n"
                             "    (U 0\n"
                             "       1\n"
                             "       Byte-Larger-Than-One\n"
                             "       Negative-Fixnum\n"
                             "       Negative-Integer-Not-Fixnum\n"
                             "       Positive-Fixnum-Not-Index\n"
                             "       Positive-Index-Not-Byte\n"
                             "       Positive-Integer-Not-Fixnum\n"
                             "       String))\n"))
