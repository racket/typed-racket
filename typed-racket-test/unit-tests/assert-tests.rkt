#lang racket/base

(require "test-utils.rkt"
         (only-in typed-racket/base-env/extra-procs assert)
         (only-in typed-racket/base-env/prims with-asserts)
         rackunit)

(provide tests)
(gen-test-main)

(define ((assertion-error? who assertion given) e)
  (and (exn:fail:contract? e)
       (regexp-match? (regexp (regexp-quote (format "~a: Assertion ~s failed" who assertion)))
                      (exn-message e))
       (regexp-match? (regexp (regexp-quote (format "given: ~e" given)))
                      (exn-message e))))

(define tests
  (test-suite "Assertion tests"
    (test-case "assert reports the original single-argument assertion"
      (check-exn (assertion-error? 'assert '(assert #f) #f)
                 (lambda () (assert #f))))

    (test-case "assert reports the assertion, predicate name, and value"
      (define pred (procedure-rename string? 'named-predicate))
      (check-exn
       (lambda (e)
         (and ((assertion-error? 'assert '(assert 123 pred) 123) e)
              (regexp-match? #rx"expected: 'named-predicate" (exn-message e))))
       (lambda () (assert 123 pred))))

    (test-case "assert evaluates the value and predicate once, in order"
      (for ([succeeds? '(#t #f)])
        (define events '())
        (define (record! event)
          (set! events (cons event events)))
        (define (run)
          (assert (begin (record! 'value) 123)
                  (begin
                    (record! 'predicate)
                    (lambda (x) (record! 'call) succeeds?))))
        (if succeeds?
            (check-equal? (run) 123)
            (check-exn exn:fail:contract? run))
        (check-equal? (reverse events) '(value predicate call))))

    (test-case "assert preserves truthiness and the identity of its result"
      (define value (box 'value))
      (check-eq? (assert value) value)
      (check-eq? (assert value (lambda (x) 'yes)) value))

    (test-case "not/p remains an ordinary locally bound function"
      (let ([not/p (lambda (p) p)] [x 123])
        (check-equal? (assert x (not/p integer?)) x)
        (check-equal? (with-asserts ([x (not/p integer?)]) x) x)
        (check-exn (assertion-error? 'assert '(assert x (not/p string?)) x)
                   (lambda () (assert x (not/p string?)))))
      (let ([not/p (lambda (p) (lambda (x) (not (p x))))] [x 123])
        (check-equal? (assert x (not/p string?)) x)
        (check-equal? (with-asserts ([x (not/p string?)]) x) x)))

    (test-case "with-asserts identifies a failed truthiness assertion"
      (let ([x #f])
        (check-exn (assertion-error? 'with-asserts '(assert x) #f)
                   (lambda () (with-asserts ([x]) x)))))

    (test-case "with-asserts identifies a failed predicate assertion"
      (let ([x 123])
        (check-exn (assertion-error? 'with-asserts '(assert x string?) x)
                   (lambda () (with-asserts ([x string?]) x)))))

    (test-case "with-asserts evaluates predicate expressions once"
      (let ([x 123] [calls 0])
        (define (predicate)
          (set! calls (add1 calls))
          integer?)
        (check-equal? (with-asserts ([x (predicate)]) x) 123)
        (check-equal? calls 1)))

    (test-case "with-asserts stops at the first failing assertion"
      (let ([x 123] [y "ok"] [events '()])
        (define (record! event)
          (set! events (cons event events)))
        (check-exn
         (assertion-error? 'with-asserts
                           '(assert x (begin (record! 'first) string?)) x)
         (lambda ()
           (with-asserts ([x (begin (record! 'first) string?)]
                          [y (begin (record! 'second) string?)])
             (record! 'body))))
        (check-equal? (reverse events) '(first))))

    (test-case "with-asserts preserves the body and multiple results"
      (let ([x 123] [y "ok"] [events '()])
        (define (record! event)
          (set! events (cons event events)))
        (check-equal?
         (call-with-values
          (lambda ()
            (with-asserts ([x (begin (record! 'first) integer?)]
                           [y (begin (record! 'second) string?)])
              (record! 'body)
              (values x y)))
          list)
         '(123 "ok"))
        (check-equal? (reverse events) '(first second body)))
      (check-equal? (call-with-values
                    (lambda () (with-asserts () (void) (values 1 2))) list)
                    '(1 2)))))
