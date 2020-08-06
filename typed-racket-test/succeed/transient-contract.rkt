#lang racket/base

;; Test transient contract utils

(module+ test
  (require rackunit
           typed-racket/utils/transient-contract)

  (test-case "procedure-arity-includes-keywords?:no-kws"
    (for ((f (in-list (list (lambda () (void))
                            (lambda args (void))))))
      (check-true (procedure-arity-includes-keywords? f '() '()))
      (check-false (procedure-arity-includes-keywords? f '(#:a) '()))
      (check-false (procedure-arity-includes-keywords? f '() '(#:b)))))

  (test-case "procedure-arity-includes-keywords?:mandatory-only"
    (let ((f (lambda (#:a a #:b b) (void))))
      (check-true (procedure-arity-includes-keywords? f '(#:a #:b) '()))
      (check-false (procedure-arity-includes-keywords? f '(#:b #:a) '()))
      (check-false (procedure-arity-includes-keywords? f '(#:b) '()))
      (check-false (procedure-arity-includes-keywords? f '() '()))
      (check-false (procedure-arity-includes-keywords? f '() '(#:a #:b)))
      (check-false (procedure-arity-includes-keywords? f '(#:a #:b) '(#:c)))))

  (test-case "procedure-arity-includes-keywords?:optional-only"
    (let ((f (lambda (#:a [a #f] #:b [b #f]) (void))))
      (check-true (procedure-arity-includes-keywords? f '() '(#:a #:b)))
      (check-true (procedure-arity-includes-keywords? f '(#:a #:b) '()))
      (check-true (procedure-arity-includes-keywords? f '(#:b #:a) '())) ;; TODO test
      (check-true (procedure-arity-includes-keywords? f '(#:b) '())) ;; TODO test
      (check-true (procedure-arity-includes-keywords? f '() '())) ;; TODO test
      (check-false (procedure-arity-includes-keywords? f '(#:a #:b) '(#:c)))
      (check-false (procedure-arity-includes-keywords? f '(#:c #:b) '()))))

  (test-case "procedure-arity-includes-keywords?:mandatory+optional"
    (let ((f (lambda (#:a a #:b [b #f] #:c [c #f]) (void))))
      (check-true (procedure-arity-includes-keywords? f '(#:a) '()))
      (check-true (procedure-arity-includes-keywords? f '(#:a #:b) '()))
      (check-true (procedure-arity-includes-keywords? f '(#:a #:b #:c) '()))
      (check-true (procedure-arity-includes-keywords? f '(#:a #:c #:b) '()))
      (check-true (procedure-arity-includes-keywords? f '(#:a) '(#:b #:c)))
      (check-true (procedure-arity-includes-keywords? f '(#:a) '(#:c #:b)))

      (check-false (procedure-arity-includes-keywords? f '(#:b #:a) '()))
      (check-false (procedure-arity-includes-keywords? f '() '()))
      (check-false (procedure-arity-includes-keywords? f '(#:a #:x) '()))
      (check-false (procedure-arity-includes-keywords? f '(#:a) '(#:x)))))
)
