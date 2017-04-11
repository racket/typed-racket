#;
(exn-pred #rx"expected: pair\\?\n *given: #<int-wrapper>")
#lang racket/base

;; This test checks that TR optimizations are disabled within a
;; sandboxed context

(require racket/sandbox)

(environment-variables-set!	(current-environment-variables)
                            (string->bytes/locale "PLT_TR_NO_OPTIMIZE")
                            #f)

(define (make-sandbox-code-inspector)
  (make-inspector (current-code-inspector)))

(parameterize ([sandbox-memory-limit #f]
               [sandbox-eval-limits #f]
               [sandbox-make-code-inspector make-sandbox-code-inspector])
  (let ([eval (make-evaluator 'racket/base
                              '(module untyped racket/base
                                 (provide extract-field
                                          (struct-out int-wrapper))

                                 (require syntax/location)
                                 (require syntax/modresolve)

                                 (struct int-wrapper (value))

                                 (module typed typed/racket
                                   (provide extract-integer)
                                   (define (extract-integer [p : (Pair Integer Integer)])
                                     (cdr p)))

                                 (require 'typed)

                                 (define extract-field
                                   (eval 'extract-integer (module->namespace (quote-module-path typed)))))
                              '(require (submod "." untyped)))])
    (eval '(extract-field (int-wrapper 42)))))
