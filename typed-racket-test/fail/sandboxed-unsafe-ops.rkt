#;
(exn-pred #rx"access disallowed by")
#lang racket/base

;; This test checks that TR's unsafe libraries are not accessible
;; from a sandboxed context

(require racket/sandbox)

;; this doesn't need a memory limit
(parameterize ([sandbox-memory-limit #f]
               [sandbox-eval-limits #f])
  (define eval (make-evaluator 'typed/racket))
  (eval '(require typed/racket/unsafe))

  ;; should fail
  (eval '(unsafe-require/typed racket/base [values 3])))
