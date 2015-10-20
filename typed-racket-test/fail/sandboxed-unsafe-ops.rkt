#;
(exn-pred #rx"access disallowed by")
#lang racket/base

;; This test checks that TR's unsafe libraries are not accessible
;; from a sandboxed context

(require racket/sandbox)

(parameterize ([sandbox-memory-limit 1000])
  (define eval (make-evaluator 'typed/racket))
  (eval '(require typed/racket/unsafe))

  ;; should fail
  (eval '(unsafe-require/typed racket/base [values 3])))
