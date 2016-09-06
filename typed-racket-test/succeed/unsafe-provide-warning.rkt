#lang racket/base

;; Compiling a module that calls `unsafe-provide` on an identifier
;;  defined outside the current module (i.e. an imported identifier)
;;  should print a warning to the `current-error-port`

(define p (open-output-string))
(parameterize ([current-error-port p])
  (void (compile
    #'(module test typed/racket/base
        (module a typed/racket/base
          (define -a 1)
          (provide -a))
        (require 'a typed/racket/unsafe)
        (define b 2)
        (unsafe-provide -a b (rename-out [-a c]))))))

(define msg (begin0 (get-output-string p) (close-output-port p)))
(define expected-error "unsafe-provide:[^\n]*-a[^\n]*\n")

(unless (regexp-match (string-append expected-error expected-error) msg)
  (error 'unsafe-provide "Expected warning about re-provided identifier"))
