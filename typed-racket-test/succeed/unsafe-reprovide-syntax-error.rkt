#lang typed/racket

(require typed/racket/unsafe)

(with-handlers ([exn:fail:syntax? (lambda (exn) (void))])
  (eval #'(unsafe-reprovide 4))
  (error 'unsafe-reprovide "expected syntax error"))
