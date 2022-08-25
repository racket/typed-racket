#lang racket/base

;; Test tc-app lambda special case for ((lambda ...) ...)

(module a typed/racket/deep
  (define-values [a b]
     ((lambda (x) (values x (list 'A 'A))) 42)))

(module r racket/base
  (provide f)
  (define (f x) (values x '(A A))))

(module b typed/racket/shallow
  (define-values [x0 x1]
     ((lambda (x) (values x (list 'A 'A))) 42))

  (require/typed
    (submod ".." r)
    (f (-> Integer (Values Integer (Listof Symbol)))))

  (define-values [x2 x3]
     ((lambda (x) (f x)) 42))

  (define-values [x4 x5]
     (f 42))
  )

(require 'a 'b)
