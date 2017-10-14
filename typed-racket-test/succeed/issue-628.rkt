#lang racket/base

;; Test that the `Spec` type can be converted to a contract that
;;  successfully **runs**

(module t typed/racket/base
  (provide f g)

  (define-type Spec
    (-> (U Spec String)))

  (: f (-> Spec))
  (define (f)
    (λ () "hello"))

  (: g (-> (Rec T (-> (U T String)))))
  (define (g)
    (λ () "hello")))

(require 't)

(void f g)
