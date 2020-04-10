#lang racket/base

;; Make sure static struct info is available after a require/typed

(module u typed/racket/base
  (struct foo ((a : Symbol)))
  (provide (struct-out foo)))

(module t typed/racket
  (require/typed/provide (submod ".." u)
    (#:struct foo ((a : Symbol)))))

(require 't racket/match)

(match (foo 'xxx)
  [(foo a0) ;; syntax error if `foo` is NOT bound to struct-info at phase 1
   (void)])
