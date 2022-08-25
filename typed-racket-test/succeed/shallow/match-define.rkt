#lang typed/racket/shallow

;; Test that `match-define` works in locally-defensive code

(struct foo ([x : Symbol]))

(: g (-> foo Boolean))
(define (g f)
  (match-define (foo q) f)
  (symbol=? q 'rrr))

