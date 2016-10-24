#lang typed/racket/base
;; https://github.com/racket/typed-racket/issues/447
(struct St ())
(define-type A (∀ (B) (→ (∩ B St) Number)))