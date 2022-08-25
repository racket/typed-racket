#lang racket/load

(module untyped racket
  (provide call-f)

  (define (call-f f)
    (call-with-continuation-prompt
     (λ () (f 0))
     (default-continuation-prompt-tag)
     (λ (x) (x "string")))))

(module typed typed/racket/optional
  (require/typed 'untyped
                 [call-f ((Integer -> Integer) -> Integer)])

  (call-f
   (λ: ([x : Integer])
     (abort-current-continuation
      (default-continuation-prompt-tag)
      (λ (x) x)))))

(require 'typed)
