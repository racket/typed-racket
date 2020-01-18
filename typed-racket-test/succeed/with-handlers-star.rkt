#lang typed/racket/base
(with-handlers* ([exn:fail? (λ (e) 2)])
  1)
(with-handlers* ([exn:fail? (λ (e) 2)])
  (error "test"))
