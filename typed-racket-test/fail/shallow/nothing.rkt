#;
(exn-pred exn:fail:contract? #rx"shape-check")

#lang typed/racket/base/shallow

;; type `Nothing` should get a none/c check

(module a racket/base
  (provide f)
  (define (f) 42))

(require/typed 'a
  (f (-> Nothing)))

(f)
