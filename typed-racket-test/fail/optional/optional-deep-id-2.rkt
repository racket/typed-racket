#;
(exn-pred exn:fail:syntax? #rx"required a flat contract")

#lang typed/racket/base/deep

;; cannot send syntax to deep

(module optional typed/racket/base/optional
  (provide xxx)
  (: xxx (Syntaxof Any))
  (define xxx
    #`#,(vector 0)))

(require 'optional)
xxx

