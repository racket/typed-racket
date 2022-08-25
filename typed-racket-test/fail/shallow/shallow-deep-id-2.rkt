#;
(exn-pred exn:fail:syntax? #rx"required a flat contract")

#lang typed/racket/base/deep

;; cannot send syntax shallow -> deep,
;; (see succeed/shallow/pass for tests 0 and 1)

(module shallow typed/racket/base/shallow
  (provide xxx)
  (: xxx (Syntaxof Any))
  (define xxx
    #`#,(vector 0)))

(require 'shallow)
xxx

