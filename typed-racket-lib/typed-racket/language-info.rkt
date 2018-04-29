#lang racket/base
(require typed-racket/typed-reader)
(provide get-info configure)

(define ((get-info arg) key default)
  (case key
    [(configure-runtime) `(#(typed-racket/language-info configure ',arg))]
    [else default]))

(define (configure options)
  (namespace-require 'racket/base)
  (define te-mode
    (cond
      [(memq 'deep options) 'deep]
      [(memq 'shallow options) 'shallow]
      [(memq 'optional options) 'optional]
      [else 'deep]))
  (eval `(begin
           (require (for-syntax typed-racket/utils/tc-utils racket/base))
           (begin-for-syntax
             (set-box! type-enforcement-mode ',te-mode)
             (set-box! typed-context? '#t)))
        (current-namespace))
  (current-readtable (readtable)))
