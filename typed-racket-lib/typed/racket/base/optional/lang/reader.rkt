#lang s-exp syntax/module-reader

typed/racket/base/optional

#:read r:read
#:read-syntax r:read-syntax
#:info make-info
#:language-info make-language-info

(define (make-info key default use-default)
  (case key
    [(drracket:opt-in-toolbar-buttons)
     '(optimization-coach)]
    [else (use-default key default)]))

(define make-language-info
  `#(typed-racket/language-info get-info (optional)))


(require (prefix-in r: typed-racket/typed-reader))
