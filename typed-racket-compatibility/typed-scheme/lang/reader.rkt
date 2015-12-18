#lang s-exp syntax/module-reader

typed-scheme

#:read r:read
#:read-syntax r:read-syntax
#:info make-info

(require (prefix-in r: typed-racket/typed-reader)
         typed-racket/private/oc-button)

(define (make-info key default use-default)
  (case key
    [(drscheme:toolbar-buttons)
     (maybe-show-OC)]
    [else (use-default key default)]))
