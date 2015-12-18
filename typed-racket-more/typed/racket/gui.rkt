#lang racket/base

;; Defines a language analogous to #lang racket/gui but typed

(require "gui/base.rkt"
         typed/racket)

(provide (all-from-out "gui/base.rkt")
         (all-from-out typed/racket))

;; language definition
(module reader syntax/module-reader
  #:read read
  #:read-syntax read-syntax
  #:language 'typed/racket/gui
  #:language-info #(typed-racket/language-info get-info ())
  #:info make-info

  (require typed-racket/private/oc-button)

  ;; see typed/racket/lang/reader.rkt
  (define (make-info key default use-default)
    (case key
      [(drscheme:toolbar-buttons)
       (maybe-show-OC)]
      [else (use-default key default)]))

  (require typed-racket/typed-reader))
