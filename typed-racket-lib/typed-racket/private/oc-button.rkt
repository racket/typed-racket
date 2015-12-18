#lang racket/base

;; Control whether the OC button show up for TR files in DrR.

(provide maybe-show-OC)

(define (maybe-show-OC)
  ;; If Optimization Coach is installed, load it.
  (with-handlers ([exn:fail:filesystem? (lambda _ '())]) ; not found
    (collection-path "optimization-coach")
    (if (dynamic-require 'optimization-coach/tool
                         'optimization-coach-loaded?)
        ;; OC is loaded, show button
        (list (dynamic-require 'optimization-coach/tool
                               'optimization-coach-drracket-button))
        '())))
