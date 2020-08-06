#lang racket/base

;; Global base environment, describes the output behavior of built-ins.
;;
;; In Transient, a function call (f x) usually needs to be guarded with a
;;  codomain shape-check ... so its rewritten to (check t? (f x))
;; Some base functions do not need a codomain check; for example, the call
;;  (list 42) is sure to make a list, so (check list? (list 42)) is overkill.
;; This environment contains all such functions.

(provide
  transient-trusted-positive?
  ;; (-> identifier? boolean?)
  ;; True if outputs from this identifier do not require a shape check

  register-transient-trusted-positive!)

(require
  syntax/id-set)

;; -----------------------------------------------------------------------------

(define the-transient-map (mutable-free-id-set))

(define (transient-trusted-positive? id)
  (free-id-set-member? the-transient-map id))

(define (register-transient-trusted-positive! id)
  (free-id-set-add! the-transient-map id))

